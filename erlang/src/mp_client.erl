%%
%% MessagePack for Erlang
%%
%% Copyright (C) 2010 UENISHI Kota
%%
%%    Licensed under the Apache License, Version 2.0 (the "License");
%%    you may not use this file except in compliance with the License.
%%    You may obtain a copy of the License at
%%
%%        http://www.apache.org/licenses/LICENSE-2.0
%%
%%    Unless required by applicable law or agreed to in writing, software
%%    distributed under the License is distributed on an "AS IS" BASIS,
%%    WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%%    See the License for the specific language governing permissions and
%%    limitations under the License.

%%%-------------------------------------------------------------------
%%% @author UENISHI Kota <kuenishi@gmail.com>
%%% @copyright (C) 2010, UENISHI Kota
%%% @doc
%%   mp_client is a client interface to access messagepack server.
%%   in erlang way, this is just a OTP worker. You can set your
%%   code into your OTP supervision tree like a gen_server.
%%
%%   current status
%%     only TCP works, now rewriting as to work also UDP works.
%%
%%  <code>
%%  sample()->
%%  %just as a syntax sugar for start_link
%%  %YourModule defines receiver-callback when notification came from server.
%%   {ok, Pid}=mp_client:connect(Identifier, YourModule, [Address, Port], [tcp]),
%%   mp_client:call(Identifier, somemethod, [1,2]), % returns {ok, 3}
%%   mp_client:call_async(Identifier, somemethod, [1,2]),
%%   receive
%%       {ok, Answer} -> ok;% maybe 3
%%       _ -> error
%%   after 1024 -> timeout end
%%   mp_client:close(Pid).
%%  </code>
%%% @end
%%% Created : 26 Aug 2010 by UENISHI Kota <kuenishi@gmail.com>
%%%-------------------------------------------------------------------
-module(mp_client).

-behaviour(gen_server).
-include("mp_rpc.hrl").

-define(SERVER, ?MODULE).

%% external API
-export([connect/2, connect/3, close/0, close/1,
	 call/3, call/4,
	 call_async/3, call_async/4, cancel_async_call/1, cancel_async_call/2,
	 watch/2, watch/3]).

%% internal: gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-type address() :: string()|atom()|inet:ip_address().

%%====================================================================
%% API
%%====================================================================
-spec connect(Address::address(), Port::(0..65535))-> {ok, pid()}.
connect(Address, Port)->
    gen_server:start_link({local,?SERVER}, ?MODULE, [{address,Address},{port,Port}], []).
% for debug			  [{debug,[trace,log,statistics]}]).

% users can set any identifier to the connection
-spec connect(Identifier::server_name(),  Address::address(), Port::(0..65535)) ->  {ok, pid()}.
connect(Identifier, Address, Port)->
    gen_server:start_link(Identifier, ?MODULE, [{address,Address},{port,Port}],
			  []).

% synchronous calls
% when method 'Method' doesn't exist in server implementation,
% it returns {error, {<<"no such method">>, nil}}
% user func error => {error, {<<"unexpected error">>, nil}}
-spec call(CallID::non_neg_integer(), Method::atom(), Argv::list()) -> 
    {ok, any()} | {error, {atom(), any()}}.
call(CallID, Method, Argv) ->   call(?SERVER, CallID, Method, Argv).

-spec call(Client::server_ref(), CallID::non_neg_integer(), 
	   Method::atom(), Argv::list()) -> 
		  {ok, any()} | {error, {atom(), any()}}.
call(Client, CallID, Method, Argv) when is_atom(Method), is_list(Argv) ->
    ok=call_async(Client,CallID,Method,Argv),
    receive
	{ok, nil,Result }->     {ok, Result};
	{ok, ResCode,Result }-> {error,{ResCode, Result}};
	{error, Reason2}->       {error, Reason2};
	_Other ->               {error, {unknown, _Other}}
    end.

% TODO: write test code for call_async/3
% afterwards, the caller will receive the response {ok, ResCode, Result} as a message
-spec call_async(CallID::non_neg_integer(),
		 Method::atom(), Argv::list()) -> ok | {error, {atom(), any()}}.
call_async(CallID, Method, Argv)->
    call_async(?SERVER, CallID, Method, Argv).

-spec call_async(Client::server_ref(), CallID::non_neg_integer(),
		 Method::atom(), Argv::list()) -> ok | {error, {atom(), any()}}.
call_async(Client, CallID, Method, Argv) when is_atom(Method), is_list(Argv)->
    Meth = <<(atom_to_binary(Method,latin1))/binary>>,
    Pid = self(),
    case msgpack:pack([?MP_TYPE_REQUEST,CallID,Meth,Argv]) of
	{error, Reason}->
	    {error, Reason};
	Pack ->
	    ok=gen_server:call(Client, {call, {CallID, Pid} ,Pack})
    end.

% TODO: write test code for cancellation
-spec cancel_async_call(CallID::non_neg_integer())->ok.
cancel_async_call(CallID)-> cancel_async_call(?SERVER, CallID).

-spec cancel_async_call(Client::server_ref(), CallID::non_neg_integer())->ok.
cancel_async_call(Client, CallID)->
    gen_server:call(Client, {cancel, CallID}).

% set a callback for notification from server.
-spec watch(Method::atom(), Callback::fun( (atom(),list()) -> any() )) -> ok | {error, any()}.
watch(Method, Callback) ->   watch(?SERVER, Method, Callback).

-spec watch(Client::server_ref(), Method::atom(),
	    Callback::fun( (atom(),list()) -> any() )) -> ok | {error, any()}.
watch(Client, Method, Callback) when is_atom(Method), is_function(Callback,2)->
    gen_server:call(Client, {watch, Method, Callback}).

% users can set any identifier to the connection
-spec close(Identifier::server_name())-> any().
close(Identifier)->  gen_server:call(Identifier, stop).

-spec close() -> any().		    
close()-> close(?SERVER).


%%====================================================================
%% gen_server callbacks
%%====================================================================
-record(state, {socket :: inet:sockt(),
		addr,
		port :: 1..65536,
		reqs = [] :: [{integer(), pid()}],
		events = [] :: [{atom(), fun()}]
	       }).

%% @private
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%%--------------------------------------------------------------------
init(Config)->
    Address = proplists:get_value(address, Config, localhost),
    Port = proplists:get_value(port, Config, 65500),
    {ok, S}=gen_tcp:connect(Address, Port, [binary, {active,once},{packet,raw}]),
    {ok, #state{socket=S, addr=Address, port=Port}}.

%% @private
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call({call, {Id, Pid}, Pack}, _From, State)->
    Reqs = [{Id, Pid}|State#state.reqs],
    ok = gen_tcp:send(State#state.socket, Pack),
    ok = inet:setopts(State#state.socket, [{active,once}]),
    {reply, ok, State#state{reqs=Reqs}};

handle_call({cancel, Id}, _From, State)->
    List=proplists:delete(Id, State#state.reqs),
    {repoly, ok, List};

handle_call({watch, Method, Callback}, _From, State)->
    List = [{Method,Callback}|State#state.events],
    {repoly, ok, List};

handle_call(stop, _From, State)->
    {stop, normal, ok, State};
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%% @private
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%% @private
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info({tcp, _Socket, Pack}, State)->
    case msgpack:unpack(Pack) of
	{error, _Reason} ->
	    ok = inet:setopts(State#state.socket, [{active,once}]),
	    {noreply, State};
	{[?MP_TYPE_NOTIFICATION,Method,Params],_RemBin}->
	    Meth = binary_to_atom(Method, latin1),
	    case proplists:get_value(Meth, State#state.events) of
		undefined->	    ok;
		Callback when is_function(Callback,2)->
		    try Callback(Params) catch _:E -> io:format("~p~n", [E]) end;
		_Other ->
		    io:format("error ~s~p: ~p~n", [?FILE, ?LINE, _Other])
	    end,
	    ok = inet:setopts(State#state.socket, [{active,once}]),
	    {noreply, State};
%     get method from State -> callback them
	{[?MP_TYPE_RESPONSE,CallID,ResCode,Result],_RemBin} ->
	    case proplists:get_value(CallID, State#state.reqs) of
		undefined->
		    ok = inet:setopts(State#state.socket, [{active,once}]),
		    {noreply, State};
		Pid when is_pid(Pid)->
		    Pid ! {ok, ResCode, Result},
		    ok = inet:setopts(State#state.socket, [{active,once}]),
		    {noreply, State#state{reqs=proplists:delete(CallID, State#state.reqs)}};
		_Other ->		% Error
		    io:format("error ~s~p: ~p~n", [?FILE, ?LINE, _Other]),
		    ok = inet:setopts(State#state.socket, [{active,once}]),
		    {noreply, State#state{reqs=proplists:delete(CallID, State#state.reqs)}}
	    end
    end;

handle_info(_Info, State) ->
    {noreply, State}.

%% @private
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, State) ->
    gen_tcp:close(State#state.socket),
    ok.

%% @private
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

my_first_case(_Config) ->
    {ok, _Pid}=mp_client:connect(localhost,65500),
    {ok, Result}=mp_client:call(42, hello, []),
    true=is_list(Result),
    ok=mp_client:close().

my_second_case(_)->
    {ok, _}=mp_client:connect({local, hoge}, localhost,65500),
    {ok, Result}=mp_client:call(hoge,42, hello, []),
    true=is_list(Result),
    {ok, 7}=mp_client:call(hoge,43, add, [3,4]),
    ok=mp_client:close(hoge).

case_add(Config)->
    Pairs=[{5,5}, {0,0}, {234, 2}, {213456789, -3}, {234, -23}, {-1,1}, {1,-1}, {-1,-1},
	  {-2000, 2000}, {2000, -2000}, {234, -234}],
    {ok, _Pid}=mp_client:connect({local,add}, localhost,65500),
    {ok, _Result}=mp_client:call(add, 42, hello, []),
    lists:map( fun({L,R})-> S=L+R, {ok,S}=mp_client:call(add, (L+42), add, [L,R])  end, Pairs ),
    {error, {<<"no such func">>,nil}}=mp_client:call(add, 890, no_such_func, []),
    mp_client:close(add).

my_test()->
    ok=sample_app:start(),
    ok=my_first_case(nil),
    ok=my_second_case(nil),
    ok=case_add(nil),
    ok=sample_app:stop().
    
%%     {ok,Pid}=mp_client:connect(localhost,65500),
%%     {ok,_Reply}=mp_client:call(Pid, hoge, []),
%%     mp_client:close().

-endif.
