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

-module(mp_client).

-behaviour(gen_server).
-include_lib("eunit/include/eunit.hrl").
-include("mp_rpc.hrl").

-define(SERVER, ?MODULE).

%% external API
-export([connect/2, call/3, call/4, close/0]).

%% internal: gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-type address() :: string()|atom()|inet:ip_address().

%%====================================================================
%% API
%%====================================================================
% Starts the server
-spec connect(Address::address(), Port::(0..65535))-> {ok, pid()}.
connect(Address, Port)->
    gen_server:start_link({local,?SERVER}, ?MODULE, [{address,Address},{port,Port}], []).

% synchronous calls
-spec call(CallID::non_neg_integer(), Method::atom(), Argv::list()) -> 
    {ok, any()} | {error, {atom(), any()}}.

call(CallID, Method, Argv) ->
    call(?SERVER, CallID, Method, Argv).

-spec call(Client::(atom()|pid()), CallID::non_neg_integer(), Method::atom(), Argv::list()) -> 
    {ok, any()} | {error, {atom(), any()}}.

call(Client, CallID, Method, Argv) when is_atom(Method), is_list(Argv) ->
    Meth = <<(atom_to_binary(Method,latin1))/binary>>,
    Pack = msgpack:pack([?MP_TYPE_REQUEST,CallID,Meth,Argv]),
    case gen_server:call(Client, {call,Pack}) of
	{ok, ResPack}->
	    case msgpack:unpack(ResPack) of
		{error, Reason} -> {error, {unpack_fail, Reason}};
		{Reply, <<>>} ->
		    case Reply of
			[?MP_TYPE_RESPONSE,CallID,nil,Result]->
			    {ok, Result};
			[?MP_TYPE_RESPONSE,CallID,ResCode,Result] ->
			    {error,{ResCode, Result}};
			_Other ->
			    {error, {unknown, _Other}}
		    end
	    end;
	{error, Reason}-> {error, Reason}
    end.

close()->
    gen_server:call(?SERVER, stop).


%%====================================================================
%% gen_server callbacks
%%====================================================================
-record(state, {socket, addr, port}).
%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%%--------------------------------------------------------------------
init(Config)->
    Address = proplists:get_value(address, Config, localhost),
    Port = proplists:get_value(port, Config, 65500),
    {ok, S}=gen_tcp:connect(Address, Port, [binary, {active,false},{packet,raw}]),
    {ok, #state{socket=S, addr=Address, port=Port}}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call({call, Pack}, _From, State)->
    ok = gen_tcp:send(State#state.socket, Pack),
    Reply = gen_tcp:recv(State#state.socket,0),
    {reply, Reply, State};
handle_call(stop, _From, State)->
    {stop, normal, ok, State};
handle_call(_oRequest, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
%handle_info({tcp, Socket, Pack}, State)->
    
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, State) ->
    gen_tcp:close(State#state.port),
    ok.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
-ifdef(EUNIT).

my_test()->
    {ok,Pid}=mp_client:connect(localhost,65500),
    {ok,Reply}=mp_client:call(Pid, hoge, []),
    elrang:display(Reply),
    mp_client:close().

-endif.
