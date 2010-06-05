%%%-------------------------------------------------------------------
%%% File    : mp_session.erl
%%% Author  : UENISHI Kota <kuenishi@gmail.com>
%%% Description : 
%%%
%%% Created :  3 Jun 2010 by UENISHI Kota <kuenishi@gmail.com>
%%%-------------------------------------------------------------------
-module(mp_session).

-behaviour(gen_server).
-include("mp_rpc.hrl").


%% API
-export([start_link/2, behaviour_info/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-spec behaviour_info(atom()) -> 'undefined' | [{atom(), arity()}].
behaviour_info(callbacks) ->
    [{init,1}, {handle_call,3}, {terminate,2}, {code_change,3}];
behaviour_info(_Other) ->
    undefined.

-record(state, {socket, module, context}).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: [Module]++[Socket] will come. 
%%   see mp_server_sup:start_client for caller.
%%--------------------------------------------------------------------
start_link(Module,Socket) when is_atom(Module), is_port(Socket)->
    gen_server:start_link(?MODULE, [Module,Socket], [{debug,[trace,log,statistics]}]).

%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%%--------------------------------------------------------------------
init([]) ->
    io:format("~p~p: ~p~n", [?FILE, ?LINE, ?MODULE]),
    {stop, {error,badarg}};
init([Module,Socket]) when is_atom(Module), is_port(Socket)->
    %[active, nodelay, keepalive, delay_send, priority, tos]) of
    ok=inet:setopts(Socket, [{active,once},{packet,raw}]),
    io:format("~p~p: ~p~n", [?FILE, ?LINE, self()]),
    case Module:init(Socket) of
	{ok, Context}->
	    {ok, #state{module=Module, socket=Socket, context=Context}};
	Error ->
	    {stop, Error}
    end.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call(Request, From, #state{context=Context,module=Module}=State) ->
    case Module:handle_call(Request, From, Context) of
	{reply, Reply, NextContext}->
	    {reply, Reply, State#state{context=NextContext}};
	{noreply, NextContext}->
	    {noreply, State#state{context=NextContext}};
	{stop, Reason, Reply, NextContext} ->
	    {stop, Reason, Reply, State#state{context=NextContext}}
    end.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast(Msg, #state{context=Context,module=Module}=State) ->
    case Module:handle_cast(Msg,Context) of
	{noreply, NextContext}->
	    {noreply, State#state{context=NextContext}};
	{stop,Reason,NextContext}->
	    {stop,Reason,State#state{context=NextContext}}
    end.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info({tcp, Socket, Bin}, #state{socket=Socket,module=Module,context=Context} = State) ->
    io:format("~p~p: ~p~n", [?FILE, ?LINE, msgpack:unpack(Bin)]),
    try
	{[Req,CallID,M,Argv],<<>>}=msgpack:unpack(Bin),
	case handle_request(Req,CallID,Module,M,Argv,Socket,Context) of
	    {ok, NextContext}->
	    % Flow control: enable forwarding of next TCP message
		ok=inet:setopts(Socket, [{active,once},{packet,raw}]),

		{noreply, State#state{context=NextContext}};
	    {stop, Reason}->
		{stop, Reason};
	    _Other->
		error_logger:error_msg("failed unpack: ~p  result: ~p~n", [Bin, _Other])
	end
    catch _:What -> 
	    error_logger:error_msg("failed: ~p~n", [What])
    end;

handle_info({tcp_closed, Socket}, #state{socket=Socket} = State) ->
    error_logger:info_msg("~p Client ~p disconnected.\n", [self(), hoge]),
    {stop, normal, State};

handle_info(_Info, State) ->
    error_logger:info_msg("~p: unknown message: ~p .\n", [self(), _Info]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(Reason, #state{module=Module, context=Context, socket=Socket}) ->
    Module:terminate(Reason,Context),
    ok=gen_tcp:close(Socket),
    ok.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(OldVsn, #state{module=Module, context=Context} = State, Extra) ->
    {ok, NextState}=Module:code_change(OldVsn, Context, Extra),
    {ok, State#state{context=NextState}}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

handle_request(?MP_TYPE_REQUEST, CallID, Module, M, Argv,Socket, Context) when is_integer(CallID), is_binary(M) ->
    Method = binary_to_atom(M, latin1),
    Prefix = [?MP_TYPE_RESPONSE, CallID],
    case erlang:apply(Module,Method,Argv) of
	{reply, Result}->
	    ReplyBin = msgpack:pack(Prefix++[nil, Result]),
	    ok=gen_tcp:send(Socket,ReplyBin),
	    {ok, Context};

	{noreply, _Result}->
	    {ok, Context};

	{stop_reply, Result, Reason}->
	    ReplyBin = msgpack:pack(Prefix++[ nil, Result]),
	    ok=gen_tcp:send(Socket,ReplyBin),
	    {stop, Reason};

	{stop, Reason}->
	    {stop, Reason};

	{error, Reason}->
	    ReplyBin = msgpack:pack(Prefix++[Reason, nil]),
	    ok=gen_tcp:send(Socket,ReplyBin),
	    {ok,Context}

    end.
