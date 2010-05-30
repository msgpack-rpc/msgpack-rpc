%%%-------------------------------------------------------------------
%%% File    : mp_client.erl
%%% Author  : UENISHI Kota <kuenishi@gmail.com>
%%% Description : MessagePack client
%%%
%%% Created : 30 May 2010 by UENISHI Kota <kuenishi@gmail.com>
%%%-------------------------------------------------------------------
-module(mp_client).

-behaviour(gen_server).

-define(SERVER, ?MODULE).

%% external API
-export([start_link/0, connect/2]).

% -> {ok, Pid}
connect(Address, Port)->
    gen_server:start_link({local,?SERVER}, ?MODULE, [{address,Address},{port,Port}], []).

% synchronous -> {ok, result()}
call(Method, Argv)->
    gen_server:call(?SERVER, {Method,Argv}).

close()->
    gen_server:call(?SERVER, stop).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).



-record(state, {socket, addr, port}).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

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
init(Config)->
    Address = proplists:get_value(Config, address, localhost),
    Port = proplists:get_value(Config, port, 65500),
    {ok, S}=gen_tcp:connect(Address, Port, [binary, {active,false},{packet, raw}]),
    {ok, #state{socket=S, addr=Address, port=Port}};

init([]) ->
    {ok, #state{}}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call({Method, Argv}, _From, State)->
    Reply = gen_tcp:send(State#state.socket, term_to_binary({Method, Argv})),
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
