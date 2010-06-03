%%%-------------------------------------------------------------------
%%% File    : mp_server_sup.erl
%%% Author  : UENISHI Kota <kuenishi@gmail.com>
%%% Description : 
%%%
%%% Created : 30 May 2010 by UENISHI Kota <kuenishi@gmail.com>
%%%-------------------------------------------------------------------
-module(mp_server_sup).

-behaviour(supervisor).

%% API
-export([start_link/3, start_client/1]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================
%% A startup function for spawning new client connection handling FSM.
%% To be called by the TCP listener process.
start_client(Socket) ->
    supervisor:start_child(mp_server_sup2, [Socket]).

%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the supervisor
%%--------------------------------------------------------------------
start_link(Module,Addr,Port)->
    supervisor:start_link({local, ?SERVER}, ?MODULE, 
			 [{module,Module},{addr,Addr},{port,Port}]).

%%====================================================================
%% Supervisor callbacks
%%====================================================================
%%--------------------------------------------------------------------
%% Func: init(Args) -> {ok,  {SupFlags,  [ChildSpec]}} |
%%                     ignore                          |
%%                     {error, Reason}
%% Description: Whenever a supervisor is started using 
%% supervisor:start_link/[2,3], this function is called by the new process 
%% to find out about restart strategy, maximum restart frequency and child 
%% specifications.
%%--------------------------------------------------------------------
init(StartProps) when is_list(StartProps)->
    Addr = proplists:get_value(addr, StartProps, localhost),
    Port = proplists:get_value(port, StartProps, 65500),
    Mod = proplists:get_value(module, StartProps, undefined),

    Children = [{mp_server_sup2,{ mp_server_sup2,start_link,[Mod]},
		 permanent,2000,supervisor,[mp_server_sup2]},
		{mp_server_srv,{mp_server_srv,start_link,[{addr,Addr},{port,Port}]},
		 permanent,2000,worker,[mp_server_srv]}],

    ok=supervisor:check_childspecs(Children),
    {ok,{{one_for_all,0,1}, Children}}.

%%====================================================================
%% Internal functions
%%====================================================================

