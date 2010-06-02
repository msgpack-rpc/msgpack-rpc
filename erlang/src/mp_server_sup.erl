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
-export([start_link/1]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the supervisor
%%--------------------------------------------------------------------
start_link(Argv) when is_list(Argv)->
    supervisor:start_link({local, ?SERVER}, ?MODULE, Argv).

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

    Children = [{mp_server_sup2,{ mp_server_sup2,start_link,[]},
		 permanent,2000,supervisor,[mp_server_sup2]},
		{mp_server_srv,{mp_server_srv,start_link,[{addr,Addr},{port,Port}]},
		 permanent,2000,worker,[mp_server_srv]}],

    ok=supervisor:check_childspecs(Children),
    {ok,{{one_for_all,0,1}, Children}}.

%%====================================================================
%% Internal functions
%%====================================================================

