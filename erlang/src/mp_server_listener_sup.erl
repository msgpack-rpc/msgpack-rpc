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
%%% File    : mp_server_sup2.erl
%%% Author  : UENISHI Kota <kuenishi@gmail.com>
%%% Description : 
%%% @private
%%% Created : 30 May 2010 by UENISHI Kota <kuenishi@gmail.com>
%%%-------------------------------------------------------------------
-module(mp_server_listener_sup).

-behaviour(supervisor).

%% API
-export([start_link/0, add_server/2, del_server/1]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

-type option() :: tcp % uses mp_tcp_listener and mp_tcp_transport
		| udp % uses mp_udp_listener and mp_udp_transport
%		| sctp | ssl | etc.. .
		| {debug, any()}.

%%====================================================================
%% API functions
%%====================================================================
% Mod is session handler
-spec add_server(Mod::atom(), Options::[option()])-> ok | {error,any()}.
add_server(Mod, Options)->
    supervisor:start_child(?SERVER, [Mod,Options]).

del_server(Name)->
    supervisor:terminate_child(?SERVER, Name).

%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the supervisor
%%--------------------------------------------------------------------
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

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
init([]) ->
    AChild = {mp_server_srv,{mp_server_srv,start_link,[]},
	      permanent,2000,worker,[mp_server_srv]},
    ok=supervisor:check_childspecs([AChild]),
    {ok,{{simple_one_for_one,0,1}, [AChild]}}.

%%====================================================================
%% Internal functions
%%====================================================================
