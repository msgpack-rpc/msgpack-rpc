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
%%% File    : mp_server_sup.erl
%%% Author  : UENISHI Kota <kuenishi@gmail.com>
%%% Description : 
%%% @private
%%% Created : 30 May 2010 by UENISHI Kota <kuenishi@gmail.com>
%%%-------------------------------------------------------------------
-module(mp_server_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

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
%% start_link(Module,Addr,Port)->
%%     supervisor:start_link({local, ?SERVER}, ?MODULE, 
%% 			 [{module,Module},{addr,Addr},{port,Port}]).
start_link()->
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
init(_StartProps) ->
    Children = [{mp_server_session_sup,
		 { mp_server_session_sup,start_link,[]},
		 permanent,2000,supervisor,[]},
                {mp_server_listener_sup,
		 {mp_server_listener_sup,start_link,[]},
		 permanent,2000,supervisor,[]}],
    ok=supervisor:check_childspecs(Children),
%    io:format("~p~p: ~p~n", [?FILE, ?LINE, Children]),
    {ok,{{one_for_all,0,1}, Children}}.

%%====================================================================
%% Internal functions
%%====================================================================

