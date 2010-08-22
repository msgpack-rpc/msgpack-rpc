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
%%% File    : mp_server.erl
%%% Author  : UENISHI Kota <kuenishi@gmail.com>
%%% Description : MessagePack server
%%%
%%% Created : 30 May 2010 by UENISHI Kota <kuenishi@gmail.com>
%%%-------------------------------------------------------------------
-module(msgpack_rpc).

-export([start/0, stop/0, add_server/2, del_server/1]).

start()->
    application:start(?MODULE).

stop()->
    application:stop(?MODULE).

add_server(Mod, Options)->
    mp_server_listener_sup:add_server(Mod, Options).

del_server(Name)->
    mp_server_listener_sup:del_server(Name).
    
