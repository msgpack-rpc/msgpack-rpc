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
-module(mp_server).

-export([start/1]).

start(StartProps)->
    Mod = proplists:get_value(module, StartProps, undefined),
    Addr = proplists:get_value(addr, StartProps, localhost),
    Port = proplists:get_value(port, StartProps, 65500),
    mp_server_sup:start_link(Mod,Addr,Port).
