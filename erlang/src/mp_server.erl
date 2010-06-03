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
