%%%-------------------------------------------------------------------
%%% File    : mp_server.erl
%%% Author  : UENISHI Kota <kuenishi@gmail.com>
%%% Description : MessagePack server
%%%
%%% Created : 30 May 2010 by UENISHI Kota <kuenishi@gmail.com>
%%%-------------------------------------------------------------------
-module(mp_server).

-export([start/0, stop/0]).

start()->
    application:start(?MODULE).

stop()->
    application:stop(?MODULE).
