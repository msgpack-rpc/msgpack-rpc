%%%-------------------------------------------------------------------
%%% File    : sample_app.erl
%%% Author  : UENISHI Kota <kuenishi@gmail.com>
%%% Description : 
%%% @hidden
%%% Created : 30 May 2010 by UENISHI Kota <kuenishi@gmail.com>
%%%-------------------------------------------------------------------
-module(sample_app).

%% Application callbacks
-export([start/0, stop/0]).

start()->
    msgpack_rpc:start(),
    case msgpack_rpc:add_server(sample_srv, [{addr, localhost}]) of
	{ok, _} -> ok;
	Other -> Other
    end.

stop()->
    msgpack_rpc:del_server(sample_srv),
    msgpack_rpc:stop().
