%%%-------------------------------------------------------------------
%%% File    : sample_client.erl
%%% Author  : UENISHI Kota <kuenishi@gmail.com>
%%% Description : 
%%%
%%% Created :  5 Jun 2010 by UENISHI Kota <kuenishi@gmail.com>
%%%-------------------------------------------------------------------
-module(sample_client).
-compile(export_all).

loop(_,  0) -> ok;
loop(Fun,N) ->
    Fun(N),
    loop(Fun,N-1).

load(P)->
    ok=sample_app:start(),
    {ok, _Pid}=mp_client:connect(localhost,65500),
    ReqFun = fun(N)-> {ok, _}=mp_client:call(N, hello, []) end,
    loop(ReqFun, P),
    ok=mp_client:close(),
    ok=sample_app:stop().
