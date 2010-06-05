%%%-------------------------------------------------------------------
%%% File    : sample_srv.erl
%%% Author  : UENISHI Kota <kuenishi@gmail.com>
%%% Description : 
%%%
%%% Created :  5 Jun 2010 by UENISHI Kota <kuenishi@gmail.com>
%%%-------------------------------------------------------------------
-module(sample_srv).
-author('kuenishi@gmail.com').

-behaviour(mp_session).

%% rpc methods
-export([hello/0]).

%% API
-export([init/1, handle_call/3, terminate/2, code_change/3]).

-record(state, {}).
%%====================================================================
%% API
%%====================================================================
init(_Argv)->
    {ok, #state{}}.

hello()->
    {reply, "hello, msgpack!"}.

handle_call(_Request, _From, State)->
    Reply=ok,
    {reply, Reply, State}.

terminate(_Reason, State)->
    {ok, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal functions
%%====================================================================

