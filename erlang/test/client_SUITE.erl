%%%-------------------------------------------------------------------
%%% File    : sample_SUITE.erl
%%% Author  : UENISHI Kota <kuenishi@gmail.com>
%%% Description : test suite that RPCs to Ruby server
%%%
%%% Created :  27 Jun 2010 by UENISHI Kota <kuenishi@gmail.com>
%%%-------------------------------------------------------------------
-module(client_SUITE).
-compile(export_all).

-include("ct.hrl").

%%--------------------------------------------------------------------
%% Function: suite() -> Info
%% Info = [tuple()]
%%--------------------------------------------------------------------
suite() ->
    [{timetrap,{seconds,30}}].

%%--------------------------------------------------------------------
%% Function: init_per_suite(Config0) ->
%%               Config1 | {skip,Reason} | {skip_and_save,Reason,Config1}
%% Config0 = Config1 = [tuple()]
%% Reason = term()
%%--------------------------------------------------------------------
init_per_suite(Config) ->
    Config.
    
%%--------------------------------------------------------------------
%% Function: end_per_suite(Config0) -> void() | {save_config,Config1}
%% Config0 = Config1 = [tuple()]
%%--------------------------------------------------------------------
end_per_suite(_Config) ->
    ok.

%%--------------------------------------------------------------------
%% Function: all() -> GroupsAndTestCases | {skip,Reason}
%% GroupsAndTestCases = [{group,GroupName} | TestCase]
%% GroupName = atom()
%% TestCase = atom()
%% Reason = term()
%%--------------------------------------------------------------------
all() -> 
    [my_first_case, my_second_case, case_add].

%%--------------------------------------------------------------------
%% Function: TestCase(Config0) ->
%%               ok | exit() | {skip,Reason} | {comment,Comment} |
%%               {save_config,Config1} | {skip_and_save,Reason,Config1}
%% Config0 = Config1 = [tuple()]
%% Reason = term()
%% Comment = term()
%%--------------------------------------------------------------------
my_first_case(_Config) ->
    Port=bgn(),
    {ok, _Pid}=mp_client:connect(localhost,65501),
    {ok, <<"ok">>}=mp_client:call(42, hello, []),
    {ok, 435}=mp_client:call(42, sum, [1,434]),
    ok=mp_client:close(),
    ed(Port).

my_second_case(_)->
    Port=bgn(),
    {ok, _}=mp_client:connect({local, hoge}, localhost,65501),
    {ok, <<"ok">>}=mp_client:call(hoge,42, hello, []),
    {ok, 7}=mp_client:call(hoge,43, sum, [3,4]),
    ok=mp_client:close(hoge),
    ed(Port).

case_add(Config)->
    Port=bgn(),
    Pairs=[{5,5}, {0,0}, {234, 2}, {213456789, -3}, {234, -23}, {-1,1}, {1,-1}, {-1,-1},
	  {-2000, 2000}, {2000, -2000}, {234, -234}],
    {ok, _Pid}=mp_client:connect({local,add}, localhost,65501),
    {ok, <<"ok">>}=mp_client:call(add, 42, hello, []),
    lists:map( fun({L,R})-> S=L+R, {ok,S}=mp_client:call(add, (L+42), sum, [L,R])  end, Pairs ),
    {error, {_,nil}}=mp_client:call(add, 890, no_such_func, []),
    mp_client:close(add),
    ed(Port),
    Config.

bgn()->
    Port = open_port({spawn, "ruby -rubygems ../../../priv/test_server.rb"}, []),
    receive
	{Port, {data, Data}}-> 
	    ct:log("~p", [Data]);
	_O -> 
	    ct:log("~p", [_O])
    after 1024->
  	    ct:log("orz", [])
    end,
    Port.

ed(Port)->
    port_close(Port).
