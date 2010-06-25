%%%-------------------------------------------------------------------
%%% File    : sample_SUITE.erl
%%% Author  : UENISHI Kota <kuenishi@gmail.com>
%%% Description : test suite for sample application.
%%%
%%% Created :  7 Jun 2010 by UENISHI Kota <kuenishi@gmail.com>
%%%-------------------------------------------------------------------
-module(sample_SUITE).
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
    sample_app:start(),
    Config.
    
%%--------------------------------------------------------------------
%% Function: end_per_suite(Config0) -> void() | {save_config,Config1}
%% Config0 = Config1 = [tuple()]
%%--------------------------------------------------------------------
end_per_suite(_Config) ->
    sample_app:stop().

%%--------------------------------------------------------------------
%% Function: init_per_group(GroupName, Config0) ->
%%               Config1 | {skip,Reason} | {skip_and_save,Reason,Config1}
%% GroupName = atom()
%% Config0 = Config1 = [tuple()]
%% Reason = term()
%%--------------------------------------------------------------------
init_per_group(_GroupName, Config) ->
    Config.

%%--------------------------------------------------------------------
%% Function: end_per_group(GroupName, Config0) ->
%%               void() | {save_config,Config1}
%% GroupName = atom()
%% Config0 = Config1 = [tuple()]
%%--------------------------------------------------------------------
end_per_group(_GroupName, _Config) ->
    ok.

%%--------------------------------------------------------------------
%% Function: init_per_testcase(TestCase, Config0) ->
%%               Config1 | {skip,Reason} | {skip_and_save,Reason,Config1}
%% TestCase = atom()
%% Config0 = Config1 = [tuple()]
%% Reason = term()
%%--------------------------------------------------------------------
init_per_testcase(_TestCase, Config) ->
    Config.

%%--------------------------------------------------------------------
%% Function: end_per_testcase(TestCase, Config0) ->
%%               void() | {save_config,Config1}
%% TestCase = atom()
%% Config0 = Config1 = [tuple()]
%%--------------------------------------------------------------------
end_per_testcase(_TestCase, _Config) ->
    ok.

%%--------------------------------------------------------------------
%% Function: groups() -> [Group]
%% Group = {GroupName,Properties,GroupsAndTestCases}
%% GroupName = atom()
%% Properties = [parallel | sequence | Shuffle | {RepeatType,N}]
%% GroupsAndTestCases = [Group | {group,GroupName} | TestCase]
%% TestCase = atom()
%% Shuffle = shuffle | {shuffle,{integer(),integer(),integer()}}
%% RepeatType = repeat | repeat_until_all_ok | repeat_until_all_fail |
%%              repeat_until_any_ok | repeat_until_any_fail
%% N = integer() | forever
%%--------------------------------------------------------------------
groups() ->
    [{add, [parallel], [case_add]}].

%%--------------------------------------------------------------------
%% Function: all() -> GroupsAndTestCases | {skip,Reason}
%% GroupsAndTestCases = [{group,GroupName} | TestCase]
%% GroupName = atom()
%% TestCase = atom()
%% Reason = term()
%%--------------------------------------------------------------------
all() -> 
    [msgpack_test, my_first_case, my_second_case, {group, add}].

%%--------------------------------------------------------------------
%% Function: TestCase(Config0) ->
%%               ok | exit() | {skip,Reason} | {comment,Comment} |
%%               {save_config,Config1} | {skip_and_save,Reason,Config1}
%% Config0 = Config1 = [tuple()]
%% Reason = term()
%% Comment = term()
%%--------------------------------------------------------------------
msgpack_test(_)->
    msgpack:test().


my_first_case(_Config) ->
    {ok, _Pid}=mp_client:connect(localhost,65500),
    {ok, Result}=mp_client:call(42, hello, []),
    true=is_list(Result),
    ok=mp_client:close().

my_second_case(_)->
    {ok, _}=mp_client:connect({local, hoge}, localhost,65500),
    {ok, Result}=mp_client:call(hoge,42, hello, []),
    true=is_list(Result),
    {ok, 7}=mp_client:call(hoge,43, add, [3,4]),
    ok=mp_client:close(hoge).

case_add(Config)->
    Pairs=[{5,5}, {0,0}, {234, 2}, {213456789, -3}, {234, -23}, {-1,1}, {1,-1}, {-1,-1},
	  {-2000, 2000}, {2000, -2000}, {234, -234}],
    {ok, _Pid}=mp_client:connect({local,add}, localhost,65500),
    {ok, _Result}=mp_client:call(add, 42, hello, []),
    lists:map( fun({L,R})-> S=L+R, {ok,S}=mp_client:call(add, (L+42), add, [L,R])  end, Pairs ),
    mp_client:close(add),
    Config.
