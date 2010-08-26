#!/usr/bin/env escript

-define(NAME, "msgpack_rpc").
-define(VSN,  "0.0.1").

% http://gist.github.com/304254 - thanks

%% Recursively copy directories
-spec recursive_copy(list(), list()) -> ok.                            
recursive_copy(From, To) ->
    {ok, Files} = file:list_dir(From),
    file:make_dir(To),
    [ok = rec_copy(From, To, X) || X <- Files],
    ok.
 
-spec rec_copy(list(), list(), list()) -> ok.
rec_copy(_, _, "test")->
    ok;
rec_copy(_From, _To, [$. | _T]) -> %% Ignore Hidden
    ok; 
rec_copy(From, To, File) ->
 
    NewFrom = filename:join(From, File),
    NewTo   = filename:join(To, File),
 
    case filelib:is_dir(NewFrom) of
 
        true  ->
	    file:make_dir(NewTo),
            ok = filelib:ensure_dir(NewTo),
            recursive_copy(NewFrom, NewTo);
        
        false ->
            case filelib:is_file(NewFrom) of                
                true  ->
                    ok = filelib:ensure_dir(NewTo),
                    {ok, _} = file:copy(NewFrom, NewTo),
                    ok;
                false ->
                    ok            
            end
    end.

main(["install"|_])->
    Dest = filename:join(code:lib_dir(), io_lib:format("~s-~s", [?NAME, ?VSN])),
    recursive_copy(".", Dest),
    io:format("uninstall: ./scripts/setup.es uninstall");

main(["uninstall"|_])->
    Dest = filename:join(code:lib_dir(), io_lib:format("~s-~s", [?NAME, ?VSN])),
    os:cmd("rm -rf "++Dest);

main(["build"|_])->
    os:cmd("make");

main([]) ->
    main(["build"]);

main([O|_])->
    io:format("usage: ./scripts/setup.es install"),
    error.
