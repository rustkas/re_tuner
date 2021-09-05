-module(mp_tests).

%%
%% Tests
%%
-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

-import(re_tuner, [mp/1, mp/2]).

get_mp_01_test() ->
    Result = mp("[0-9]"),
    ?assert(is_tuple(Result)),
    {re_pattern, _, _, _, _} = Result.

get_mp_02_test() ->
    ?assertException(error, badarg, mp("[9-0]")).

get_mp_03_test() ->
    Result = mp("[a-z]", [caseless]),
    {ok, MP} = re:compile("[a-z]", [caseless]),
    ?assertEqual(MP, Result).

research_01_01_test()->
    Expected = badarg,
    Result = try
	   error(badarg)
    catch
       error:Error -> Error
    end, 
    %?debugFmt("Result = ~p", [Result]).
    ?assertEqual(Expected, Result).

research_01_02_test() ->
    Expected = badarg,
    MainResult = try
	   error(badarg)
	   of 
	   Result -> Result
    catch
       error:Error -> Error
    end, 
    %?debugFmt("Result = ~p", [Result]).
    ?assertEqual(Expected, MainResult).

research_01_03_test() ->
    Expected = {error,{"missing )",4}},
    Regex = "\\d+(",
	Result = re:compile(Regex),
	%?debugFmt("Result = ~p", [Result]).
	?assertEqual(Expected, Result).

research_01_04_01_test() ->
    Expected = ok,
	Regex = "\\d+(",
	MainResult = try
	    re_tuner:mp(Regex)
	of 
	   Result -> Result
	catch
	    error:Error -> ok
	end,
	?assertEqual(Expected, MainResult).
	
research_01_04_02_test() ->
	Regex = "\\d+",
	Result = try
	    re_tuner:mp(Regex)
	of 
	   TryResult -> TryResult
	catch
	    error:Error -> ok
	end,
	?assert(is_tuple(Result)).



-endif.
