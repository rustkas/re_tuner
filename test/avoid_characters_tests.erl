-module(avoid_characters_tests).

%%
%% Tests
%%
-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

compile_01_test() ->
    Expected = {error, {"nothing to repeat", 0}},
    Regex = "*",
    Result = re:compile(Regex),
    ?assertEqual(Expected, Result).

compile_02_test() ->
    Expected = {error, {"nothing to repeat", 0}},
    Regex = "+",
    Result = re:compile(Regex),
    ?assertEqual(Expected, Result).

compile_03_test() ->
    Expected = {error, {"nothing to repeat", 0}},
    Regex = "?",
    Result = re:compile(Regex),
    ?assertEqual(Expected, Result).

compile_04_test() ->
    Expected = {error, {"missing )", 1}},
    Regex = "(",
    Result = re:compile(Regex),
    ?assertEqual(Expected, Result).

compile_05_test() ->
    Expected = {error, {"unmatched parentheses", 0}},
    Regex = ")",
    Result = re:compile(Regex),
    ?assertEqual(Expected, Result).

compile_06_test() ->
    Expected = {error, {"missing terminating ] for character class", 1}},
    Regex = "[",
    Result = re:compile(Regex),
    ?assertEqual(Expected, Result).

save_compile_01_test() ->
    Expected = true,
    AvoidCharacters = "*.^$|{}]\-",
    Result =
        lists:all(fun(Elem) ->
                     Regex = "\\Q" ++ [Elem] ++ "\\E",
                     {ok, _} = re:compile(Regex),
                     true
                  end,
                  AvoidCharacters),

    ?assertEqual(Expected, Result).

-endif.
