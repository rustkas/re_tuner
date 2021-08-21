-module(unicode_block_tests).

%%
%% Tests
%%
-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

research_01_test() ->
    Expected = "[\\x0-\\x7F]",
    Range = re_tuner:unicode_block("\\p{InBasicLatin}"),
    Result = Range,
    ?assertEqual(Expected, Result).

research_02_test() ->
    Expected = nomatch,
    Range = re_tuner:unicode_block("\\p{Latin}"),
    Result = Range,
    ?assertEqual(Expected, Result).

-endif.
