-module(is_full_match_tests).

%%
%% Tests
%%
-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").
 
research_01_test()->
	Expected = true,
	Text = "hello world",
	Regex = "hello world",
	Result = re_tuner:is_full_match(Text, Regex),
	?assertEqual(Expected, Result).

research_02_test()->
	Expected = false,
	Text = "hello world",
	Regex = "hello",
	Result = re_tuner:is_full_match(Text, Regex),
	?assertEqual(Expected, Result).

research_03_test()->
   Expected = false,
   Regex = "(",
   Result = re_tuner:is_full_match("(", Regex),
   ?assertEqual(Expected, Result).

-endif.
