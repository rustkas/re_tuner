-module(all_match_tests).

%%
%% Tests
%%
-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

research_01_01_test()->
	Expected = [ "7", "13", "16", "42", "65", "99"],
	Text = "The lucky numbers are 7, 13, 16, 42, 65, and 99",
	Regex = "\\d+",
	Result = re_tuner:all_match(Text, Regex),
	?assertEqual(Expected, Result).

research_01_02_test()->
	Expected = [ "7", "13", "16", "42", "65", "99"],
	Text = "The lucky numbers are 7, 13, 16, 42, 65, and 99",
	Regex = "\\d+",
	MP = re_tuner:mp(Regex),
	Result = re_tuner:all_match(Text, MP),
	?assertEqual(Expected, Result).

research_01_03_test()->
	Expected = nomatch,
	Text = "The lucky numbers are",
	Regex = "\\d+",
	MP = re_tuner:mp(Regex),
	Result = re_tuner:all_match(Text, MP),
	?assertEqual(Expected, Result).

-endif.