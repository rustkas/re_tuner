-module(first_match_tests).

%%
%% Tests
%%
-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

research_01_01_test()->
	Expected = "13",
	Text = "Do you like 13 or 42?",
	Regex = "\\d+",
	Result = re_tuner:first_match(Text, Regex),
	?assertEqual(Expected, Result).

research_01_02_test()->
	Expected = "13",
	Text = "Do you like 13 or 42?",
	Regex = "\\d+",
	MP = re_tuner:mp(Regex),
	Result = re_tuner:first_match(Text, MP),
	?assertEqual(Expected, Result).

research_01_03_test()->
	Expected = nomatch,
	Text = "Do you like 13 or 42?",
	Regex = "\\d{3,}",
	MP = re_tuner:mp(Regex),
	Result = re_tuner:first_match(Text, MP),
	?assertEqual(Expected, Result).

-endif.
