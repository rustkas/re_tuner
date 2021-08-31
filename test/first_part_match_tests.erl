-module(first_part_match_tests).

%%
%% Tests
%%
-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

research_01_01_test()->
	Expected = "www.regexcookbook.com",
	Text = "Please visit http://www.regexcookbook.com for more information",
	Regex = "http://([a-z0-9.-]+)",
	Result = re_tuner:first_part_match(Text, Regex),
	?assertEqual(Expected, Result).

research_01_02_test()->
	Expected = "www.regexcookbook.com",
	Text = "Please visit http://www.regexcookbook.com for more information",
	Regex = "http://([a-z0-9.-]+)",
	MP = re_tuner:mp(Regex),
	Result = re_tuner:first_part_match(Text, MP),
	?assertEqual(Expected, Result).

research_01_03_test()->
	Expected = nomatch,
	Text = "Please visit http://www.regexcookbook.com for more information",
	Regex = "https://([a-z0-9.-]+)",
	MP = re_tuner:mp(Regex),
	Result = re_tuner:first_part_match(Text, MP),
	?assertEqual(Expected, Result).

-endif.
