-module(filter_tests).

%%
%% Tests
%%
-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

research_01_01_01_test()->
	Expected = ["13", "65"],
	Text = "The lucky numbers are 7, 13, 16, 42, 65, and 99",
	Regex = "\\d+",
	Function = fun(Elem)->
		Integer = list_to_integer(Elem),
		Result = (0 =:= (Integer rem 13)),
		Result
	end,
	Result = re_tuner:filter(Text, Regex, Function),
	?assertEqual(Expected, Result).

research_01_02_01_test()->
	Expected = ["13", "65"],
	Text = "The lucky numbers are 7, 13, 16, 42, 65, and 99",
	Regex = "\\d+",
	MP = re_tuner:mp(Regex),
	Function = fun(Elem)->
		Integer = list_to_integer(Elem),
		Result = (0 =:= (Integer rem 13)),
		Result
	end,
	Result = re_tuner:filter(Text, MP, Function),
	?assertEqual(Expected, Result).

research_01_03_01_test()->
	Expected = nomatch,
	Text = "The lucky numbers are",
	Regex = "\\d+",
	MP = re_tuner:mp(Regex),
	Function = fun(Elem)->
		Integer = list_to_integer(Elem),
		Result = (0 =:= (Integer rem 13)),
		Result
	end,
	Result = re_tuner:filter(Text, MP, Function),
	?assertEqual(Expected, Result).

-endif.