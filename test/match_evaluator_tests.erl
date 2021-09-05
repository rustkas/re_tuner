-module(match_evaluator_tests).

%%
%% Tests
%%
-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

-import(re_tuner, [mp/1, mp/2]).

build_input_and_expectation()->
    Expected = "20 40 60",
	Text = "1 2 3",
    Regex = "\\d+",

    DoAction = fun(SubString)->
	  Integer = list_to_integer(SubString),
	  Value = Integer * 20,
	  NewSubString = integer_to_list(Value),
	  NewSubString
	end,
    {Text, Expected, Regex, DoAction}.


research_test_() ->
    {"Replace Matches with Replacements Generated in Code",
     {foreach,
      local,
      fun build_input_and_expectation/0,
      [
	   fun research_01_01_01/1,
	   fun research_01_02_01/1,
	   fun research_01_03_01/1]}}.

research_01_01_01({Text, Expected, Regex, DoAction})->
	Result = re_tuner:match_evaluator(DoAction, Text, Regex),
	?_assertEqual(Expected, Result).

research_01_02_01({Text, Expected, Regex, DoAction})->
	MP = re_tuner:mp(Regex),
	Result = re_tuner:match_evaluator(DoAction, Text, MP),
	?_assertEqual(Expected, Result).

research_01_03_01({_Text, _Expected, Regex, DoAction})->
	Expected = "",
	Text = Expected,
	MP = re_tuner:mp(Regex),
	Result = re_tuner:match_evaluator(DoAction, Text, MP),
	?_assertEqual(Expected, Result).	



-endif.
