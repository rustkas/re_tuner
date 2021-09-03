-module(replace3_tests).

%%
%% Tests
%%
-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

build_text_and_expectation()->
    Text = "before",
    Expected = "after",
	Regex = Text,
    {Text, Expected,Regex}.


research_test_() ->
    {"Replace All Matches",
     {foreach,
      local,
      fun build_text_and_expectation/0,
      [
	   fun research_01_01_01/1,
	   fun research_01_02_01/1,
	   fun research_01_03_01/1]}}.

research_01_01_01({Text, Expected, Regex})->
	Result = re_tuner:replace(Text,Regex,Expected),
	?_assertEqual(Expected, Result).

research_01_02_01({Text, Expected, Regex})->
	MP = re_tuner:mp(Regex),
	Result = re_tuner:replace(Text,MP,Expected),
	?_assertEqual(Expected, Result).

research_01_03_01({_Text, _Expected, Regex})->
	Expected = "",
	Text = Expected,
	MP = re_tuner:mp(Regex),
	Result = re_tuner:replace(Text,MP,Expected),
	?_assertEqual(Expected, Result).	

-endif.
