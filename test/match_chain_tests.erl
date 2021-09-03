-module(match_chain_tests).

%%
%% Tests
%%
-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

research_test_() ->
    {"Get the matches of one regex within the matches of another regex",
     {foreach,
      local,
      fun build_text_and_expectation/0,
      [fun research_01_01_01/1,
       fun research_01_02_01/1, 
	   fun research_01_03_01/1
	   ]}}.


build_text_and_expectation()->
	Expected = ["2", "5", "6", "7"],
        Text= "
<!DOCTYPE html>
<html>
<head>
    <meta charset=\"UTF-8\">
    <meta http-equiv=\"X-UA-Compatible\" content=\"IE=edge\">
    <meta name=\"viewport\" content=\"width=device-width, initial-scale=1.0\">
    <title>Document</title>
</head>
<body>
    <div>
    <b>2</b> 3 4 <b>5 6 7</b> <b>a</b>
	</div>
</body>
</html>
	",
	
    OuterRegex = "<b>(.*?)</b>",
	InnerRegex = "\\d+",
		
{Text, Expected, [OuterRegex, InnerRegex]}.

research_01_01_01({Text, Expected, ListRegex})->
	Result = re_tuner:match_chain(Text, ListRegex),
	?_assertEqual(Expected, Result).

research_01_02_01({Text, Expected, ListRegex})->
    List_MP = lists:map(fun(Regex)-> 
	  re_tuner:mp(Regex)
	end, ListRegex),
	Result = re_tuner:match_chain(Text, List_MP),
	?_assertEqual(Expected, Result).

research_01_03_01({_Text, _Expected, ListRegex})->
	Expected = nomatch,
	Text = "",
	Result = re_tuner:match_chain(Text, ListRegex),
	?_assertEqual(Expected, Result).

-endif.