-module(subfilter_tests).

%%
%% Tests
%%
-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

research_test_() ->
    {"Find a Match Within Another Match",
     {foreach,
      local,
      fun build_text_and_expectation/0,
      [fun research_01_01_01/1,
       fun research_01_02_01/1, 
	   fun research_01_03_01/1
	   ]}}.


build_text_and_expectation()->
	Expected = ["2", "5", "6", "7"],
        Text="
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
		
{Text, Expected, OuterRegex, InnerRegex}.

research_01_01_01({Text, Expected, OuterRegex, InnerRegex})->
	Result = re_tuner:subfilter(Text, OuterRegex, InnerRegex),
	?_assertEqual(Expected, Result).

research_01_02_01({Text, Expected, OuterRegex, InnerRegex})->
	OuterMP = re_tuner:mp(OuterRegex),
	InnerMP = re_tuner:mp(InnerRegex),
	Result = re_tuner:subfilter(Text, OuterMP, InnerMP),
	?_assertEqual(Expected, Result).

research_01_03_01({_Text, _Expected, OuterRegex, InnerRegex})->
	Expected = nomatch,
	Text = "",
	OuterMP = re_tuner:mp(OuterRegex),
	InnerMP = re_tuner:mp(InnerRegex),
	Result = re_tuner:subfilter(Text, OuterMP, InnerMP),
	?_assertEqual(Expected, Result).

-endif.