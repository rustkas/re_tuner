-module(submatch_evaluator2_tests).

%%
%% Tests
%%
-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").


submatch_evaluator(Text, OuterRegex, InnerRegex, Replacement)  when is_list(OuterRegex), is_list(InnerRegex) ->
	Result = try
	    MP = re_tuner:mp(OuterRegex,[no_start_optimize]),
		InnerMP = re_tuner:mp(InnerRegex,[]),
	    submatch_evaluator(Text, MP, InnerMP, Replacement)
	of
	    TryResult -> TryResult	
    catch
	    error:_ -> Text
	end,	
	Result;
	
submatch_evaluator(Text, MP, InnerMP, InnerReplacement) when is_tuple(MP), is_tuple(InnerMP)-> 
	DoAction = fun(DoActionText, DoActionMP, DoActionReplacement) ->
	    Result = re:replace(DoActionText, DoActionMP, DoActionReplacement, [global,{return, list}]),
		%?debugFmt("Replace Result = ~p",[Result]),
		Result
	end,
	
	ReplaceFun = fun(FullString, MatchResult) ->
	  %?debugFmt("MatchResult = ~p",[MatchResult]),
	  Index = element(1,MatchResult),
	  Length = element(2,MatchResult),
	  Offset = Index + Length,
	  SubString = string:slice(FullString, Index, Length),
	  NewSubString = DoAction(SubString,InnerMP,InnerReplacement),
	  NewOffset = Index + string:length(NewSubString),
	  FullStringLength = string:length(FullString),
	  NewString = string:slice(FullString, 0, Index) ++ NewSubString 
	  ++ string:slice(FullString, Offset, FullStringLength - Offset ),
	  {NewString, NewOffset}
	end,

	MatchEvaluationFun = fun MatchEvaluator(FullString, EvaluationMP, Offset) ->
	    RunResult = re:run(FullString, EvaluationMP,[{capture,first,index},{offset, Offset}]),
		%?debugFmt("RunResult = ~p",[RunResult]),
		RunResultCheck = case RunResult of
	        nomatch -> FullString;
	        {match,[MatchResult]} -> ReplaceFun(FullString,MatchResult)
	    end,
		MatchEvaluationResult = case is_tuple(RunResultCheck) of
		    true -> 
			     {NewString, NewOffset} = RunResultCheck,
				 MatchEvaluator(NewString, EvaluationMP, NewOffset);
			_ -> RunResultCheck
        end,
	    MatchEvaluationResult
    end,
	%?debugFmt("Text = ~p",[Text]),
	StartOffset = 0,
	Result = MatchEvaluationFun(Text,MP,StartOffset),
	Result.

build_text()->
	
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
    before <b>first before</b> before <b>before before</b>
	</div>
</body>
</html>
	",
	SanitizedText = sanitize_text(Text),
	
    OuterRegex = "(?<=<b>).*?(?=</b>)",
	InnerRegex = "before",
	InnerReplacement = "after",
		
    {SanitizedText, OuterRegex, InnerRegex, InnerReplacement}.

sanitize_text(Text) when is_list(Text)->
    SanitizedText = string:replace(Text, [$\r,$\n], [$\n], all),
    SanitizedText.
	
research_test_() ->
    {"Replace All Matches Within the Matches of Another Regex",
     {foreach,
      local,
      fun build_text/0,
      [fun research_01/1,
	   fun research_02/1,
	   fun research_03/1,
	   fun research_04_01/1,
	   fun research_04_02/1,
	   fun research_04_03/1
	   ]}}.

research_01({Text, OuterRegex, InnerRegex, InnerReplacement})->
    Expected = lists:duplicate(3, InnerReplacement),
	SubmatchEvaluatorResult = submatch_evaluator(Text, OuterRegex, InnerRegex, InnerReplacement),
	Result = re_tuner:subfilter(SubmatchEvaluatorResult, OuterRegex, InnerReplacement),
	?_assertEqual(Expected, Result).

research_02({Text, OuterRegex, InnerRegex, InnerReplacement})->
    Expected = lists:duplicate(3, InnerReplacement),
	OuterMP = re_tuner:mp(OuterRegex),
	InnerMP = re_tuner:mp(InnerRegex),
	
	SubmatchEvaluatorResult = submatch_evaluator(Text, OuterMP, InnerMP, InnerReplacement),
	Result = re_tuner:subfilter(SubmatchEvaluatorResult, OuterRegex, InnerReplacement),
	?_assertEqual(Expected, Result).
	
research_03({_Text, OuterRegex, InnerRegex, InnerReplacement})->
    
	Expected = nomatch,
	Text = "",
	OuterMP = re_tuner:mp(OuterRegex),
	InnerMP = re_tuner:mp(InnerRegex),
	
	SubmatchEvaluatorResult = submatch_evaluator(Text, OuterMP, InnerMP, InnerReplacement),
	
	%?debugFmt("SubmatchEvaluatorResult = ~s", [SubmatchEvaluatorResult]),
	Result = re_tuner:subfilter(SubmatchEvaluatorResult, OuterRegex, InnerReplacement),
	?_assertEqual(Expected, Result).

research_04_01({Text, OuterRegex, InnerRegex, InnerReplacement})->
    Expected = lists:duplicate(3, InnerReplacement),
	SubmatchEvaluatorResult = re_tuner:submatch_evaluator(Text, OuterRegex, InnerRegex, InnerReplacement),
	Result = re_tuner:subfilter(SubmatchEvaluatorResult, OuterRegex, InnerReplacement),
	?_assertEqual(Expected, Result).
	
research_04_02({Text, OuterRegex, InnerRegex, InnerReplacement})->
    Expected = lists:duplicate(3, InnerReplacement),
	OuterMP = re_tuner:mp(OuterRegex),
	InnerMP = re_tuner:mp(InnerRegex),
	
	SubmatchEvaluatorResult = re_tuner:submatch_evaluator(Text, OuterMP, InnerMP, InnerReplacement),
	Result = re_tuner:subfilter(SubmatchEvaluatorResult, OuterRegex, InnerReplacement),
	?_assertEqual(Expected, Result).

research_04_03({_Text, OuterRegex, InnerRegex, InnerReplacement})->
    
	Expected = nomatch,
	Text = "",
	OuterMP = re_tuner:mp(OuterRegex),
	InnerMP = re_tuner:mp(InnerRegex),
	
	SubmatchEvaluatorResult = re_tuner:submatch_evaluator(Text, OuterMP, InnerMP, InnerReplacement),
	
	%?debugFmt("SubmatchEvaluatorResult = ~s", [SubmatchEvaluatorResult]),
	Result = re_tuner:subfilter(SubmatchEvaluatorResult, OuterRegex, InnerReplacement),
	?_assertEqual(Expected, Result).


-endif.

