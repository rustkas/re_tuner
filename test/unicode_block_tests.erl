-module(unicode_block_tests).

%%
%% Tests
%%
-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

research_01_test() ->
    Expected = "[\\x0-\\x7F]",
    Range = re_tuner:unicode_block("\\p{InBasicLatin}"),
    Result = Range,
    ?assertEqual(Expected, Result).

research_01_01_test() ->
    Expected = nomatch,
    Range = re_tuner:unicode_block("\\p{Latin}"),
    Result = Range,
    ?assertEqual(Expected, Result).
	
research_02_01_test() ->
    Expected = true,
    List = lists:seq(0, 16#7F),
    Regex = re_tuner:unicode_block("\\p{InBasicLatin}"),
    {ok, MP} = re:compile(Regex, [unicode]),
    Result =
        lists:all(fun(Elem) ->
                     Text = [Elem],
                     match == re:run(Text, MP, [{capture, none}])
                  end,
                  List),
    ?assertEqual(Expected, Result).
	
research_02_02_test() ->
    Expected = true,
    List = lists:seq(16#80, 16#FF),
    Regex = re_tuner:unicode_block("\\p{InLatin-1Supplement}"),
	%?debugFmt("Regex = ~p~n",[Regex]),
    {ok, MP} = re:compile(Regex, [unicode]),
    Result =
        lists:all(fun(Elem) ->
                     Text = [Elem],
                     match == re:run(Text, MP, [{capture, none}])
                  end,
                  List),
    ?assertEqual(Expected, Result).	
	
research_02_03_test() ->
    Expected = true,
    List = lists:seq(16#100, 16#17F),
    Regex = re_tuner:unicode_block("\\p{InLatinExtended-A}"),
	%?debugFmt("Regex = ~p~n",[Regex]),
    {ok, MP} = re:compile(Regex, [unicode]),
    Result =
        lists:all(fun(Elem) ->
                     Text = [Elem],
                     match == re:run(Text, MP, [{capture, none}])
                  end,
                  List),
    ?assertEqual(Expected, Result).		

research_02_04_test() ->
    Expected = true,
    List = lists:seq(16#180, 16#24F),
    Regex = re_tuner:unicode_block("\\p{InLatinExtended-B}"),
	%?debugFmt("Regex = ~p~n",[Regex]),
    {ok, MP} = re:compile(Regex, [unicode]),
    Result =
        lists:all(fun(Elem) ->
                     Text = [Elem],
                     match == re:run(Text, MP, [{capture, none}])
                  end,
                  List),
    ?assertEqual(Expected, Result).
	
research_02_05_test() ->
    Expected = true,
    List = lists:seq(16#250, 16#2AF),
    Regex = re_tuner:unicode_block("\\p{InIPAExtensions}"),
	%?debugFmt("Regex = ~p~n",[Regex]),
    {ok, MP} = re:compile(Regex, [unicode]),
    Result =
        lists:all(fun(Elem) ->
                     Text = [Elem],
                     match == re:run(Text, MP, [{capture, none}])
                  end,
                  List),
    ?assertEqual(Expected, Result).	

research_02_06_test() ->
    Expected = true,
    List = lists:seq(16#2B0, 16#2FF),
    Regex = re_tuner:unicode_block("\\p{InSpacingModifierLetters}"),
	%?debugFmt("Regex = ~p~n",[Regex]),
    {ok, MP} = re:compile(Regex, [unicode]),
    Result =
        lists:all(fun(Elem) ->
                     Text = [Elem],
                     match == re:run(Text, MP, [{capture, none}])
                  end,
                  List),
    ?assertEqual(Expected, Result).

research_02_07_test() ->
    Expected = true,
    List = lists:seq(16#300, 16#36F),
    Regex = re_tuner:unicode_block("\\p{InCombiningDiacriticalMarks}"),
	%?debugFmt("Regex = ~p~n",[Regex]),
    {ok, MP} = re:compile(Regex, [unicode]),
    Result =
        lists:all(fun(Elem) ->
                     Text = [Elem],
                     match == re:run(Text, MP, [{capture, none}])
                  end,
                  List),
    ?assertEqual(Expected, Result).

% set chcp 65001	
research_02_08_test() ->
    Expected = true,
    List = lists:seq(16#370, 16#3FF),
    Regex = re_tuner:unicode_block("\\p{InGreekandCoptic}"),
	%?debugFmt("Regex = ~p~n",[Regex]),
    {ok, MP} = re:compile(Regex, [unicode]),
    Result =
        lists:all(fun(Elem) ->
                     Text = [Elem],
					 ?debugFmt("Letter = ~ts, Value =~.16B  ",[Text,Elem]),
                     match == re:run(Text, MP, [{capture, none}])
                  end,
                  List),
    ?assertEqual(Expected, Result).	

-endif.
