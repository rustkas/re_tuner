-module(shorthands_w_tests).

%%
%% Tests
%%
-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

-import(eunit_helper, [check_all_by_regex/3, check_all_by_regex/4]).

get_valid_character_list() ->
    ValidCharacterList =
        lists:seq(48, 57) ++ lists:seq(65, 90) ++ [95] ++ lists:seq(97, 122),
    ValidCharacterList.

research_01_test() ->
    Expected = true,
    ValidCharacterList = get_valid_character_list(),
    RegularExpression = "\\w",
    TunedRegularExpression = re_tuner:tune(RegularExpression),
    {ok, MP} = re:compile(TunedRegularExpression),

    Result = check_all_by_regex(MP, ValidCharacterList, true),
    ?assertEqual(Expected, Result).

research_02_test() ->
    Expected = true,
    ValidCharacterList = get_valid_character_list(),
    RegularExpression = "[^\\w]",
    TunedRegularExpression = re_tuner:tune(RegularExpression),
    {ok, MP} = re:compile(TunedRegularExpression),
    Result = Result = check_all_by_regex(MP, ValidCharacterList, false, match),
    ?assertEqual(Expected, Result).

-endif.
