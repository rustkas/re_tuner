-module(posix_cntrl_tests).

%%
%% Tests
%%
-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

-import(eunit_helper, [check_all_by_regex/3]).

get_valid_character_list() ->
    ValidCharacterList =  lists:seq(0, 31),
    ValidCharacterList.

research_01_test() ->
    Expected = true,
    ValidCharacterList = get_valid_character_list(),
    RegularExpression = "[[:cntrl:]]",
    TunedRegularExpression = re_tuner:tune(RegularExpression),
    {ok, MP} = re:compile(TunedRegularExpression),
    Result = check_all_by_regex(MP, ValidCharacterList, true),
    ?assertEqual(Expected, Result).

research_02_test() ->
    Expected = true,
    ValidCharacterList = get_valid_character_list(),
    RegularExpression = "[[:^cntrl:]]",
    TunedRegularExpression = re_tuner:tune(RegularExpression),
    {ok, MP} = re:compile(TunedRegularExpression),
    Result = Result = check_all_by_regex(MP, ValidCharacterList, false),
    ?assertEqual(Expected, Result).

-endif.
