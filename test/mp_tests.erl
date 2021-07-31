-module(mp_tests).

%%
%% Tests
%%
-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

-import(re_tuner, [mp/1]).

get_mp_01_test() ->
    Result = mp("[0-9]"),
    ?assert(is_tuple(Result)),
    {re_pattern, _, _, _, _} = Result.

get_mp_02_test() ->
    ?assertException(error, badarg, mp("[9-0]")).

-endif.
