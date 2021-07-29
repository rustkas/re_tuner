-module(replace_tests).

%%
%% Tests
%%
-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

-import(re_tuner, [replace/1]).

replace_s_01_test() ->
    Expected =
        "^([ \\t\\n\\r-!#$%&'*+-/=?^_`{}~]+)@((?:[ \\t\\n\\r]+[ \\t\\n\\r]?[ \\t\\n\\r]*.)+)(?:[a-zA-Z]{2,4})$",

    InputString =
        "^([\\s-!#$%&'*+-/=?^_`{\}~]+)@((?:\\s+\\s?\\s*\.)+)(?:[a-zA-Z]{2,4})$",

    NewString = replace(InputString),
    Result = NewString,
    %?debugFmt("Result = ~p~n", [NewString]).
    ?assertEqual(Expected, Result).

replace_w_01_test() ->
    Expected =
        "^([0-9_a-zA-Z-!#$%&'*+-/=?^_`{}~]+)@((?:[0-9_a-zA-Z]+[0-9_a-zA-Z]?[0-9_a-zA-Z]*.)+)(?:[a-zA-Z]{2,4})$",

    InputString =
        "^([\\w-!#$%&'*+-/=?^_`{\}~]+)@((?:\\w+\\w?\\w*\.)+)(?:[a-zA-Z]{2,4})$",

    NewString = replace(InputString),
    Result = NewString,
    %?debugFmt("Result = ~p~n", [NewString]).
    ?assertEqual(Expected, Result).

replace_w_02_test() ->
    Expected = "java1cprog@yandex.ru",

    InputString =
        "^([\\w-!#$%&'*+-/=?^_`{\}~]+)@((?:\\w+\\w?\\w*\.)+)(?:[a-zA-Z]{2,4})$",

    NewString = replace(InputString),

    EmailRegex = NewString,
    {ok, MP} = re:compile(EmailRegex),
    Email = Expected,

    {match, [Captured]} = re:run(Email, MP, [{capture, first, list}]),
    Result = Captured,
    ?assertEqual(Expected, Result).

replace_h_01_test() ->
    Expected =
        "^([\\x9\\x20-!#$%&'*+-/=?^_`{}~]+)@((?:[\\x9\\x20]+[\\x9\\x20]?[\\x9\\x20]*.)+)(?:[a-zA-Z]{2,4})$",
    InputString =
        "^([\\h-!#$%&'*+-/=?^_`{\}~]+)@((?:\\h+\\h?\\h*\.)+)(?:[a-zA-Z]{2,4})$",

    NewString = replace(InputString),
    Result = NewString,
    %?debugFmt("Result = ~p~n", [NewString]).
    ?assertEqual(Expected, Result).

replace_v_01_test() ->
    Expected =
        "^([\\xA-\\xD-!#$%&'*+-/=?^_`{}~]+)@((?:[\\xA-\\xD]+[\\xA-\\xD]?[\\xA-\\xD]*.)+)(?:[a-zA-Z]{2,4})$",
    InputString =
        "^([\\v-!#$%&'*+-/=?^_`{\}~]+)@((?:\\v+\\v?\\v*\.)+)(?:[a-zA-Z]{2,4})$",

    NewString = replace(InputString),
    Result = NewString,
    %?debugFmt("Result = ~p~n", [NewString]).
    ?assertEqual(Expected, Result).

-endif.
