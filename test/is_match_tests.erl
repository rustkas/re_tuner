-module(is_match_tests).

%%
%% Tests
%%
-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").



research_03_test()->
   Regex = "(",
   false = re_tuner:is_match("(", Regex).

research_04_test()->
   Regex = "[(]",
   MP = re_tuner:mp(Regex),
   true = re_tuner:is_match("(", MP).

-endif.
