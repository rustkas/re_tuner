%% @author Anatolii Kosorukov <java1cprog@yandex.ru> [rustkas.github.io/]
%% @copyright 2021 by Anatolii Kosorukov
%% @doc Helper function for working with Regular Expression Erlanb re module.

-module(re_tuner).

-export([tune/1,avoid_characters/0]).

%% @doc Replace Regex pattern to more siple one.
%% @returns Transformed Regex pattern.

-spec tune(Regex) -> Result
    when Regex :: string(),
         Result :: string().
% \s
tune("\\s") ->
    "[ \\t\\n\\r]";
% [^\s]
tune("[^\\s]") ->
    "[^ \\t\\n\\r]";
% \h
tune("\\h") ->
    "[\\x9,\\x20]";
% [^\h]
tune("[^\\h]") ->
    "[\\0-\\x8\\xA-\\x1F\\x21-x7F]";
% \v
tune("\\v") ->
    "[\\xA-\\xD]";
% [^\h]
tune("[^\\v]") ->
    "[\\0-\\x9\\xE-x7F]";
% Posix [[:alnum:]]
tune("[[:alnum:]]") ->
    "[\\x30-\\x39\\x41-\\x5A\\x61-\\x7A]";
% Posix [[:^alnum:]]
tune("[[:^alnum:]]") ->
    "[\\0-\\x29\\x3A-\\x40\\x5B-\\x60\\x7B-\\x7F]";
% Posix [[:alpha:]]
tune("[[:alpha:]]") ->
    "[\\x41-\\x5A\\x61-\\x7A]";
% Posix [[:^alpha:]]
tune("[[:^alpha:]]") ->
    "[\\0-\\x40\\x5B-\\x60\\x7B-\\x7F]";
% Posix [[:lower:]]
tune("[[:lower:]]") ->
    "[\\x61-\\x7A]";
% Posix [[:^lower:]]
tune("[[:^lower:]]") ->
    "[\\0-\\x60\\x7B-\\x7F]";
% Posix [[:upper:]]
tune("[[:upper:]]") ->
    "[\\x41-\\x5A]";
tune("[[:^upper:]]") ->
    "[\\0-\\x40\\x5B-\\xF7]";
% Posix [[:digit:]]
tune("[[:digit:]]") ->
    "[\\x30-\\x39]";
tune("[[:^digit:]]") ->
    "[\\0-\\x29\\x3A-\\x7F]";
% Posix [[:graph:]]
tune("[[:graph:]]") ->
    "[\\x21-\\x7E]";
tune("[[:^graph:]]") ->
    "[\\0-\\x20\\x7F]";
% Posix [[:print:]]
tune("[[:print:]]") ->
    "[\\x20-\\x7E]";
tune("[[:^print:]]") ->
    "[\\0-\\x1F\\x7F]";
% Posix [[:punct:]]
tune("[[:punct:]]") ->
    "[\\x21-\\x2F\\x3A-\\x40\\x5B-\\x60\\x7B-\\x7E]";
tune("[[:^punct:]]") ->
    "[\\0-\\x20\\x30-\\x39\\x41-\\x5A\\x61-\\x7A\\x7F]";
% Posix [[:space:]]
tune("[[:space:]]") ->
    "[\\x9-\\xD\\x20]";
tune("[[:^space:]]") ->
    "[\\0-\\x8\\xE-\\x1F\\x21-\\x7F]";
% Posix [[:cntrl:]]
tune("[[:cntrl:]]") ->
    "[\\0-\\x1F]";
tune("[[:^cntrl:]]") ->
    "[\\x20-\\x7F]";
% Posix [[:ascii:]]
tune("[[:ascii:]]") ->
    "[\\0-\\x7F]".

%% @doc The list of characters which raise an error if escape character is not used.
%% @returns The list of spectial characters.

-spec avoid_characters() -> Result when Result :: string().
avoid_characters() ->
    "*.^$|{}]\-".
