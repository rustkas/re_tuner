%% @author Anatolii Kosorukov <java1cprog@yandex.ru> [rustkas.github.io/]
%% @copyright 2021 by Anatolii Kosorukov
%% @doc Helper function for working with Regular Expression Erlanb re module.

-module(re_tuner).

-export([tune/1, avoid_characters/0, save_pattern/1, replace/1, mp/1, mp/2]).

-type mp() :: {re_pattern, term(), term(), term(), term()}.

-type nl_spec() :: cr | crlf | lf | anycrlf | any.

-type compile_option() :: unicode | anchored | caseless | dollar_endonly | dotall |
    extended | firstline | multiline | no_auto_capture |
    dupnames | ungreedy |
    {newline, nl_spec()} |
    bsr_anycrlf | bsr_unicode | no_start_optimize | ucp |
    never_utf.

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
% \w
tune("\\w") ->
    "[0-9_a-zA-Z]";
% [^\w]
tune("[^\\w]") ->
    "[^0-9_a-zA-Z]";
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

%% @doc Make save Regex pattern which make literal for any character.
%% @returns Save pattern

-spec save_pattern(Pattern) -> SavePattern
    when Pattern :: string(),
         SavePattern :: string().
save_pattern(Pattern) ->
    "\\Q" ++ Pattern ++ "\\E".

%% @doc Replace one of shorthand pattern from the list `[\s,\w,\h,v]'
%% in a pattern string.
%% <br/>
%% Don't apply `\w' shorthand to unicode content.
%% @param Pattern searched regex pattern for replacing
%% @returns Updated Regex pattern string

-spec replace(Pattern) -> UpdatedPattern
    when Pattern :: string(),
         UpdatedPattern :: string().
replace(Pattern) ->
    CheckedShorthandsPlus =
        [% \s
         {"\\x{5c}s(?=\\+)", "[ \\\\t\\\\n\\\\r]"},
         % \w
         {"\\x{5c}w(?=\\+)", "[0-9_a-zA-Z]"},
         % \h
         {"\\x{5c}h(?=\\+)", "[\\\\x9\\\\x20]"},
         % \v
         {"\\x{5c}v(?=\\+)", "[\\\\xA-\\\\xD]"}],

    UpdatedPatternPlus =
        lists:foldl(fun(Elem, Acc) ->
                       Regex = element(1, Elem),
                       Markup = element(2, Elem),
                       NewContent = re:replace(Acc, Regex, Markup, [global, {return, list}]),
                       NewContent
                    end,
                    Pattern,
                    CheckedShorthandsPlus),

    CheckedShorthandsQuestionMark =
        [% \s
         {"\\x{5c}s(?=\\?)", "[ \\\\t\\\\n\\\\r]"},
         % \w
         {"\\x{5c}w(?=\\?)", "[0-9_a-zA-Z]"},
         % \h
         {"\\x{5c}h(?=\\?)", "[\\\\x9\\\\x20]"},
         % \v
         {"\\x{5c}v(?=\\?)", "[\\\\xA-\\\\xD]"}],

    UpdatedPatternQuestionMark =
        lists:foldl(fun(Elem, Acc) ->
                       Regex = element(1, Elem),
                       Markup = element(2, Elem),
                       NewContent = re:replace(Acc, Regex, Markup, [global, {return, list}]),
                       NewContent
                    end,
                    UpdatedPatternPlus,
                    CheckedShorthandsQuestionMark),

    CheckedShorthandsAsterisk =
        [% \s
         {"\\x{5c}s(?=\\*)", "[ \\\\t\\\\n\\\\r]"},
         % \w
         {"\\x{5c}w(?=\\*)", "[0-9_a-zA-Z]"},
         % \h
         {"\\x{5c}h(?=\\*)", "[\\\\x9\\\\x20]"},
         % \v
         {"\\x{5c}v(?=\\*)", "[\\\\xA-\\\\xD]"}],

    UpdatedPatternAsterisk =
        lists:foldl(fun(Elem, Acc) ->
                       Regex = element(1, Elem),
                       Markup = element(2, Elem),
                       NewContent = re:replace(Acc, Regex, Markup, [global, {return, list}]),
                       NewContent
                    end,
                    UpdatedPatternQuestionMark,
                    CheckedShorthandsAsterisk),

    CheckedShorthands =
        [% \s
         {"\\x{5c}s", " \\\\t\\\\n\\\\r"},
         % \w
         {"\\x{5c}w", "0-9_a-zA-Z"},
         % \h
         {"\\x{5c}h", "\\\\x9\\\\x20"},
         % \v
         {"\\x{5c}v", "\\\\xA-\\\\xD"}],

    UpdatedPattern =
        lists:foldl(fun(Elem, Acc) ->
                       Regex = element(1, Elem),
                       Markup = element(2, Elem),
                       NewContent = re:replace(Acc, Regex, Markup, [global, {return, list}]),
                       NewContent
                    end,
                    UpdatedPatternAsterisk,
                    CheckedShorthands),

    UpdatedPattern.

%% @doc It is reduced form of `re:compile/1' function.
%% Return opaque data type containing a compiled regular expression or raise an error `badarg'.
%% <br/>
%% <b>See also:</b>
%% [http://erlang.org/doc/man/re.html#type-mp mp()].
%% @param Regex regex pattern
%% @returns Opaque data type containing a compiled regular expression

-spec mp(Regex) -> MP | {error, badarg}
    when Regex :: string(),
         MP :: mp().
mp(Regex) ->
    case re:compile(Regex) of
        {ok, MP} ->
            MP;
        {error, _ErrSpec} ->
            error(badarg)
    end.

%% @doc It is reduced form of `re:compile/1' function.
%% Return opaque data type containing a compiled regular expression or raise an error `badarg'.
%% <br/>
%% <b>See also:</b>
%% [http://erlang.org/doc/man/re.html#type-mp mp()].
%% @param Regex regex pattern
%% @param Options additional regular expression metadata
%% @returns Opaque data type containing a compiled regular expression

-spec mp(Regex,Options) -> MP | {error, badarg}
    when Regex :: string(),
	     Options :: [Option],
         MP :: mp(),
		 Option :: compile_option().
		 
mp(Regex,Options) ->
    case re:compile(Regex,Options) of
        {ok, MP} ->
            MP;
        {error, _ErrSpec} ->
            error(badarg)
    end.