%% @author Anatolii Kosorukov <java1cprog@yandex.ru> [rustkas.github.io/]
%% @copyright 2021 by Anatolii Kosorukov
%% @doc Helper function for working with Regular Expression Erlanb re module.

-module(re_tuner).

-export([tune/1, avoid_characters/0, save_pattern/1, replace/1, mp/1, mp/2,
         unicode_block/1, is_match/2]).

-type mp() :: {re_pattern, term(), term(), term(), term()}.
-type nl_spec() :: cr | crlf | lf | anycrlf | any.
-type compile_option() ::
    unicode |
    anchored |
    caseless |
    dollar_endonly |
    dotall |
    extended |
    firstline |
    multiline |
    no_auto_capture |
    dupnames |
    ungreedy |
    {newline, nl_spec()} |
    bsr_anycrlf |
    bsr_unicode |
    no_start_optimize |
    ucp |
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

-spec mp(Regex, Options) -> MP | {error, badarg}
    when Regex :: string(),
         Options :: [Option],
         MP :: mp(),
         Option :: compile_option().
mp(Regex, Options) ->
    case re:compile(Regex, Options) of
        {ok, MP} ->
            MP;
        {error, _ErrSpec} ->
            error(badarg)
    end.

%% @doc The Unicode character database divides all the code points into blocks. Each block
%% consists of a single range of code points. The code points U+0000 through U+FFFF
%% are divided into 156 blocks in version 6.1 of the Unicode standard.
%% <br/>
%% ```
%% ‹U+0000…U+007F \p{InBasicLatin}›
%% ‹U+0080…U+00FF \p{InLatin-1Supplement}›
%% ‹U+0100…U+017F \p{InLatinExtended-A}›
%% ‹U+0180…U+024F \p{InLatinExtended-B}›
%% ‹U+0250…U+02AF \p{InIPAExtensions}›
%% ‹U+02B0…U+02FF \p{InSpacingModifierLetters}›
%% ‹U+0300…U+036F \p{InCombiningDiacriticalMarks}›
%% ‹U+0370…U+03FF \p{InGreekandCoptic}›
%% ‹U+0400…U+04FF \p{InCyrillic}›
%% ‹U+0500…U+052F \p{InCyrillicSupplement}›
%% ‹U+0530…U+058F \p{InArmenian}›
%% ‹U+0590…U+05FF \p{InHebrew}›
%% ‹U+0600…U+06FF \p{InArabic}›
%% ‹U+0700…U+074F \p{InSyriac}›
%% ‹U+0750…U+077F \p{InArabicSupplement}›
%% ‹U+0780…U+07BF \p{InThaana}›
%% ‹U+07C0…U+07FF \p{InNKo}›
%% ‹U+0800…U+083F \p{InSamaritan}›
%% ‹U+0840…U+085F \p{InMandaic}›
%% ‹U+08A0…U+08FF \p{InArabicExtended-A}›
%% ‹U+0900…U+097F \p{InDevanagari}›
%% ‹U+0980…U+09FF \p{InBengali}›
%% ‹U+0A00…U+0A7F \p{InGurmukhi}›
%% ‹U+0A80…U+0AFF \p{InGujarati}›
%% ‹U+0B00…U+0B7F \p{InOriya}›
%% ‹U+0B80…U+0BFF \p{InTamil}›
%% ‹U+0C00…U+0C7F \p{InTelugu}›
%% ‹U+0C80…U+0CFF \p{InKannada}›
%% ‹U+0D00…U+0D7F \p{InMalayalam}›
%% ‹U+0D80…U+0DFF \p{InSinhala}›
%% ‹U+0E00…U+0E7F \p{InThai}›
%% ‹U+0E80…U+0EFF \p{InLao}›
%% ‹U+0F00…U+0FFF \p{InTibetan}›
%% ‹U+1000…U+109F \p{InMyanmar}›
%% ‹U+10A0…U+10FF \p{InGeorgian}›
%% ‹U+1100…U+11FF \p{InHangulJamo}›
%% ‹U+1200…U+137F \p{InEthiopic}›
%% ‹U+1380…U+139F \p{InEthiopicSupplement}›
%% ‹U+13A0…U+13FF \p{InCherokee}›
%% ‹U+1400…U+167F \p{InUnifiedCanadianAboriginalSyllabics}›
%% ‹U+1680…U+169F \p{InOgham}›
%% ‹U+16A0…U+16FF \p{InRunic}›
%% ‹U+1700…U+171F \p{InTagalog}›
%% ‹U+1720…U+173F \p{InHanunoo}›
%% ‹U+1740…U+175F \p{InBuhid}›
%% ‹U+1760…U+177F \p{InTagbanwa}›
%% ‹U+1780…U+17FF \p{InKhmer}›
%% ‹U+1800…U+18AF \p{InMongolian}›
%% ‹U+18B0…U+18FF \p{InUnifiedCanadianAboriginalSyllabicsExtended}›
%% ‹U+1900…U+194F \p{InLimbu}›
%% ‹U+1950…U+197F \p{InTaiLe}›
%% ‹U+1980…U+19DF \p{InNewTaiLue}›
%% ‹U+19E0…U+19FF \p{InKhmerSymbols}›
%% ‹U+1A00…U+1A1F \p{InBuginese}›
%% ‹U+1A20…U+1AAF \p{InTaiTham}›
%% ‹U+1B00…U+1B7F \p{InBalinese}›
%% ‹U+1B80…U+1BBF \p{InSundanese}›
%% ‹U+1BC0…U+1BFF \p{InBatak}›
%% ‹U+1C00…U+1C4F \p{InLepcha}›
%% ‹U+1C50…U+1C7F \p{InOlChiki}›
%% ‹U+1CC0…U+1CCF \p{InSundaneseSupplement}›
%% ‹U+1CD0…U+1CFF \p{InVedicExtensions}›
%% ‹U+1D00…U+1D7F \p{InPhoneticExtensions}›
%% ‹U+1D80…U+1DBF \p{InPhoneticExtensionsSupplement}›
%% ‹U+1DC0…U+1DFF \p{InCombiningDiacriticalMarksSupplement}›
%% ‹U+1E00…U+1EFF \p{InLatinExtendedAdditional}›
%% ‹U+1F00…U+1FFF \p{InGreekExtended}›
%% ‹U+2000…U+206F \p{InGeneralPunctuation}›
%% ‹U+2070…U+209F \p{InSuperscriptsandSubscripts}›
%% ‹U+20A0…U+20CF \p{InCurrencySymbols}›
%% ‹U+20D0…U+20FF \p{InCombiningDiacriticalMarksforSymbols}›
%% ‹U+2100…U+214F \p{InLetterlikeSymbols}›
%% ‹U+2150…U+218F \p{InNumberForms}›
%% ‹U+2190…U+21FF \p{InArrows}›
%% ‹U+2200…U+22FF \p{InMathematicalOperators}›
%% ‹U+2300…U+23FF \p{InMiscellaneousTechnical}›
%% ‹U+2400…U+243F \p{InControlPictures}›
%% ‹U+2440…U+245F \p{InOpticalCharacterRecognition}›
%% ‹U+2460…U+24FF \p{InEnclosedAlphanumerics}›
%% ‹U+2500…U+257F \p{InBoxDrawing}›
%% ‹U+2580…U+259F \p{InBlockElements}›
%% ‹U+25A0…U+25FF \p{InGeometricShapes}›
%% ‹U+2600…U+26FF \p{InMiscellaneousSymbols}›
%% ‹U+2700…U+27BF \p{InDingbats}›
%% ‹U+27C0…U+27EF \p{InMiscellaneousMathematicalSymbols-A}›
%% ‹U+27F0…U+27FF \p{InSupplementalArrows-A}›
%% ‹U+2800…U+28FF \p{InBraillePatterns}›
%% ‹U+2900…U+297F \p{InSupplementalArrows-B}›
%% ‹U+2980…U+29FF \p{InMiscellaneousMathematicalSymbols-B}›
%% ‹U+2A00…U+2AFF \p{InSupplementalMathematicalOperators}›
%% ‹U+2B00…U+2BFF \p{InMiscellaneousSymbolsandArrows}›
%% ‹U+2C00…U+2C5F \p{InGlagolitic}›
%% ‹U+2C60…U+2C7F \p{InLatinExtended-C}›
%% ‹U+2C80…U+2CFF \p{InCoptic}›
%% ‹U+2D00…U+2D2F \p{InGeorgianSupplement}›
%% ‹U+2D30…U+2D7F \p{InTifinagh}›
%% ‹U+2D80…U+2DDF \p{InEthiopicExtended}›
%% ‹U+2DE0…U+2DFF \p{InCyrillicExtended-A}›
%% ‹U+2E00…U+2E7F \p{InSupplementalPunctuation}›
%% ‹U+2E80…U+2EFF \p{InCJKRadicalsSupplement}›
%% ‹U+2F00…U+2FDF \p{InKangxiRadicals}›
%% ‹U+2FF0…U+2FFF \p{InIdeographicDescriptionCharacters}›
%% ‹U+3000…U+303F \p{InCJKSymbolsandPunctuation}›
%% ‹U+3040…U+309F \p{InHiragana}›
%% ‹U+30A0…U+30FF \p{InKatakana}›
%% ‹U+3100…U+312F \p{InBopomofo}›
%% ‹U+3130…U+318F \p{InHangulCompatibilityJamo}›
%% ‹U+3190…U+319F \p{InKanbun}›
%% ‹U+31A0…U+31BF \p{InBopomofoExtended}›
%% ‹U+31C0…U+31EF \p{InCJKStrokes}›
%% ‹U+31F0…U+31FF \p{InKatakanaPhoneticExtensions}›
%% ‹U+3200…U+32FF \p{InEnclosedCJKLettersandMonths}›
%% ‹U+3300…U+33FF \p{InCJKCompatibility}›
%% ‹U+3400…U+4DBF \p{InCJKUnifiedIdeographsExtensionA}›
%% ‹U+4DC0…U+4DFF \p{InYijingHexagramSymbols}›
%% ‹U+4E00…U+9FFF \p{InCJKUnifiedIdeographs}›
%% ‹U+A000…U+A48F \p{InYiSyllables}›
%% ‹U+A490…U+A4CF \p{InYiRadicals}›
%% ‹U+A4D0…U+A4FF \p{InLisu}›
%% ‹U+A500…U+A63F \p{InVai}›
%% ‹U+A640…U+A69F \p{InCyrillicExtended-B}›
%% ‹U+A6A0…U+A6FF \p{InBamum}›
%% ‹U+A700…U+A71F \p{InModifierToneLetters}›
%% ‹U+A720…U+A7FF \p{InLatinExtended-D}›
%% ‹U+A800…U+A82F \p{InSylotiNagri}›
%% ‹U+A830…U+A83F \p{InCommonIndicNumberForms}›
%% ‹U+A840…U+A87F \p{InPhags-pa}›
%% ‹U+A880…U+A8DF \p{InSaurashtra}›
%% ‹U+A8E0…U+A8FF \p{InDevanagariExtended}›
%% ‹U+A900…U+A92F \p{InKayahLi}›
%% ‹U+A930…U+A95F \p{InRejang}›
%% ‹U+A960…U+A97F \p{InHangulJamoExtended-A}›
%% ‹U+A980…U+A9DF \p{InJavanese}›
%% ‹U+AA00…U+AA5F \p{InCham}›
%% ‹U+AA60…U+AA7F \p{InMyanmarExtended-A}›
%% ‹U+AA80…U+AADF \p{InTaiViet}›
%% ‹U+AAE0…U+AAFF \p{InMeeteiMayekExtensions}›
%% ‹U+AB00…U+AB2F \p{InEthiopicExtended-A}›
%% ‹U+ABC0…U+ABFF \p{InMeeteiMayek}›
%% ‹U+AC00…U+D7AF \p{InHangulSyllables}›
%% ‹U+D7B0…U+D7FF \p{InHangulJamoExtended-B}›
%% ‹U+D800…U+DB7F \p{InHighSurrogates}›
%% ‹U+DB80…U+DBFF \p{InHighPrivateUseSurrogates}›
%% ‹U+DC00…U+DFFF \p{InLowSurrogates}›
%% ‹U+E000…U+F8FF \p{InPrivateUseArea}›
%% ‹U+F900…U+FAFF \p{InCJKCompatibilityIdeographs}›
%% ‹U+FB00…U+FB4F \p{InAlphabeticPresentationForms}›
%% ‹U+FB50…U+FDFF \p{InArabicPresentationForms-A}›
%% ‹U+FE00…U+FE0F \p{InVariationSelectors}›
%% ‹U+FE10…U+FE1F \p{InVerticalForms}›
%% ‹U+FE20…U+FE2F \p{InCombiningHalfMarks}›
%% ‹U+FE30…U+FE4F \p{InCJKCompatibilityForms}›
%% ‹U+FE50…U+FE6F \p{InSmallFormVariants}›
%% ‹U+FE70…U+FEFF \p{InArabicPresentationForms-B}›
%% ‹U+FF00…U+FFEF \p{InHalfwidthandFullwidthForms}›
%% ‹U+FFF0…U+FFFF \p{InSpecials}›
%% '''
%%
%% @param BlockName is Regular Expression block name
%% @returns Regular Expressions range of code points

-spec unicode_block(BlockName) -> Range | nomatch
    when BlockName :: string(),
         Range :: string().
unicode_block(BlockName) ->
    UnicodeBlockList =
        [{"\\p{InAlphabeticPresentationForms}", "[\\x{FB00}-\\x{FB4F}]"},
         {"\\p{InArabic}", "[\\x{600}-\\x{6FF}]"},
         {"\\p{InArabicExtended-A}", "[\\x{8A0}-\\x{8FF}]"},
         {"\\p{InArabicPresentationForms-A}", "[\\x{FB50}-\\x{FDFF}]"},
         {"\\p{InArabicPresentationForms-B}", "[\\x{FE70}-\\x{FEFF}]"},
         {"\\p{InArabicSupplement}", "[\\x{750}-\\x{77F}]"},
         {"\\p{InArmenian}", "[\\x{530}-\\x{58F}]"}, {"\\p{InArrows}", "[\\x{2190}-\\x{21FF}]"},
         {"\\p{InBalinese}", "[\\x{1B00}-\\x{1B7F}]"}, {"\\p{InBamum}", "[\\x{A6A0}-\\x{A6FF}]"},
         {"\\p{InBasicLatin}", "[\\x0-\\x7F]"}, {"\\p{InBatak}", "[\\x{1BC0}-\\x{1BFF}]"},
         {"\\p{InBengali}", "[\\x{980}-\\x{9FF}]"},
         {"\\p{InBlockElements}", "[\\x{2580}-\\x{259F}]"},
         {"\\p{InBopomofo}", "[\\x{3100}-\\x{312F}]"},
         {"\\p{InBopomofoExtended}", "[\\x{31A0}-\\x{31BF}]"},
         {"\\p{InBoxDrawing}", "[\\x{2500}-\\x{257F}]"},
         {"\\p{InBraillePatterns}", "[\\x{2800}-\\x{28FF}]"},
         {"\\p{InBuginese}", "[\\x{1A00}-\\x{1A1F}]"}, {"\\p{InBuhid}", "[\\x{1740}-\\x{175F}]"},
         {"\\p{InCham}", "[\\x{AA00}-\\x{AA5F}]"}, {"\\p{InCherokee}", "[\\x{13A0}-\\x{13FF}]"},
         {"\\p{InCJKCompatibility}", "[\\x{3300}-\\x{33FF}]"},
         {"\\p{InCJKCompatibilityForms}", "[\\x{FE30}-\\x{FE4F}]"},
         {"\\p{InCJKCompatibilityIdeographs}", "[\\x{F900}-\\x{FAFF}]"},
         {"\\p{InCJKRadicalsSupplement}", "[\\x{2E80}-\\x{2EFF}]"},
         {"\\p{InCJKStrokes}", "[\\x{31C0}-\\x{31EF}]"},
         {"\\p{InCJKSymbolsandPunctuation}", "[\\x{3000}-\\x{303F}]"},
         {"\\p{InCJKUnifiedIdeographs}", "[\\x{4E00}-\\x{9FFF}]"},
         {"\\p{InCJKUnifiedIdeographsExtensionA}", "[\\x{3400}-\\x{4DBF}]"},
         {"\\p{InCombiningDiacriticalMarks}", "[\\x{300}-\\x{36F}]"},
         {"\\p{InCombiningDiacriticalMarksforSymbols}", "[\\x{20D0}-\\x{20FF}]"},
         {"\\p{InCombiningDiacriticalMarksSupplement}", "[\\x{1DC0}-\\x{1DFF}]"},
         {"\\p{InCombiningHalfMarks}", "[\\x{FE20}-\\x{FE2F}]"},
         {"\\p{InCommonIndicNumberForms}", "[\\x{A830}-\\x{A83F}]"},
         {"\\p{InControlPictures}", "[\\x{2400}-\\x{243F}]"},
         {"\\p{InCoptic}", "[\\x{2C80}-\\x{2CFF}]"},
         {"\\p{InCurrencySymbols}", "[\\x{20A0}-\\x{20CF}]"},
         {"\\p{InCyrillic}", "[\\x{400}-\\x{4FF}]"},
         {"\\p{InCyrillicExtended-A}", "[\\x{2DE0}-\\x{2DFF}]"},
         {"\\p{InCyrillicExtended-B}", "[\\x{A640}-\\x{A69F}]"},
         {"\\p{InCyrillicSupplement}", "[\\x{500}-\\x{52F}]"},
         {"\\p{InDevanagari}", "[\\x{900}-\\x{97F}]"},
         {"\\p{InDevanagariExtended}", "[\\x{A8E0}-\\x{A8FF}]"},
         {"\\p{InDingbats}", "[\\x{2700}-\\x{27BF}]"},
         {"\\p{InEnclosedAlphanumerics}", "[\\x{2460}-\\x{24FF}]"},
         {"\\p{InEnclosedCJKLettersandMonths}", "[\\x{3200}-\\x{32FF}]"},
         {"\\p{InEthiopic}", "[\\x{1200}-\\x{137F}]"},
         {"\\p{InEthiopicExtended}", "[\\x{2D80}-\\x{2DDF}]"},
         {"\\p{InEthiopicExtended-A}", "[\\x{AB00}-\\x{AB2F}]"},
         {"\\p{InEthiopicSupplement}", "[\\x{1380}-\\x{139F}]"},
         {"\\p{InGeneralPunctuation}", "[\\x{2000}-\\x{206F}]"},
         {"\\p{InGeometricShapes}", "[\\x{25A0}-\\x{25FF}]"},
         {"\\p{InGeorgian}", "[\\x{10A0}-\\x{10FF}]"},
         {"\\p{InGeorgianSupplement}", "[\\x{2D00}-\\x{2D2F}]"},
         {"\\p{InGlagolitic}", "[\\x{2C00}-\\x{2C5F}]"},
         {"\\p{InGreekandCoptic}", "[\\x{370}-\\x{3FF}]"},
         {"\\p{InGreekExtended}", "[\\x{1F00}-\\x{1FFF}]"},
         {"\\p{InGujarati}", "[\\x{A80}-\\x{AFF}]"}, {"\\p{InGurmukhi}", "[\\x{A00}-\\x{A7F}]"},
         {"\\p{InHalfwidthandFullwidthForms}", "[\\x{FF00}-\\x{FFEF}]"},
         {"\\p{InHangulCompatibilityJamo}", "[\\x{3130}-\\x{318F}]"},
         {"\\p{InHangulJamo}", "[\\x{1100}-\\x{11FF}]"},
         {"\\p{InHangulJamoExtended-A}", "[\\x{A960}-\\x{A97F}]"},
         {"\\p{InHangulJamoExtended-B}", "[\\x{D7B0}-\\x{D7FF}]"},
         {"\\p{InHangulSyllables}", "[\\x{AC00}-\\x{D7AF}]"},
         {"\\p{InHanunoo}", "[\\x{1720}-\\x{173F}]"}, {"\\p{InHebrew}", "[\\x{590}-\\x{5FF}]"},
         {"\\p{InHighPrivateUseSurrogates}", "[\\x{DB80}-\\x{DBFF}]"},
         {"\\p{InHighSurrogates}", "[\\x{D800}-\\x{DB7F}]"},
         {"\\p{InHiragana}", "[\\x{3040}-\\x{309F}]"},
         {"\\p{InIdeographicDescriptionCharacters}", "[\\x{2FF0}-\\x{2FFF}]"},
         {"\\p{InIPAExtensions}", "[\\x{250}-\\x{2AF}]"},
         {"\\p{InJavanese}", "[\\x{A980}-\\x{A9DF}]"},
         {"\\p{InKanbun}", "[\\x{3190}-\\x{319F}]"},
         {"\\p{InKangxiRadicals}", "[\\x{2F00}-\\x{2FDF}]"},
         {"\\p{InKannada}", "[\\x{C80}-\\x{CFF}]"}, {"\\p{InKatakana}", "[\\x{30A0}-\\x{30FF}]"},
         {"\\p{InKatakanaPhoneticExtensions}", "[\\x{31F0}-\\x{31FF}]"},
         {"\\p{InKayahLi}", "[\\x{A900}-\\x{A92F}]"}, {"\\p{InKhmer}", "[\\x{1780}-\\x{17FF}]"},
         {"\\p{InKhmerSymbols}", "[\\x{19E0}-\\x{19FF}]"}, {"\\p{InLao}", "[\\x{E80}-\\x{EFF}]"},
         {"\\p{InLatin-1Supplement}", "[\\x80-\\xFF]"},
         {"\\p{InLatinExtended-A}", "[\\x{100}-\\x{17F}]"},
         {"\\p{InLatinExtendedAdditional}", "[\\x{1E00}-\\x{1EFF}]"},
         {"\\p{InLatinExtended-B}", "[\\x{180}-\\x{24F}]"},
         {"\\p{InLatinExtended-C}", "[\\x{2C60}-\\x{2C7F}]"},
         {"\\p{InLatinExtended-D}", "[\\x{A720}-\\x{A7FF}]"},
         {"\\p{InLepcha}", "[\\x{1C00}-\\x{1C4F}]"},
         {"\\p{InLetterlikeSymbols}", "[\\x{2100}-\\x{214F}]"},
         {"\\p{InLimbu}", "[\\x{1900}-\\x{194F}]"}, {"\\p{InLisu}", "[\\x{A4D0}-\\x{A4FF}]"},
         {"\\p{InLowSurrogates}", "[\\x{DC00}-\\x{DFFF}]"},
         {"\\p{InMalayalam}", "[\\x{D00}-\\x{D7F}]"}, {"\\p{InMandaic}", "[\\x{840}-\\x{85F}]"},
         {"\\p{InMathematicalOperators}", "[\\x{2200}-\\x{22FF}]"},
         {"\\p{InMeeteiMayek}", "[\\x{ABC0}-\\x{ABFF}]"},
         {"\\p{InMeeteiMayekExtensions}", "[\\x{AAE0}-\\x{AAFF}]"},
         {"\\p{InMiscellaneousMathematicalSymbols-A}", "[\\x{27C0}-\\x{27EF}]"},
         {"\\p{InMiscellaneousMathematicalSymbols-B}", "[\\x{2980}-\\x{29FF}]"},
         {"\\p{InMiscellaneousSymbols}", "[\\x{2600}-\\x{26FF}]"},
         {"\\p{InMiscellaneousSymbolsandArrows}", "[\\x{2B00}-\\x{2BFF}]"},
         {"\\p{InMiscellaneousTechnical}", "[\\x{2300}-\\x{23FF}]"},
         {"\\p{InModifierToneLetters}", "[\\x{A700}-\\x{A71F}]"},
         {"\\p{InMongolian}", "[\\x{1800}-\\x{18AF}]"},
         {"\\p{InMyanmar}", "[\\x{1000}-\\x{109F}]"},
         {"\\p{InMyanmarExtended-A}", "[\\x{AA60}-\\x{AA7F}]"},
         {"\\p{InNewTaiLue}", "[\\x{1980}-\\x{19DF}]"}, {"\\p{InNKo}", "[\\x{7C0}-\\x{7FF}]"},
         {"\\p{InNumberForms}", "[\\x{2150}-\\x{218F}]"},
         {"\\p{InOgham}", "[\\x{1680}-\\x{169F}]"}, {"\\p{InOlChiki}", "[\\x{1C50}-\\x{1C7F}]"},
         {"\\p{InOpticalCharacterRecognition}", "[\\x{2440}-\\x{245F}]"},
         {"\\p{InOriya}", "[\\x{B00}-\\x{B7F}]"}, {"\\p{InPhags-pa}", "[\\x{A840}-\\x{A87F}]"},
         {"\\p{InPhoneticExtensions}", "[\\x{1D00}-\\x{1D7F}]"},
         {"\\p{InPhoneticExtensionsSupplement}", "[\\x{1D80}-\\x{1DBF}]"},
         {"\\p{InPrivateUseArea}", "[\\x{E000}-\\x{F8FF}]"},
         {"\\p{InRejang}", "[\\x{A930}-\\x{A95F}]"}, {"\\p{InRunic}", "[\\x{16A0}-\\x{16FF}]"},
         {"\\p{InSamaritan}", "[\\x{800}-\\x{83F}]"},
         {"\\p{InSaurashtra}", "[\\x{A880}-\\x{A8DF}]"},
         {"\\p{InSinhala}", "[\\x{D80}-\\x{DFF}]"},
         {"\\p{InSmallFormVariants}", "[\\x{FE50}-\\x{FE6F}]"},
         {"\\p{InSpacingModifierLetters}", "[\\x{2B0}-\\x{2FF}]"},
         {"\\p{InSpecials}", "[\\x{FFF0}-\\x{FFFF}]"},
         {"\\p{InSundanese}", "[\\x{1B80}-\\x{1BBF}]"},
         {"\\p{InSundaneseSupplement}", "[\\x{1CC0}-\\x{1CCF}]"},
         {"\\p{InSuperscriptsandSubscripts}", "[\\x{2070}-\\x{209F}]"},
         {"\\p{InSupplementalArrows-A}", "[\\x{27F0}-\\x{27FF}]"},
         {"\\p{InSupplementalArrows-B}", "[\\x{2900}-\\x{297F}]"},
         {"\\p{InSupplementalMathematicalOperators}", "[\\x{2A00}-\\x{2AFF}]"},
         {"\\p{InSupplementalPunctuation}", "[\\x{2E00}-\\x{2E7F}]"},
         {"\\p{InSylotiNagri}", "[\\x{A800}-\\x{A82F}]"},
         {"\\p{InSyriac}", "[\\x{700}-\\x{74F}]"}, {"\\p{InTagalog}", "[\\x{1700}-\\x{171F}]"},
         {"\\p{InTagbanwa}", "[\\x{1760}-\\x{177F}]"}, {"\\p{InTaiLe}", "[\\x{1950}-\\x{197F}]"},
         {"\\p{InTaiTham}", "[\\x{1A20}-\\x{1AAF}]"},
         {"\\p{InTaiViet}", "[\\x{AA80}-\\x{AADF}]"}, {"\\p{InTamil}", "[\\x{B80}-\\x{BFF}]"},
         {"\\p{InTelugu}", "[\\x{C00}-\\x{C7F}]"}, {"\\p{InThaana}", "[\\x{780}-\\x{7BF}]"},
         {"\\p{InThai}", "[\\x{E00}-\\x{E7F}]"}, {"\\p{InTibetan}", "[\\x{F00}-\\x{FFF}]"},
         {"\\p{InTifinagh}", "[\\x{2D30}-\\{2D7F}x]"},
         {"\\p{InUnifiedCanadianAboriginalSyllabics}", "[\\x{1400}-\\x{167F}]"},
         {"\\p{InUnifiedCanadianAboriginalSyllabicsExtended}", "[\\x{18B0}-\\x{18FF}]"},
         {"\\p{InVai}", "[\\x{A500}-\\x{A63F}]"},
         {"\\p{InVariationSelectors}", "[\\x{FE00}-\\x{FE0F}]"},
         {"\\p{InVedicExtensions}", "[\\x{1CD0}-\\x{1CFF}]"},
         {"\\p{InVerticalForms}", "[\\x{FE10}-\\x{FE1F}]"},
         {"\\p{InYijingHexagramSymbols}", "[\\x{4DC0}-\\x{4DFF}]"},
         {"\\p{InYiRadicals}", "[\\x{A490}-\\x{A4CF}]"},
         {"\\p{InYiSyllables}", "[\\x{A000}-\\x{A48F}]"}],
    Tuple = lists:keyfind(BlockName, 1, UnicodeBlockList),
    Result =
        case Tuple of
            false ->
                nomatch;
            _ ->
                element(2, Tuple)
        end,
    Result.

%% @doc Check whether a match can be found for a particular regular expression in a particular string. 
%% A partial match is sufficient. 
%% <br/>
%% <b>See also:</b>
%% [http://erlang.org/doc/man/re.html#compile_2].
%% @param Text regex pattern
%% @param Regex regex pattern
%% @param MP compiled a regular expression
%% @returns true or false

-spec is_match(Text, ReInput) -> Result
    when Text :: string(),
	     ReInput :: string() | tuple(),
         Result :: true | false.
		 
is_match(Text, Regex) when is_list(Regex) ->
   try 
      MP = re_tuner:mp(Regex),
	  is_match(Text, MP)  
   catch
      error:Error -> false
   end;

is_match(Text, MP)  when is_tuple(MP) ->
	MatchResult = re:run(Text, MP, [{capture,none}]),
	Result = (MatchResult == match),
	Result.