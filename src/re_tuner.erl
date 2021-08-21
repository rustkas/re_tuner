%% @author Anatolii Kosorukov <java1cprog@yandex.ru> [rustkas.github.io/]
%% @copyright 2021 by Anatolii Kosorukov
%% @doc Helper function for working with Regular Expression Erlanb re module.

-module(re_tuner).

-export([tune/1, avoid_characters/0, save_pattern/1, replace/1, mp/1, mp/2,
         unicode_block/1]).

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
        [{"\\p{InAlphabeticPresentationForms}", "[\\xFB00-\\xFB4F]"},
         {"\\p{InArabic}", "[\\x600-\\x6FF]"},
         {"\\p{InArabicExtended-A}", "[\\x8A0-\\x8FF]"},
         {"\\p{InArabicPresentationForms-A}", "[\\xFB50-\\xFDFF]"},
         {"\\p{InArabicPresentationForms-B}", "[\\xFE70-\\xFEFF]"},
         {"\\p{InArabicSupplement}", "[\\x750-\\x77F]"},
         {"\\p{InArmenian}", "[\\x530-\\x58F]"}, {"\\p{InArrows}", "[\\x2190-\\x21FF]"},
         {"\\p{InBalinese}", "[\\x1B00-\\x1B7F]"}, {"\\p{InBamum}", "[\\xA6A0-\\xA6FF]"},
         {"\\p{InBasicLatin}", "[\\x0-\\x7F]"}, {"\\p{InBatak}", "[\\x1BC0-\\x1BFF]"},
         {"\\p{InBengali}", "[\\x980-\\x9FF]"},
         {"\\p{InBlockElements}", "[\\x2580-\\x259F]"},
         {"\\p{InBopomofo}", "[\\x3100-\\x312F]"},
         {"\\p{InBopomofoExtended}", "[\\x31A0-\\x31BF]"},
         {"\\p{InBoxDrawing}", "[\\x2500-\\x257F]"},
         {"\\p{InBraillePatterns}", "[\\x2800-\\x28FF]"},
         {"\\p{InBuginese}", "[\\x1A00-\\x1A1F]"}, {"\\p{InBuhid}", "[\\x1740-\\x175F]"},
         {"\\p{InCham}", "[\\xAA00-\\xAA5F]"}, {"\\p{InCherokee}", "[\\x13A0-\\x13FF]"},
         {"\\p{InCJKCompatibility}", "[\\x3300-\\x33FF]"},
         {"\\p{InCJKCompatibilityForms}", "[\\xFE30-\\xFE4F]"},
         {"\\p{InCJKCompatibilityIdeographs}", "[\\xF900-\\xFAFF]"},
         {"\\p{InCJKRadicalsSupplement}", "[\\x2E80-\\x2EFF]"},
         {"\\p{InCJKStrokes}", "[\\x31C0-\\x31EF]"},
         {"\\p{InCJKSymbolsandPunctuation}", "[\\x3000-\\x303F]"},
         {"\\p{InCJKUnifiedIdeographs}", "[\\x4E00-\\x9FFF]"},
         {"\\p{InCJKUnifiedIdeographsExtensionA}", "[\\x3400-\\x4DBF]"},
         {"\\p{InCombiningDiacriticalMarks}", "[\\x300-\\x36F]"},
         {"\\p{InCombiningDiacriticalMarksforSymbols}", "[\\x20D0-\\x20FF]"},
         {"\\p{InCombiningDiacriticalMarksSupplement}", "[\\x1DC0-\\x1DFF]"},
         {"\\p{InCombiningHalfMarks}", "[\\xFE20-\\xFE2F]"},
         {"\\p{InCommonIndicNumberForms}", "[\\xA830-\\xA83F]"},
         {"\\p{InControlPictures}", "[\\x2400-\\x243F]"},
         {"\\p{InCoptic}", "[\\x2C80-\\x2CFF]"},
         {"\\p{InCurrencySymbols}", "[\\x20A0-\\x20CF]"},
         {"\\p{InCyrillic}", "[\\x400-\\x4FF]"},
         {"\\p{InCyrillicExtended-A}", "[\\x2DE0-\\x2DFF]"},
         {"\\p{InCyrillicExtended-B}", "[\\xA640-\\xA69F]"},
         {"\\p{InCyrillicSupplement}", "[\\x500-\\x52F]"},
         {"\\p{InDevanagari}", "[\\x900-\\x97F]"},
         {"\\p{InDevanagariExtended}", "[\\xA8E0-\\xA8FF]"},
         {"\\p{InDingbats}", "[\\x2700-\\x27BF]"},
         {"\\p{InEnclosedAlphanumerics}", "[\\x2460-\\x24FF]"},
         {"\\p{InEnclosedCJKLettersandMonths}", "[\\x3200-\\x32FF]"},
         {"\\p{InEthiopic}", "[\\x1200-\\x137F]"},
         {"\\p{InEthiopicExtended}", "[\\x2D80-\\x2DDF]"},
         {"\\p{InEthiopicExtended-A}", "[\\xAB00-\\xAB2F]"},
         {"\\p{InEthiopicSupplement}", "[\\x1380-\\x139F]"},
         {"\\p{InGeneralPunctuation}", "[\\x2000-\\x206F]"},
         {"\\p{InGeometricShapes}", "[\\x25A0-\\x25FF]"},
         {"\\p{InGeorgian}", "[\\x10A0-\\x10FF]"},
         {"\\p{InGeorgianSupplement}", "[\\x2D00-\\x2D2F]"},
         {"\\p{InGlagolitic}", "[\\x2C00-\\x2C5F]"},
         {"\\p{InGreekandCoptic}", "[\\x370-\\x3FF]"},
         {"\\p{InGreekExtended}", "[\\x1F00-\\x1FFF]"},
         {"\\p{InGujarati}", "[\\xA80-\\xAFF]"}, {"\\p{InGurmukhi}", "[\\xA00-\\xA7F]"},
         {"\\p{InHalfwidthandFullwidthForms}", "[\\xFF00-\\xFFEF]"},
         {"\\p{InHangulCompatibilityJamo}", "[\\x3130-\\x318F]"},
         {"\\p{InHangulJamo}", "[\\x1100-\\x11FF]"},
         {"\\p{InHangulJamoExtended-A}", "[\\xA960-\\xA97F]"},
         {"\\p{InHangulJamoExtended-B}", "[\\xD7B0-\\xD7FF]"},
         {"\\p{InHangulSyllables}", "[\\xAC00-\\xD7AF]"},
         {"\\p{InHanunoo}", "[\\x1720-\\x173F]"}, {"\\p{InHebrew}", "[\\x590-\\x5FF]"},
         {"\\p{InHighPrivateUseSurrogates}", "[\\xDB80-\\xDBFF]"},
         {"\\p{InHighSurrogates}", "[\\xD800-\\xDB7F]"},
         {"\\p{InHiragana}", "[\\x3040-\\x309F]"},
         {"\\p{InIdeographicDescriptionCharacters}", "[\\x2FF0-\\x2FFF]"},
         {"\\p{InIPAExtensions}", "[\\x250-\\x2AF]"},
         {"\\p{InJavanese}", "[\\xA980-\\xA9DF]"},
         {"\\p{InKanbun}", "[\\x3190-\\x319F]"},
         {"\\p{InKangxiRadicals}", "[\\x2F00-\\x2FDF]"},
         {"\\p{InKannada}", "[\\xC80-\\xCFF]"}, {"\\p{InKatakana}", "[\\x30A0-\\x30FF]"},
         {"\\p{InKatakanaPhoneticExtensions}", "[\\x31F0-\\x31FF]"},
         {"\\p{InKayahLi}", "[\\xA900-\\xA92F]"}, {"\\p{InKhmer}", "[\\x1780-\\x17FF]"},
         {"\\p{InKhmerSymbols}", "[\\x19E0-\\x19FF]"}, {"\\p{InLao}", "[\\xE80-\\xEFF]"},
         {"\\p{InLatin-1Supplement}", "[\\x80-\\xFF]"},
         {"\\p{InLatinExtended-A}", "[\\x100-\\x17F]"},
         {"\\p{InLatinExtendedAdditional}", "[\\x1E00-\\x1EFF]"},
         {"\\p{InLatinExtended-B}", "[\\x180-\\x24F]"},
         {"\\p{InLatinExtended-C}", "[\\x2C60-\\x2C7F]"},
         {"\\p{InLatinExtended-D}", "[\\xA720-\\xA7FF]"},
         {"\\p{InLepcha}", "[\\x1C00-\\x1C4F]"},
         {"\\p{InLetterlikeSymbols}", "[\\x2100-\\x214F]"},
         {"\\p{InLimbu}", "[\\x1900-\\x194F]"}, {"\\p{InLisu}", "[\\xA4D0-\\xA4FF]"},
         {"\\p{InLowSurrogates}", "[\\xDC00-\\xDFFF]"},
         {"\\p{InMalayalam}", "[\\xD00-\\xD7F]"}, {"\\p{InMandaic}", "[\\x840-\\x85F]"},
         {"\\p{InMathematicalOperators}", "[\\x2200-\\x22FF]"},
         {"\\p{InMeeteiMayek}", "[\\xABC0-\\xABFF]"},
         {"\\p{InMeeteiMayekExtensions}", "[\\xAAE0-\\xAAFF]"},
         {"\\p{InMiscellaneousMathematicalSymbols-A}", "[\\x27C0-\\x27EF]"},
         {"\\p{InMiscellaneousMathematicalSymbols-B}", "[\\x2980-\\x29FF]"},
         {"\\p{InMiscellaneousSymbols}", "[\\x2600-\\x26FF]"},
         {"\\p{InMiscellaneousSymbolsandArrows}", "[\\x2B00-\\x2BFF]"},
         {"\\p{InMiscellaneousTechnical}", "[\\x2300-\\x23FF]"},
         {"\\p{InModifierToneLetters}", "[\\xA700-\\xA71F]"},
         {"\\p{InMongolian}", "[\\x1800-\\x18AF]"},
         {"\\p{InMyanmar}", "[\\x1000-\\x109F]"},
         {"\\p{InMyanmarExtended-A}", "[\\xAA60-\\xAA7F]"},
         {"\\p{InNewTaiLue}", "[\\x1980-\\x19DF]"}, {"\\p{InNKo}", "[\\x7C0-\\x7FF]"},
         {"\\p{InNumberForms}", "[\\x2150-\\x218F]"},
         {"\\p{InOgham}", "[\\x1680-\\x169F]"}, {"\\p{InOlChiki}", "[\\x1C50-\\x1C7F]"},
         {"\\p{InOpticalCharacterRecognition}", "[\\x2440-\\x245F]"},
         {"\\p{InOriya}", "[\\xB00-\\xB7F]"}, {"\\p{InPhags-pa}", "[\\xA840-\\xA87F]"},
         {"\\p{InPhoneticExtensions}", "[\\x1D00-\\x1D7F]"},
         {"\\p{InPhoneticExtensionsSupplement}", "[\\x1D80-\\x1DBF]"},
         {"\\p{InPrivateUseArea}", "[\\xE000-\\xF8FF]"},
         {"\\p{InRejang}", "[\\xA930-\\xA95F]"}, {"\\p{InRunic}", "[\\x16A0-\\x16FF]"},
         {"\\p{InSamaritan}", "[\\x800-\\x83F]"},
         {"\\p{InSaurashtra}", "[\\xA880-\\xA8DF]"},
         {"\\p{InSinhala}", "[\\xD80-\\xDFF]"},
         {"\\p{InSmallFormVariants}", "[\\xFE50-\\xFE6F]"},
         {"\\p{InSpacingModifierLetters}", "[\\x2B0-\\x2FF]"},
         {"\\p{InSpecials}", "[\\xFFF0-\\xFFFF]"},
         {"\\p{InSundanese}", "[\\x1B80-\\x1BBF]"},
         {"\\p{InSundaneseSupplement}", "[\\x1CC0-\\x1CCF]"},
         {"\\p{InSuperscriptsandSubscripts}", "[\\x2070-\\x209F]"},
         {"\\p{InSupplementalArrows-A}", "[\\x27F0-\\x27FF]"},
         {"\\p{InSupplementalArrows-B}", "[\\x2900-\\x297F]"},
         {"\\p{InSupplementalMathematicalOperators}", "[\\x2A00-\\x2AFF]"},
         {"\\p{InSupplementalPunctuation}", "[\\x2E00-\\x2E7F]"},
         {"\\p{InSylotiNagri}", "[\\xA800-\\xA82F]"},
         {"\\p{InSyriac}", "[\\x700-\\x74F]"}, {"\\p{InTagalog}", "[\\x1700-\\x171F]"},
         {"\\p{InTagbanwa}", "[\\x1760-\\x177F]"}, {"\\p{InTaiLe}", "[\\x1950-\\x197F]"},
         {"\\p{InTaiTham}", "[\\x1A20-\\x1AAF]"},
         {"\\p{InTaiViet}", "[\\xAA80-\\xAADF]"}, {"\\p{InTamil}", "[\\xB80-\\xBFF]"},
         {"\\p{InTelugu}", "[\\xC00-\\xC7F]"}, {"\\p{InThaana}", "[\\x780-\\x7BF]"},
         {"\\p{InThai}", "[\\xE00-\\xE7F]"}, {"\\p{InTibetan}", "[\\xF00-\\xFFF]"},
         {"\\p{InTifinagh}", "[\\x2D30-\\2D7Fx]"},
         {"\\p{InUnifiedCanadianAboriginalSyllabics}", "[\\x1400-\\x167F]"},
         {"\\p{InUnifiedCanadianAboriginalSyllabicsExtended}", "[\\x18B0-\\x18FF]"},
         {"\\p{InVai}", "[\\xA500-\\xA63F]"},
         {"\\p{InVariationSelectors}", "[\\xFE00-\\xFE0F]"},
         {"\\p{InVedicExtensions}", "[\\x1CD0-\\x1CFF]"},
         {"\\p{InVerticalForms}", "[\\xFE10-\\xFE1F]"},
         {"\\p{InYijingHexagramSymbols}", "[\\x4DC0-\\x4DFF]"},
         {"\\p{InYiRadicals}", "[\\xA490-\\xA4CF]"},
         {"\\p{InYiSyllables}", "[\\xA000-\\xA48F]"}],
    Tuple = lists:keyfind(BlockName, 1, UnicodeBlockList),
    Result =
        case Tuple of
            false ->
                nomatch;
            _ ->
                element(2, Tuple)
        end,
    Result.
