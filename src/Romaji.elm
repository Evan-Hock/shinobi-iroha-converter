module Romaji exposing (toKana)


import Dict exposing (Dict)
import Parser exposing (Parser, (|=), (|.), Step(..))


import Jank.Util


syllablePairings : List ( String, String )
syllablePairings =
    String.words"""
     a あ  i い u  う  e え  o お
    ka か ki き ku く ke け ko こ
    ga が gi ぎ gu ぐ ge げ go ご
    sa さ si し su す se せ so そ shi し
    za ざ zi じ zu ず ze ぜ zo ぞ  ji じ
    ta た ti ち tu つ te て to と chi ち tsu つ
    da だ di ぢ du づ de で do ど
    na な ni に nu ぬ ne ね no の
    ha は hi ひ hu ふ he へ ho ほ fu ふ
    ba ば bi び bu ぶ be べ bo ぼ
    pa ぱ pi ぴ pu ぷ pe ぺ po ぽ
    ma ま mi み mu む me め mo も
    ya や yu ゆ yo よ
    ra ら ri り ru る re れ ro ろ
    wa わ wi ゐ we ゑ wo を  n ん

    kya きゃ kyu きゅ kyo きょ
    gya ぎゃ gyu ぎゅ gyo ぎょ
    sya しゃ syu しゅ syo しょ sha しゃ shu しゅ sho しょ
    zya じゃ zyu じゅ zyo じょ  ja じゃ  ju じゅ  jo じょ
    tya ちゃ tyu ちゅ tyo ちょ cha ちゃ chu ちゅ cho ちょ
    dya ぢゃ dyu ぢゅ dyo ぢょ
    nya にゃ nyu にゅ nyo にょ
    hya ひゃ hyu ひゅ hyo ひょ
    bya びゃ byu びゅ byo びょ
    pya ぴゃ pyu ぴゅ pyo ぴょ
    mya みゃ myu みゅ myo みょ
    rya りゃ ryu りゅ ryo りょ
    """ 
    |> Jank.Util.groupPairs


syllables : List String
syllables =
    List.unzip syllablePairings
    |> Tuple.first
    |> List.sortBy (String.length >> negate)


romajiToKanaMapping : Dict String String
romajiToKanaMapping =
    Dict.fromList syllablePairings


parseSyllable : Parser String
parseSyllable =
    Parser.getChompedString
        <| Parser.oneOf
            <| List.map Parser.token syllables


parseSyllables : Parser (List String)
parseSyllables =
    Parser.loop [] parseSyllablesHelp


parseSyllablesHelp : List String -> Parser (Step (List String) (List String))
parseSyllablesHelp acc =
    Parser.oneOf
        [ parseSyllable
            |> Parser.map ( \ syl -> Loop (syl :: acc ) )
        , Parser.end
            |> Parser.map ( \ _ -> Done (List.reverse acc) ) 
        , Parser.succeed (Loop acc)
            |. Parser.chompIf ( \ _ -> True )
        ]


{-| Extremely permissive rōmaji converter that really only works
for the purposes of this project. Ignores any character combination
that is not in the Japanese syllabary. Not case-sensitive. Does
not preserve whitespace or punctuation.
-}
toKana : String -> String
toKana romaji =
    case Parser.run parseSyllables (String.toLower romaji) of
        -- NB: Should never happen. The parser is very permissive
        Err _ ->
            ""
        
        Ok syls ->
            syls
            |> List.filterMap ( \ syl -> Dict.get syl romajiToKanaMapping )
            |> String.concat