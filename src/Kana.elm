module Kana exposing
    ( removeDakuten
    , kataToHira
    , normalizeSize
    )


import Dict exposing (Dict)


import Jank.Util


removeDakuten : String -> String
removeDakuten =
    String.map removeDakutenChar


removeDakutenChar : Char -> Char
removeDakutenChar char =
    Dict.get char removeDakutenTable
    |> Maybe.withDefault char


removeDakutenTable : Dict Char Char
removeDakutenTable =
    String.words"""
    が か ぎ き ぐ く げ け ご こ 
    ざ さ じ し ず す ぜ せ ぞ そ 
    だ た ぢ ち づ つ で て ど と 
    ば は び ひ ぶ ふ べ へ ぼ ほ 
    ぱ は ぴ ひ ぷ ふ ぺ へ ぽ ほ 
    ガ カ ギ キ グ ク ゲ ケ ゴ コ 
    ザ サ ジ シ ズ ス ゼ セ ゾ ソ 
    ダ タ ヂ チ ヅ ツ デ テ ド ト 
    バ ハ ビ ヒ ブ フ ベ ヘ ボ ホ 
    パ ハ ピ ヒ プ フ ペ ヘ ポ ホ 
    """
    |> List.filterMap Jank.Util.stringToChar
    |> Jank.Util.groupPairs
    |> Dict.fromList


kataToHira : String -> String
kataToHira =
    String.map kataToHiraChar


kataToHiraChar : Char -> Char
kataToHiraChar char =
    Dict.get char kataToHiraTable
    |> Maybe.withDefault char


kata : List Char
kata =
    String.words"""
    ア イ ウ エ オ 
    カ キ ク ケ コ 
    ガ ギ グ ゲ ゴ 
    サ シ ス セ ソ 
    ザ ジ ズ ゼ ゾ 
    タ チ ツ テ ト 
    ダ ヂ ヅ デ ド 
    ナ ニ ヌ ネ ノ
    ハ ヒ フ ヘ ホ 
    バ ビ ブ ベ ボ 
    パ ピ プ ペ ポ 
    マ ミ ム メ モ 
    ヤ ユ ヨ
    ラ リ ル レ ロ 
    ワ ヰ ヱ ヲ ン 

    ァ ィ ゥ ェ ォ
    ッ ャ ュ ョ
    """
    |> List.filterMap Jank.Util.stringToChar


hira : List Char
hira =
    String.words"""
    あ い う え お 
    か き く け こ 
    が ぎ ぐ げ ご 
    さ し す せ そ 
    ざ じ ず ぜ ぞ 
    た ち つ て と 
    だ ち づ で ど 
    な に ぬ ね の 
    は ひ ふ へ ほ 
    ば び ぶ べ ぼ 
    ぱ ぴ ぷ ぺ ぽ 
    ま み む め も 
    や ゆ よ 
    ら り る れ ろ 
    わ ゐ ゑ を ん 

    ぁ ぃ ぅ ぇ ぉ 
    っ ゃ ゅ ょ 
    """
    |> List.filterMap Jank.Util.stringToChar


kataToHiraTable : Dict Char Char
kataToHiraTable =
    Dict.fromList
        <| List.map2 Tuple.pair
            kata
            hira


normalizeSize : String -> String
normalizeSize =
    String.map normalizeSizeChar


normalizeSizeChar : Char -> Char
normalizeSizeChar char =
    Dict.get char normalizeSizeTable
    |> Maybe.withDefault char


normalizeSizeTable : Dict Char Char
normalizeSizeTable =
    String.words"""
    ぁ あ ぃ い ぅ う ぇ え ぉ お 
    っ つ ゃ や ゅ ゆ ょ よ 
    ァ ア ィ イ ゥ ウ ェ エ ォ オ 
    ッ ツ ャ ヤ ュ ユ ョ ヨ 
    """
    |> List.filterMap Jank.Util.stringToChar
    |> Jank.Util.groupPairs
    |> Dict.fromList