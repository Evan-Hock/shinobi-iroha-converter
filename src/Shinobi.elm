module Shinobi exposing
    ( ShinobiChar(..)
    , convert
    )


import Dict exposing (Dict)


import Kana


{-| Shinobi characters.
-}
type ShinobiChar
    =  I | Ro | Ha | Ni | Ho | He | To
    | Ti | Ri | Nu | Ru | Wo | Wa | Ka
    | Yo | Ta | Re | So | Tu | Ne | Na
    | Ra | Mu |  U | Wi | No |  O | Ku
    | Ya | Ma | Ke | Hu | Ko |  E | Te
    |  A | Sa | Ki | Yu | Me | Mi | Si
    | We | Hi | Mo | Se | Su |  N


{-| Converts a kana string into a list of shinobi characters.

Essentially does these transformations:
* Convert katakana to hiragana
* Remove dakuten
* Convert small characters into normal characters
-}
convert : String -> List ShinobiChar
convert =
    Kana.kataToHira
    >> Kana.removeDakuten
    >> Kana.normalizeSize
    >> String.toList
    >> List.filterMap convertChar


convertChar : Char -> Maybe ShinobiChar
convertChar char =
    Dict.get char convertCharTable


convertCharTable : Dict Char ShinobiChar
convertCharTable =
    Dict.fromList
        [ ( 'い',  I ), ( 'ろ', Ro ), ( 'は', Ha ), ( 'に', Ni ), ( 'ほ', Ho ), ( 'へ', He ), ( 'と', To )
        , ( 'ち', Ti ), ( 'り', Ri ), ( 'ぬ', Nu ), ( 'る', Ru ), ( 'を', Wo ), ( 'わ', Wa ), ( 'か', Ka )
        , ( 'よ', Yo ), ( 'た', Ta ), ( 'れ', Re ), ( 'そ', So ), ( 'つ', Tu ), ( 'ね', Ne ), ( 'な', Na )
        , ( 'ら', Ra ), ( 'む', Mu ), ( 'う',  U ), ( 'ゐ', Wi ), ( 'の', No ), ( 'お',  O ), ( 'く', Ku )
        , ( 'や', Ya ), ( 'ま', Ma ), ( 'け', Ke ), ( 'ふ', Hu ), ( 'こ', Ko ), ( 'え',  E ), ( 'て', Te )
        , ( 'あ',  A ), ( 'さ', Sa ), ( 'き', Ki ), ( 'ゆ', Yu ), ( 'め', Me ), ( 'み', Mi ), ( 'し', Si )
        , ( 'ゑ', We ), ( 'ひ', Hi ), ( 'も', Mo ), ( 'せ', Se ), ( 'す', Su ), ( 'ん',  N )
        ]