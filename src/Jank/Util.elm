module Jank.Util exposing
    ( groupPairs
    , stringToChar
    )


groupPairs : List a -> List ( a, a )
groupPairs list =
    case list of
        x::y::xs ->
            ( x, y ) :: groupPairs xs

        _ ->
            []


stringToChar : String -> Maybe Char
stringToChar =
    String.uncons >> Maybe.map Tuple.first