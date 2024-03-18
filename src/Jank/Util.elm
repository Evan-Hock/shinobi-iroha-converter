module Jank.Util exposing
    ( groupPairs
    , stringToChar
    , cond
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


cond : List ( Bool, a ) -> a -> a
cond list dfl =
    case list of
        ( True, x )::_ ->
            x

        _::xs ->
            cond xs dfl

        [] ->
            dfl