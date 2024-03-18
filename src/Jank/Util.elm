module Jank.Util exposing
    ( groupPairs
    , stringToChar
    , cond
    )


{-| Groups adjacent elements into a list of pairs.

```elm
groupPairs [1, 2, 3, 4] == [( 1, 2 ), ( 3, 4 )]
groupPairs [1, 2, 3] == [( 1, 2 )]
```
-}
groupPairs : List a -> List ( a, a )
groupPairs list =
    case list of
        x::y::xs ->
            ( x, y ) :: groupPairs xs

        _ ->
            []


{-| Converts a String into a Char.

Simply yields the first letter wrapped in Just.

Returns Nothing if the conversion is not possible, i.e. the String is empty.
-}
stringToChar : String -> Maybe Char
stringToChar =
    String.uncons >> Maybe.map Tuple.first


{-| Reimplementation of LISP's `cond` special form.
-}
cond : List ( Bool, a ) -> a -> a
cond list dfl =
    case list of
        ( True, x )::_ ->
            x

        _::xs ->
            cond xs dfl

        [] ->
            dfl