module Main exposing (main)


import Browser
import Html exposing (Html)
import Html.Attributes as Attr
import Html.Events as Event
import Html.Keyed as Keyed
import Html.Lazy exposing (lazy2)


import Jank.Util


import Shinobi exposing (ShinobiChar(..))
import Romaji


type alias Model =
    { mode: ConversionMode
    , input: String
    , output: List ShinobiChar
    , outputMode : OutputMode
    }


type ConversionMode
    = Kana
    | Romaji


type OutputMode
    = Text
    | Images
    | RomajiOutput


type Msg
    = UpdateInput String
    | Convert
    | SwitchMode ConversionMode
    | SwitchOutputMode OutputMode


-- MAIN

main : Program () Model Msg
main =
    Browser.sandbox
        { init = init
        , update = update
        , view = view
        }


-- INIT

init : Model
init =
    { mode = Romaji
    , input = ""
    , output = []
    , outputMode = Text
    }


-- VIEW

view : Model -> Html Msg
view model =
    Html.main_ []
        [ Html.h1 [] [ Html.text "Shinobi Iroha" ]
        , conversionInput model
        , lazy2 conversionOutput model.outputMode model.output
        ]


conversionInput : Model -> Html Msg
conversionInput model =
    Html.form [ Event.onSubmit Convert ]
        [ Html.input [ Attr.value model.input, Event.onInput UpdateInput ] []
        , [ ( "Rōmaji", Romaji ), ( "Kana", Kana ) ]
            |> modeSelectorList model
                { toMsg = SwitchMode
                , fromModel = .mode
                }
        , [ ( "Unicode", Text ), ( "Images", Images ), ( "Rōmaji", RomajiOutput ) ]
            |> modeSelectorList model
                { toMsg = SwitchOutputMode
                , fromModel = .outputMode
                }
        ]


type alias ModeSelectorOpts mode =
    { toMsg : mode -> Msg
    , fromModel : Model -> mode
    }
        

modeSelectorList : Model -> ModeSelectorOpts mode -> List ( String, mode ) -> Html Msg
modeSelectorList model opts =
    List.map (modeSelector model opts)
    >> Keyed.ul [ Attr.class "mode-selector" ]


modeSelector : Model -> ModeSelectorOpts mode -> ( String, mode ) -> ( String, Html Msg )
modeSelector model opts ( label, mode ) =
    let
        conversionButton =
            Html.li []
                [ Html.button
                    [ Attr.type_ "button"
                    , Event.onClick (opts.toMsg mode)
                    , Attr.classList [ ( "selected", opts.fromModel model == mode ) ]
                    ] [ Html.text label ]
                ]
    in
        ( label, conversionButton )


conversionOutput : OutputMode -> List ShinobiChar -> Html Msg
conversionOutput mode =
    let
        outputFunc =
            case mode of
                Text ->
                    charToDigraph

                Images ->
                    charToImg

                RomajiOutput ->
                    charToRomaji

   in
        List.indexedMap outputFunc
        >> Keyed.ul [ Attr.class "output" ]


charToRomaji : Int -> ShinobiChar -> ( String, Html Msg )
charToRomaji i char =
    ( String.fromInt i, Html.li [] [ Html.text (toRomaji char) ] )


charToDigraph : Int -> ShinobiChar -> ( String, Html Msg )
charToDigraph i char =
    let
        digraph =
            Html.li [] [ Html.text (getHen char ++ getTukuri char) ]
    in
        ( String.fromInt i, digraph )


getTukuri : ShinobiChar -> String
getTukuri char =
    let
        in_ =
            List.member char
    in
        Jank.Util.cond
            [ ( in_ [  I, Ro, Ha, Ni, Ho, He, To ], "色" )
            , ( in_ [ Ti, Ri, Nu, Ru, Wo, Wa, Ka ], "青" )
            , ( in_ [ Yo, Ta, Re, So, Tu, Ne, Na ], "黄" )
            , ( in_ [ Ra, Mu,  U, Wi, No,  O, Ku ], "赤" )
            , ( in_ [ Ya, Ma, Ke, Hu, Ko,  E, Te ], "白" )
            , ( in_ [  A, Sa, Ki, Yu, Me, Mi, Si ], "黒" )
            ] "紫"


getHen : ShinobiChar -> String
getHen char =
    let
        in_ =
            List.member char
    in
        Jank.Util.cond
            [ ( in_ [  I, Ti, Yo, Ra, Ya,  A, We ], "木" )
            , ( in_ [ Ro, Ri, Ta, Mu, Ma, Sa, Hi ], "火" )
            , ( in_ [ Ha, Nu, Re,  U, Ke, Ki, Mo ], "土" )
            , ( in_ [ Ni, Ru, So, Wi, Hu, Yu, Se ], "金" )
            , ( in_ [ Ho, Wo, Tu, No, Ko, Me, Su ], "水" )
            , ( in_ [ He, Wa, Ne,  O,  E, Mi,  N ], "人" )
            ] "身"



charToImg : Int -> ShinobiChar -> ( String, Html Msg )
charToImg i char =
    let
        id =
            toRomaji char
        li =
            Html.li []
                [ Html.img
                    [ Attr.src (shinobiImgUrl id)
                    , Attr.alt id
                    ] []
                ]
    in
        ( String.fromInt i, li )


shinobiImgUrl : String -> String
shinobiImgUrl uniqId =
    "/shinobi_chars/" ++ uniqId ++ ".svg"


toRomaji : ShinobiChar -> String
toRomaji char =
    case char of
        I ->
            "i"
        
        Ro ->
            "ro"

        Ha ->
            "ha"

        Ni ->
            "ni"
            
        Ho ->
            "ho"

        He ->
            "he"

        To ->
            "to"

        Ti ->
            "ti"

        Ri ->
            "ri"

        Nu ->
            "nu"

        Ru ->
            "ru"

        Wo ->
            "wo"

        Wa ->
            "wa"

        Ka ->
            "ka"

        Yo ->
            "yo"
        
        Ta ->
            "ta"

        Re ->
            "re"

        So ->
            "so"

        Tu ->
            "tu"

        Ne ->
            "ne"

        Na ->
            "na"

        Ra ->
            "ra"

        Mu ->
            "mu"

        U ->
            "u"

        Wi ->
            "wi"
        
        No ->
            "no"

        O ->
            "o"

        Ku ->
            "ku"

        Ya ->
            "ya"

        Ma ->
            "ma"

        Ke ->
            "ke"

        Hu ->
            "hu"

        Ko ->
            "ko"

        E ->
            "e"

        Te ->
           "te"

        A ->
            "a"

        Sa ->
            "sa"

        Ki ->
            "ki"

        Yu ->
            "yu"

        Me ->
            "me"

        Mi ->
            "mi"
        
        Si ->
            "si"

        We ->
            "we"

        Hi ->
            "hi"

        Mo ->
            "mo"

        Se ->
            "se"

        Su ->
            "su"

        N ->
            "n"
        


-- UPDATE

update : Msg -> Model -> Model
update msg model =
    case msg of
        UpdateInput newInput ->
            { model | input = newInput }

        SwitchMode newMode ->
            { model | mode = newMode }

        SwitchOutputMode newOutputMode ->
            { model | outputMode = newOutputMode }

        Convert ->
            let
                convert =
                    case model.mode of
                        Romaji ->
                            Romaji.toKana >> Shinobi.convert

                        Kana ->
                            Shinobi.convert
            in
                { model | output = convert model.input }