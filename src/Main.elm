module Main exposing (main)


import Browser
import Html exposing (Html)
import Html.Attributes as Attr
import Html.Events as Event
import Html.Keyed as Keyed
import Html.Lazy exposing (lazy)


import Shinobi exposing (ShinobiChar(..))
import Romaji


type alias Model =
    { mode: ConversionMode
    , input: String
    , output: List ShinobiChar
    }


type ConversionMode
    = Kana
    | Romaji


type Msg
    = UpdateInput String
    | Convert
    | SwitchMode ConversionMode


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
    { mode = Kana
    , input = ""
    , output = []
    }


-- VIEW

view : Model -> Html Msg
view model =
    Html.main_ []
        [ Html.h1 [] [ Html.text "Shinobi Iroha" ]
        , conversionInput model
        , lazy conversionOutput model.output
        ]


conversionInput : Model -> Html Msg
conversionInput model =
    Html.form [ Event.onSubmit Convert ]
        [ Html.input [ Attr.value model.input, Event.onInput UpdateInput ] []
        , [ ( "Kana", Kana ), ( "Rōmaji", Romaji ) ]
            |> List.map (conversionModeSelector model)
            |> Keyed.ul [ Attr.class "mode-selector" ]
        ]


conversionModeSelector : Model -> ( String, ConversionMode ) -> ( String, Html Msg )
conversionModeSelector model ( label, mode ) =
    let
        conversionButton =
            Html.li []
                [ Html.button
                    [ Attr.type_ "button"
                    , Event.onClick (SwitchMode mode)
                    , Attr.classList [ ( "selected", model.mode == mode ) ]
                    ] [ Html.text label ]
                ]
    in
        ( label, conversionButton )


conversionOutput : List ShinobiChar -> Html Msg
conversionOutput =
    List.indexedMap charToImg
    >> Keyed.ul []


charToImg : Int -> ShinobiChar -> ( String, Html Msg )
charToImg i char =
    let
        id =
            charToImgUrlUniqueId char
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


charToImgUrlUniqueId : ShinobiChar -> String
charToImgUrlUniqueId char =
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