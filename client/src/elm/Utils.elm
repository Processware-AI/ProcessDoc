module Utils exposing (avatar, delay, emptyText, getFieldErrors, hash, hexEncode, myDebug, onClickStop, onClickStopStyled, ternary, text, textElmCss, textNoTr, voxEmporiumHash)

{--import DebugToJson exposing (pp)--}

import Hex
import Html exposing (Html)
import Html.Events exposing (stopPropagationOn)
import Html.Styled
import Html.Styled.Events
import Json.Decode as Dec
import Murmur3 exposing (hashString)
import Process
import SHA256
import Task
import Translation exposing (Language, TranslationId, tr)
import Url


ternary : Bool -> a -> a -> a
ternary condition trueValue falseValue =
    if condition then
        trueValue

    else
        falseValue


onClickStop : msg -> Html.Attribute msg
onClickStop msg =
    stopPropagationOn "click" (Dec.succeed ( msg, True ))


onClickStopStyled : msg -> Html.Styled.Attribute msg
onClickStopStyled msg =
    Html.Styled.Events.stopPropagationOn "click" (Dec.succeed ( msg, True ))


hexEncode : String -> String
hexEncode input =
    input
        |> String.toList
        |> List.map Char.toCode
        |> List.map Hex.toString
        |> String.join ""


hash : Int -> String -> String
hash seed str =
    hashString seed str
        |> Hex.toString


voxEmporiumHash : String -> String
voxEmporiumHash rawEmail =
    (rawEmail ++ "{%VOX_EMPORIUM_SALT%}")
        |> String.toLower
        |> String.trim
        |> SHA256.fromString
        |> SHA256.toHex
        |> String.left 16


delay : Int -> msg -> Cmd msg
delay ms msg =
    Task.perform (always msg) (Process.sleep <| toFloat ms)


{-| Locally generated avatar (no external request): the email's first letter
on a circle whose color is derived from the email. Returns a data: URI for `img src`.
-}
avatar : Int -> String -> String
avatar size rawEmail =
    let
        email =
            rawEmail |> String.trim |> String.toLower

        initial =
            case String.uncons email of
                Just ( c, _ ) ->
                    if Char.isAlphaNum c then
                        String.fromChar (Char.toUpper c)

                    else
                        "?"

                Nothing ->
                    "?"

        hue =
            hashString 0 email |> modBy 360 |> String.fromInt

        sizeStr =
            String.fromInt size

        svg =
            "<svg xmlns='http://www.w3.org/2000/svg' width='"
                ++ sizeStr
                ++ "' height='"
                ++ sizeStr
                ++ "' viewBox='0 0 100 100'><circle cx='50' cy='50' r='50' fill='hsl("
                ++ hue
                ++ ",45%,50%)'/><text x='50' y='50' dy='.35em' text-anchor='middle' font-family='sans-serif' font-size='52' fill='white'>"
                ++ initial
                ++ "</text></svg>"
    in
    "data:image/svg+xml," ++ Url.percentEncode svg



-- Debugging


myDebug : String -> a -> a
myDebug label value =
    let
        _ =
            --Debug.log label (pp (Debug.toString value))
            ()
    in
    value



-- Translation Helper Function


text : Language -> TranslationId -> Html msg
text lang tid =
    Html.text <| tr lang tid


textElmCss : Language -> TranslationId -> Html.Styled.Html msg
textElmCss lang tid =
    Html.Styled.text <| tr lang tid


textNoTr : String -> Html msg
textNoTr str =
    Html.text str


emptyText : Html msg
emptyText =
    Html.text ""


getFieldErrors : field -> List ( field, a ) -> List a
getFieldErrors field errs =
    errs
        |> List.filter ((==) field << Tuple.first)
        |> List.map Tuple.second
