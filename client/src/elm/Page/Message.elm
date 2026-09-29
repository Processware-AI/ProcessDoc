module Page.Message exposing (..)

import Html exposing (Html, a, br, div, h1, p, text)
import Html.Attributes exposing (class, href, id)
import Route
import Translation exposing (Language)


viewSuccess : Language -> { title : String, body : List (Html msg) }
viewSuccess lang =
    { title = Translation.enKo lang "Payment Successful" "결제 완료"
    , body =
        [ div [ id "message-page" ]
            [ div [ class "message-card" ]
                [ h1 [] [ text (Translation.enKo lang "Thank you for your payment" "결제해 주셔서 감사합니다") ]
                , p [] [ text (Translation.enKo lang "Your support is greatly appreciated 😄." "보내 주신 성원에 진심으로 감사드립니다 😄.") ]
                , br [] []
                , a [ href (Route.toString Route.Root), class "message-cta" ] [ text (Translation.enKo lang "Continue" "계속") ]
                ]
            ]
        ]
    }
