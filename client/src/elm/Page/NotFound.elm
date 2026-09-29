module Page.NotFound exposing (view)

import Html exposing (Html, div, text)
import Translation exposing (Language)


view : Language -> { title : String, body : List (Html msg) }
view lang =
    { title = Translation.enKo lang "Not Found" "찾을 수 없음"
    , body = [ div [] [ text (Translation.enKo lang "Not Found" "페이지를 찾을 수 없습니다") ] ]
    }
