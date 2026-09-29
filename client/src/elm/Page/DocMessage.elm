module Page.DocMessage exposing (..)

import Ant.Icons.Svg as AntIcons
import Html exposing (Html, br, div, h1, img, p, text)
import Html.Attributes exposing (class, id, src)
import Html.Events exposing (on, onClick)
import Json.Decode as Dec
import Translation exposing (Language(..))



-- VIEW


viewEmptyIn : Language -> { newClicked : msg, emptyMessage : msg } -> List (Html msg)
viewEmptyIn lang msgs =
    [ div [ id "document-header" ] []
    , div [ id "empty-message" ]
        [ h1 [] [ text (Translation.enKo lang "You don't have any documents" "문서가 없습니다") ]
        , p [] [ text (Translation.enKo lang "Click to create one:" "클릭하여 새 문서를 만드세요:") ]
        , br [] []
        , div [ id "new-button", onClick msgs.newClicked ] [ AntIcons.fileAddOutlined [] ]
        , img [ src "", on "error" (Dec.succeed msgs.emptyMessage) ] []
        ]
    ]


viewNotFoundIn : Language -> msg -> List (Html msg)
viewNotFoundIn lang _ =
    [ div [ id "document-header" ] []
    , div [ id "doc-error-message" ]
        [ h1 [] [ text (Translation.enKo lang "Hmm, we couldn't find this document" "이 문서를 찾을 수 없습니다") ]
        , p [] [ text (Translation.enKo lang "The file might have been moved, or deleted." "파일이 이동되었거나 삭제되었을 수 있습니다.") ]
        ]
    ]
