module UI.Collaborators.Modal exposing (..)

-- MODEL

import Ant.Icons.Svg as AntIcon
import Doc.List exposing (documentListChanged)
import Doc.Metadata as Metadata
import Html exposing (Html, br, div, hr, img, input, span, strong, text)
import Html.Attributes exposing (autofocus, class, id, placeholder, src, type_)
import Html.Events exposing (onClick, onInput)
import Json.Decode as Dec
import Svg.Attributes as SA exposing (height, width)
import Translation
import Utils


type alias Model =
    { isOwner : Bool
    , collabs : List String
    , newCollabField : String
    }


init : String -> List String -> Model
init myEmail collabs =
    { isOwner = not <| List.member myEmail collabs
    , collabs = collabs
    , newCollabField = ""
    }



-- UPDATE


type Msg
    = AddCollabFieldUpdated String
    | DocListChanged Dec.Value


update : String -> Msg -> Model -> Model
update docId msg model =
    case msg of
        AddCollabFieldUpdated newCollabField ->
            { model | newCollabField = newCollabField }

        DocListChanged value ->
            case Dec.decodeValue Metadata.listDecoder value of
                Ok docList ->
                    let
                        collabsNew =
                            docList
                                |> List.filter (\d -> docId == Metadata.getDocId d)
                                |> List.concatMap Metadata.getCollaborators
                    in
                    { model | collabs = collabsNew }

                Err _ ->
                    model



-- VIEW


view : { toSelf : Msg -> msg, addCollab : String -> msg, removeCollab : String -> msg } -> Translation.Language -> Model -> List (Html msg)
view msgs lang model =
    if model.isOwner then
        [ div [] (List.map (viewCollab msgs.removeCollab) model.collabs)
        , div [ class "flex-row", class "gap-2", class "mt-3" ]
            [ input
                [ id "add-collab-input"
                , placeholder (Translation.enKo lang "New collaborator's email" "새 공동 작업자 이메일")
                , class "w-2/3"
                , class "text-base"
                , class "px-2"
                , type_ "email"
                , autofocus True
                , onInput (msgs.toSelf << AddCollabFieldUpdated)
                ]
                []
            , div
                [ class "w-1/3"
                , class "py-2"
                , class "bg-blue-400"
                , class "rounded-md"
                , class "text-white"
                , class "flex"
                , class "items-center"
                , class "justify-center"
                , class "cursor-pointer"
                , onClick (msgs.addCollab model.newCollabField)
                ]
                [ text (Translation.enKo lang "Grant access" "권한 부여") ]
            ]
        , hr [ class "w-full" ] []
        , betaWarning lang
        ]

    else
        [ div [ class "pt-2", class "pb-5" ]
            [ text (Translation.enKo lang "You can edit this document, but you cannot manage who can access it." "이 문서를 편집할 수는 있지만, 접근 권한은 관리할 수 없습니다.")
            , br [] []
            , text (Translation.enKo lang "Contact the owner of the document if you need to add or remove collaborators." "공동 작업자를 추가하거나 제거하려면 문서 소유자에게 문의하십시오.")
            ]
        ]


viewCollab : (String -> msg) -> String -> Html msg
viewCollab removeMsg email =
    div [ class "flex", class "items-center", class "gap-2", class "mb-2" ]
        [ img [ src (Utils.avatar 22 email) ] []
        , span [ class "cursor-default" ] [ text email ]
        , AntIcon.closeCircleFilled
            [ width "16px"
            , height "16px"
            , SA.class "cursor-pointer"
            , onClick (removeMsg email)
            ]
        ]


betaWarning : Translation.Language -> Html msg
betaWarning lang =
    div
        [ class "bg-amber-100"
        , class "border-amber-400"
        , class "rounded-md"
        , class "flex"
        , class "gap-4"
        , class "p-4"
        , class "text-amber-900"
        , class "fill-orange-600"
        , class "items-center"
        , class "mt-6"
        , class "text-sm"
        ]
        [ AntIcon.warningFilled [ width "20px", height "20px" ]
        , div []
            [ text (Translation.enKo lang "Realtime Collaboration is " "실시간 공동 작업은 ")
            , strong [] [ text (Translation.enKo lang "currently in beta" "현재 베타 버전") ]
            , text (Translation.enKo lang "." "입니다.")
            , br [] []
            , text (Translation.enKo lang "Please back up your document regularly while testing this feature." "이 기능을 사용하는 동안 문서를 정기적으로 백업하십시오.")
            ]
        ]



-- SUBSCRIPTIONS


subscriptions : Sub Msg
subscriptions =
    documentListChanged DocListChanged
