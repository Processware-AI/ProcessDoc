port module Import.Bulk.UI exposing (Model, Msg, init, subscriptions, update, view)

import Doc.Data as Data
import Doc.Metadata as Metadata exposing (Metadata)
import File exposing (File)
import File.Select as Select
import GlobalData exposing (GlobalData)
import Html exposing (..)
import Html.Attributes exposing (checked, classList, disabled, for, height, href, id, src, style, target, type_, width)
import Html.Events exposing (on, onCheck, onClick)
import Import.Bulk
import Json.Decode as Dec
import Json.Encode as Enc
import Octicons as Icon exposing (defaultOptions)
import Outgoing exposing (Msg(..), send)
import Session exposing (LoggedIn)
import SharedUI exposing (modalWrapper)
import Task
import Time
import Translation exposing (Language)
import Types exposing (Tree)



-- MODEL


type alias Model =
    { state : ImportModalState
    , user : LoggedIn
    , globalData : GlobalData.GlobalData
    }


type ImportModalState
    = Closed
    | ModalOpen { loginState : LoginState, isFileDragging : Bool }
    | ImportSelecting ImportSelection
    | ImportSaving ImportSelection


type LoginState
    = Checking
    | LoggedIn
    | LoggedOut
    | Manual


type alias ImportSelection =
    List
        { selected : Bool
        , tree : ( String, Metadata, Tree )
        }


init : GlobalData -> LoggedIn -> Model
init globalData user =
    { state = ModalOpen { loginState = Checking, isFileDragging = False }, user = user, globalData = globalData }



-- UPDATE


type Msg
    = NoOp
    | ModalClosed
    | LegacyLoginStateChanged Bool
    | ManualChosen
    | Retry
    | FileRequested
    | FileDraggedOver Bool
    | FileSelected File
    | FileLoaded Int String
    | SelectAllToggled Bool
    | TreeSelected String Bool
    | SelectionDone
    | Completed


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({ state, user } as model) =
    case ( msg, state ) of
        ( ModalClosed, _ ) ->
            ( { model | state = Closed }, Cmd.none )

        ( LegacyLoginStateChanged isLoggedIn, _ ) ->
            let
                newState =
                    if isLoggedIn then
                        LoggedIn

                    else
                        LoggedOut
            in
            ( { model | state = ModalOpen { loginState = newState, isFileDragging = False } }, Cmd.none )

        ( ManualChosen, _ ) ->
            ( { model | state = ModalOpen { loginState = Manual, isFileDragging = False } }, Cmd.none )

        ( Retry, ModalOpen modalData ) ->
            ( { model | state = ModalOpen { modalData | loginState = Checking } }, Cmd.none )

        ( FileRequested, _ ) ->
            ( model, Select.file [ "text/*", "application/json" ] FileSelected )

        ( FileDraggedOver isDraggedOver, ModalOpen modalState ) ->
            ( { model | state = ModalOpen { modalState | isFileDragging = isDraggedOver } }, Cmd.none )

        ( FileSelected file, _ ) ->
            ( model, Task.perform (FileLoaded (GlobalData.currentTime model.globalData |> Time.posixToMillis)) (File.toString file) )

        ( FileLoaded currTime contents, ModalOpen _ ) ->
            case Dec.decodeString (Import.Bulk.decoder currTime) contents of
                Ok dataList ->
                    let
                        listWithSelectState =
                            dataList
                                |> List.sortBy (\( _, mdata, _ ) -> Metadata.getUpdatedAt mdata |> Time.posixToMillis)
                                |> List.reverse
                                |> List.map (\t -> { selected = False, tree = t })
                    in
                    ( { model | state = ImportSelecting listWithSelectState }, Cmd.none )

                Err _ ->
                    ( model, Cmd.none )

        ( SelectAllToggled selectAll, ImportSelecting selectList ) ->
            let
                mapFn item =
                    { item | selected = selectAll }
            in
            ( { model | state = ImportSelecting (selectList |> List.map mapFn) }, Cmd.none )

        ( TreeSelected treeId isSelected, ImportSelecting selectList ) ->
            let
                mapFn ({ selected, tree } as orig) =
                    let
                        ( tid, _, _ ) =
                            tree
                    in
                    if tid == treeId then
                        { orig | selected = isSelected }

                    else
                        orig

                newList =
                    selectList |> List.map mapFn
            in
            ( { model | state = ImportSelecting newList }, Cmd.none )

        ( SelectionDone, ImportSelecting selectList ) ->
            let
                author =
                    user |> Session.name

                treeInfoToCommitReq ( id, mdata, tree ) =
                    Data.requestCommit tree author Data.empty (Metadata.encode mdata)

                treesToSave =
                    selectList
                        |> List.filter .selected
                        |> List.map .tree
                        |> List.map treeInfoToCommitReq
                        |> List.filterMap identity
                        |> Enc.list identity
            in
            ( { model | state = ImportSaving selectList }, send <| SaveBulkImportedData treesToSave )

        ( Completed, ImportSaving _ ) ->
            ( { model | state = Closed }, Cmd.none )

        _ ->
            ( model, Cmd.none )



-- VIEW


view : Language -> Model -> List (Html Msg)
view lang { state } =
    let
        fileDropDecoder =
            Dec.map
                (\files ->
                    case List.head files of
                        Just file ->
                            FileSelected file

                        Nothing ->
                            NoOp
                )
                (Dec.field "dataTransfer" (Dec.field "files" (Dec.list File.decoder)))
    in
    case state of
        Closed ->
            [ text "" ]

        ModalOpen { loginState, isFileDragging } ->
            case loginState of
                Checking ->
                    [ text (Translation.enKo lang "Checking to see if you're logged in or not..." "로그인 여부를 확인하고 있습니다...")
                    , br [] []
                    , iframe [ src "https://gingkoapp.com/loggedin", width 0, height 0 ] []
                    , br [] []
                    , p []
                        [ h4 [] [ text (Translation.enKo lang "Taking too long?" "너무 오래 걸리나요?") ]
                        , text (Translation.enKo lang "Click " "v1 파일을 직접 다운로드하려면 ")
                        , button [ onClick ManualChosen ] [ text (Translation.enKo lang "here" "여기") ]
                        , text (Translation.enKo lang " to download your v1 files manually." "를 클릭하십시오.")
                        ]
                    ]
                        |> modalWrapper ModalClosed Nothing Nothing (Translation.enKo lang "Import From Gingko v1" "Gingko v1에서 가져오기")

                LoggedIn ->
                    [ p [] [ text (Translation.enKo lang "To transfer multiple trees from your old account to this new one, follow these steps." "이전 계정의 여러 트리를 새 계정으로 옮기려면 다음 단계를 따르십시오.") ]
                    , p []
                        [ text (Translation.enKo lang "1. Click here to download a backup of all your trees: " "1. 여기를 클릭하여 모든 트리의 백업을 다운로드합니다: ")
                        , br [] []
                        , a [ href "https://gingkoapp.com/export/all" ] [ text (Translation.enKo lang "Download Full Backup" "전체 백업 다운로드") ]
                        ]
                    , p []
                        [ text (Translation.enKo lang "2. Drag the backup file here:" "2. 백업 파일을 여기로 끌어다 놓습니다:")
                        , div
                            [ classList [ ( "file-drop-zone", True ), ( "dragged-over", isFileDragging ) ]
                            , on "dragenter" (Dec.succeed (FileDraggedOver True))
                            , on "dragleave" (Dec.succeed (FileDraggedOver False))
                            , on "drop" fileDropDecoder
                            ]
                            []
                        , text (Translation.enKo lang "or find the file in your system: " "또는 컴퓨터에서 파일을 찾습니다: ")
                        , button [ onClick FileRequested ] [ text (Translation.enKo lang "Browse..." "찾아보기...") ]
                        ]
                    ]
                        |> modalWrapper ModalClosed Nothing Nothing (Translation.enKo lang "Import From Gingko v1" "Gingko v1에서 가져오기")

                LoggedOut ->
                    [ p [] [ text (Translation.enKo lang "To transfer trees from your old account, you need to be logged in to it." "이전 계정의 트리를 옮기려면 해당 계정에 로그인되어 있어야 합니다.") ]
                    , p [] [ text (Translation.enKo lang "But it seems you are not logged in to your old account." "하지만 이전 계정에 로그인되어 있지 않은 것 같습니다.") ]
                    , p []
                        [ text "1. "
                        , a [ href "https://gingkoapp.com/login", target "_blank" ] [ text (Translation.enKo lang "Login there" "해당 사이트에서 로그인합니다") ]
                        , text "."
                        ]
                    , p []
                        [ text (Translation.enKo lang "2. Then, come back and " "2. 그런 다음 돌아와서 "), button [ id "retry-button", onClick Retry ] [ text (Translation.enKo lang "Try again" "다시 시도") ], text "." ]
                    , br [] []
                    , p []
                        [ h4 [] [ text (Translation.enKo lang "Having issues?" "문제가 있나요?") ]
                        , text (Translation.enKo lang "Click " "v1 파일을 직접 다운로드하려면 ")
                        , button [ onClick ManualChosen ] [ text (Translation.enKo lang "here" "여기") ]
                        , text (Translation.enKo lang " to download your v1 files manually." "를 클릭하십시오.")
                        ]
                    ]
                        |> modalWrapper ModalClosed Nothing Nothing (Translation.enKo lang "Import From Gingko v1" "Gingko v1에서 가져오기")

                Manual ->
                    [ p []
                        [ text "1. "
                        , a [ href "https://gingkoapp.com/login", target "_blank" ] [ text (Translation.enKo lang "Login" "로그인") ]
                        , text (Translation.enKo lang " to your old Gingko App account." "하여 이전 Gingko App 계정에 접속합니다.")
                        ]
                    , p []
                        [ text (Translation.enKo lang "2. Click on the Settings (" "2. 설정 ("), Icon.gear defaultOptions, text (Translation.enKo lang ") icon." ") 아이콘을 클릭합니다.") ]
                    , p []
                        [ text (Translation.enKo lang "3. Click 'Backup All Files'." "3. 'Backup All Files'를 클릭합니다.") ]
                    , p []
                        [ text (Translation.enKo lang "4. Drag the backup file here:" "4. 백업 파일을 여기로 끌어다 놓습니다:")
                        , div
                            [ classList [ ( "file-drop-zone", True ), ( "dragged-over", isFileDragging ) ]
                            , on "dragenter" (Dec.succeed (FileDraggedOver True))
                            , on "dragleave" (Dec.succeed (FileDraggedOver False))
                            , on "drop" fileDropDecoder
                            ]
                            []
                        , text (Translation.enKo lang "or find the file in your system: " "또는 컴퓨터에서 파일을 찾습니다: ")
                        , button [ onClick FileRequested ] [ text (Translation.enKo lang "Browse..." "찾아보기...") ]
                        ]
                    ]
                        |> modalWrapper ModalClosed Nothing Nothing (Translation.enKo lang "Import From Gingko v1" "Gingko v1에서 가져오기")

        ImportSelecting importSelection ->
            let
                isDisabled =
                    importSelection
                        |> List.any .selected
                        |> not
            in
            [ div [ style "display" "flex", style "margin-top" "10px" ] [ span [ style "flex" "auto" ] [ text (Translation.enKo lang "Name" "이름") ], span [] [ text (Translation.enKo lang "Last Modified" "마지막 수정") ] ]
            , div [ id "import-selection-list" ] [ ul [] (List.map (viewSelectionEntry lang) importSelection) ]
            , span []
                [ input [ id "import-select-all", type_ "checkbox", onCheck <| SelectAllToggled ] []
                , label [ for "import-select-all" ] [ text (Translation.enKo lang "Select All" "모두 선택") ]
                ]
            , button [ onClick SelectionDone, disabled isDisabled ] [ text (Translation.enKo lang "Import Selected Trees" "선택한 트리 가져오기") ]
            ]
                |> modalWrapper ModalClosed Nothing Nothing (Translation.enKo lang "Import From Gingko v1" "Gingko v1에서 가져오기")

        ImportSaving importSelection ->
            let
                importCount =
                    importSelection
                        |> List.filter .selected
                        |> List.length
            in
            [ p []
                [ text <| Translation.enKo lang ("Importing selected " ++ String.fromInt importCount ++ " trees...") ("선택한 트리 " ++ String.fromInt importCount ++ "개를 가져오는 중입니다...")
                , br [] []
                , text (Translation.enKo lang "This might take a while..." "시간이 다소 걸릴 수 있습니다...")
                ]
            ]
                |> modalWrapper ModalClosed Nothing Nothing (Translation.enKo lang "Import From Gingko v1" "Gingko v1에서 가져오기")


viewSelectionEntry : Language -> { selected : Bool, tree : ( String, Metadata, Tree ) } -> Html Msg
viewSelectionEntry lang { selected, tree } =
    let
        ( id, mdata, _ ) =
            tree
    in
    li []
        [ span []
            [ input [ type_ "checkbox", checked selected, onCheck (TreeSelected id) ] []
            , text (Metadata.getDocName mdata |> Maybe.withDefault (Translation.enKo lang "Untitled" "제목 없음"))
            ]
        , span [] [ text (Metadata.getUpdatedAt mdata |> Translation.dateFormat lang) ]
        ]



-- SUBSCRIPTIONS


port iframeLoginStateChange : (Bool -> msg) -> Sub msg


subscriptions : Model -> Sub Msg
subscriptions _ =
    iframeLoginStateChange LegacyLoginStateChanged
