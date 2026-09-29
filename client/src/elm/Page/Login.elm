module Page.Login exposing (Model, Msg, globalData, init, navKey, subscriptions, toSession, transition, update, view)

import Ant.Icons.Svg as AntIcons
import Browser.Dom
import Browser.Navigation as Nav
import GlobalData exposing (GlobalData)
import Html exposing (..)
import Html.Attributes exposing (autocomplete, autofocus, class, for, href, id, src, type_, value)
import Html.Events exposing (onClick, onInput, onSubmit)
import Html.Extra exposing (viewIf)
import Http exposing (Error(..))
import Result exposing (Result)
import Route
import Session exposing (Guest, LoggedIn, Session(..))
import Svg.Attributes
import Task
import Translation
import Utils exposing (getFieldErrors)
import Validate exposing (Valid, Validator, ifBlank, ifInvalidEmail, ifTrue, validate)



-- MODEL


type alias Model =
    { globalData : GlobalData
    , session : Guest
    , transition : Maybe LoggedIn
    , navKey : Nav.Key
    , email : String
    , password : String
    , showPassword : Bool
    , errors : List ( Field, String )
    }


type Field
    = Form
    | Email
    | Password


init : Nav.Key -> GlobalData -> Guest -> ( Model, Cmd msg )
init nKey gData session =
    ( { globalData = gData
      , session = session
      , transition = Nothing
      , navKey = nKey
      , email = ""
      , password = ""
      , showPassword = False
      , errors = []
      }
    , Cmd.none
    )


toSession : Model -> Session
toSession model =
    model.session |> GuestSession


transition : Model -> Maybe LoggedIn
transition model =
    model.transition


navKey : Model -> Nav.Key
navKey model =
    model.navKey


globalData : Model -> GlobalData
globalData model =
    model.globalData



-- UPDATE


type Msg
    = NoOp
    | SubmittedForm
    | EnteredEmail String
    | EnteredPassword String
    | ToggleShowPassword
    | CompletedLogin (Result Http.Error ( LoggedIn, Translation.Language ))
    | UserSaved


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        SubmittedForm ->
            case validate (modelValidator (GlobalData.language model.globalData)) model of
                Ok validModel ->
                    ( model
                    , sendLoginRequest validModel
                    )

                Err errs ->
                    ( { model | errors = errs }, Cmd.none )

        EnteredEmail email ->
            ( { model | email = email }, Cmd.none )

        EnteredPassword password ->
            ( { model | password = password }, Cmd.none )

        ToggleShowPassword ->
            ( { model | showPassword = not model.showPassword }, Task.attempt (\_ -> NoOp) (Browser.Dom.focus "password-input") )

        CompletedLogin (Ok ( user, lang )) ->
            ( { model | transition = Just user, globalData = GlobalData.setLanguage lang model.globalData }, Session.storeLogin lang user )

        CompletedLogin (Err error) ->
            let
                lang =
                    GlobalData.language model.globalData

                fallbackMsg =
                    ( Form, Translation.enKo lang "Server Issue. Something wrong on our end. Please let us know!" "서버 문제가 발생했습니다. 저희 쪽에 문제가 있는 것 같습니다. 알려 주세요!" )

                errorMsg =
                    case error of
                        Timeout ->
                            ( Form, Translation.enKo lang "Timed out. Maybe there's a server issue?" "시간이 초과되었습니다. 서버에 문제가 있는 것 같습니다." )

                        NetworkError ->
                            ( Form, Translation.enKo lang "Network Error. Maybe you're offline?" "네트워크 오류입니다. 오프라인 상태인지 확인해 주세요." )

                        BadStatus statusCode ->
                            case statusCode of
                                401 ->
                                    ( Form, Translation.enKo lang "Email or Password was incorrect.\n\nNOTE: that this is separate from existing gingkoapp.com accounts.\n\n" "이메일 또는 비밀번호가 올바르지 않습니다.\n\n참고: 기존 gingkoapp.com 계정과는 별개입니다.\n\n" )

                                _ ->
                                    fallbackMsg

                        _ ->
                            fallbackMsg
            in
            ( { model | errors = [ errorMsg ], password = "" }, Cmd.none )

        UserSaved ->
            ( model, Route.pushUrl model.navKey Route.Root )


modelValidator : Translation.Language -> Validator ( Field, String ) Model
modelValidator lang =
    Validate.all
        [ Validate.firstError
            [ ifBlank .email ( Email, Translation.enKo lang "Please enter an email address." "이메일 주소를 입력해 주세요." )
            , ifInvalidEmail .email (\_ -> ( Email, Translation.enKo lang "This does not seem to be a valid email." "올바른 이메일 주소가 아닌 것 같습니다." ))
            ]
        , ifTrue (\model -> String.length model.password < 7) ( Password, Translation.enKo lang "Password should be 7 characters or more." "비밀번호는 7자 이상이어야 합니다." )
        , ifBlank .password ( Password, Translation.enKo lang "Please enter a password." "비밀번호를 입력해 주세요." )
        ]


sendLoginRequest : Valid Model -> Cmd Msg
sendLoginRequest validModel =
    let
        { email, password, session } =
            Validate.fromValid validModel
    in
    Session.requestLogin CompletedLogin email password session



-- VIEW


view : Model -> Html Msg
view model =
    let
        lang =
            GlobalData.language model.globalData

        formErrors =
            getFieldErrors Form model.errors

        emailErrors =
            getFieldErrors Email model.errors

        passwordErrors =
            getFieldErrors Password model.errors

        fromLegacy =
            Session.fromLegacy model.session

        showHidePassword =
            if model.showPassword then
                div [ id "show-hide-password", onClick ToggleShowPassword ] [ AntIcons.eyeInvisibleOutlined [ Svg.Attributes.class "icon" ], text (Translation.enKo lang "Hide" "숨기기") ]

            else
                div [ id "show-hide-password", onClick ToggleShowPassword ] [ AntIcons.eyeOutlined [ Svg.Attributes.class "icon" ], text (Translation.enKo lang "Show" "보기") ]
    in
    div [ id "form-page" ]
        [ div [ class "page-backdrop" ] []
        , div [ class "page-bg" ] []
        , a [ class "brand", href "{%HOMEPAGE_URL%}" ] [ img [ id "logo", src "gingko-leaf-logo.svg" ] [] ]
        , div [ class "form-header-container" ]
            [ h1 [ class "headline" ] [ text (Translation.enKo lang "Login" "로그인") ]
            , p [ class "subtitle" ] [ text (Translation.enKo lang "Welcome back!" "다시 오신 것을 환영합니다!") ]
            ]
        , div [ class "center-form" ]
            [ form [ onSubmit SubmittedForm ]
                [ label [ for "email-input" ] [ text (Translation.enKo lang "Email" "이메일") ]
                , input
                    [ onInput EnteredEmail
                    , id "email-input"
                    , type_ "email"
                    , value model.email
                    , autofocus True
                    , autocomplete True
                    ]
                    []
                , viewIf (not <| List.isEmpty emailErrors) <| div [ class "input-errors" ] [ text (String.join "\n" emailErrors) ]
                , label [ for "password-input" ] [ text (Translation.enKo lang "Password" "비밀번호"), showHidePassword ]
                , input
                    [ onInput EnteredPassword
                    , id "password-input"
                    , type_
                        (if model.showPassword then
                            "text"

                         else
                            "password"
                        )
                    , value model.password
                    , autocomplete True
                    ]
                    []
                , viewIf (not <| List.isEmpty passwordErrors) <| div [ class "input-errors" ] [ text (String.join "\n" passwordErrors) ]
                , if List.length formErrors > 0 then
                    div [ id "form-errors" ] [ text (String.join "\n" formErrors) ]

                  else
                    text ""
                , button [ id "login-button", class "cta" ] [ text (Translation.enKo lang "Login" "로그인") ]
                , div [ id "post-cta-divider" ] [ hr [] [], div [] [ text (Translation.enKo lang "or" "또는") ], hr [] [] ]
                , span [ class "alt-action" ]
                    [ text (Translation.enKo lang "New to Gingko? " "Gingko가 처음이신가요? ")
                    , a [ href "/signup" ] [ text (Translation.enKo lang "Signup" "회원가입") ]
                    , br [] []
                    , br [] []
                    , a [ class "forgot-password", href "/forgot-password" ] [ text (Translation.enKo lang "Forgot your Password?" "비밀번호를 잊으셨나요?") ]
                    ]
                , br [] []
                , if fromLegacy then
                    small [ class "extra-info", class "legacy" ]
                        [ text (Translation.enKo lang "This is " "이 계정은 gingkoapp.com 계정과 ")
                        , strong [] [ text (Translation.enKo lang "not connected" "연결되어 있지 않습니다") ]
                        , text (Translation.enKo lang " to your gingkoapp.com account." ".")
                        , br [] []
                        , br [] []
                        , a [ href <| Route.toString Route.Signup ] [ text (Translation.enKo lang "Signup here" "여기에서 회원가입") ]
                        , text (Translation.enKo lang " to get started." "하고 시작해 보세요.")
                        ]

                  else
                    text ""
                ]
            ]
        ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Session.userLoggedIn UserSaved
