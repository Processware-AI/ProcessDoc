module Page.ResetPassword exposing (Model, Msg, globalData, init, navKey, subscriptions, toSession, transition, update, view)

import Browser.Dom
import Browser.Navigation as Nav
import GlobalData exposing (GlobalData)
import Html exposing (..)
import Html.Attributes exposing (autofocus, class, classList, href, id, placeholder, src, type_, value)
import Html.Events exposing (onBlur, onInput, onSubmit)
import Http exposing (Error(..))
import Session exposing (Guest, LoggedIn, Session(..))
import Task
import Translation exposing (Language)
import Utils exposing (getFieldErrors)
import Validate exposing (Valid, Validator, ifBlank, ifFalse, ifInvalidEmail, ifTrue, validate)



-- MODEL


type alias Model =
    { globalData : GlobalData
    , session : Guest
    , transition : Maybe LoggedIn
    , navKey : Nav.Key
    , password : String
    , passwordConfirm : String
    , resetToken : String
    , errors : List ( Field, String )
    }


type Field
    = Form
    | Password
    | PasswordConfirm


init : Nav.Key -> GlobalData -> Guest -> String -> ( Model, Cmd Msg )
init nKey gData session resetToken =
    ( { globalData = gData
      , session = session
      , transition = Nothing
      , navKey = nKey
      , resetToken = resetToken
      , password = ""
      , passwordConfirm = ""
      , errors = []
      }
    , Task.attempt (\_ -> NoOp) <| Browser.Dom.focus "signup-password"
    )


toSession : Model -> Session
toSession { session } =
    session |> GuestSession


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
    | EnteredPassword String
    | EnteredPassConfirm String
    | Blurred Field
    | CompletedResetPassword (Result Http.Error ( LoggedIn, Language ))
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
                    , sendResetPasswordRequest validModel
                    )

                Err errs ->
                    ( { model | errors = errs }, Cmd.none )

        Blurred field ->
            let
                lang =
                    GlobalData.language model.globalData

                validator =
                    case field of
                        Password ->
                            passwordValidator lang

                        PasswordConfirm ->
                            passwordConfirmValidator lang

                        Form ->
                            ifTrue (always True) ( Form, "" )
            in
            case validate validator model of
                Ok _ ->
                    ( { model | errors = [] }, Cmd.none )

                Err errs ->
                    ( { model | errors = errs }, Cmd.none )

        EnteredPassword password ->
            ( { model | password = password }, Cmd.none )

        EnteredPassConfirm passwordConfirm ->
            ( { model | passwordConfirm = passwordConfirm }, Cmd.none )

        CompletedResetPassword (Ok ( user, lang )) ->
            ( { model | transition = Just user }, Session.storeLogin lang user )

        CompletedResetPassword (Err error) ->
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
                                409 ->
                                    ( Form, Translation.enKo lang "Username already exists. Login?" "이미 존재하는 계정입니다. 로그인하시겠습니까?" )

                                _ ->
                                    fallbackMsg

                        _ ->
                            fallbackMsg
            in
            ( { model | errors = [ errorMsg ], password = "", passwordConfirm = "" }, Cmd.none )

        UserSaved ->
            ( model, Nav.replaceUrl model.navKey "/" )


passwordValidator : Language -> Validator ( Field, String ) Model
passwordValidator lang =
    Validate.firstError
        [ ifBlank .password ( Password, Translation.enKo lang "Please enter a password." "비밀번호를 입력해 주세요." )
        , ifTrue (\model -> String.length model.password < 7) ( Password, Translation.enKo lang "Password should be 7 characters or more." "비밀번호는 7자 이상이어야 합니다." )
        ]


passwordConfirmValidator : Language -> Validator ( Field, String ) Model
passwordConfirmValidator lang =
    Validate.firstError
        [ ifBlank .passwordConfirm ( PasswordConfirm, Translation.enKo lang "Please enter your password twice." "비밀번호를 두 번 입력해 주세요." )
        , ifFalse (\m -> m.password == m.passwordConfirm) ( PasswordConfirm, Translation.enKo lang "Passwords do not match." "비밀번호가 일치하지 않습니다." )
        ]


modelValidator : Language -> Validator ( Field, String ) Model
modelValidator lang =
    Validate.all
        [ passwordValidator lang
        , passwordConfirmValidator lang
        ]


sendResetPasswordRequest : Valid Model -> Cmd Msg
sendResetPasswordRequest validModel =
    let
        { password, resetToken, session } =
            Validate.fromValid validModel
    in
    Session.requestResetPassword CompletedResetPassword { newPassword = password, token = resetToken } session



-- VIEW


view : Model -> Html Msg
view model =
    let
        lang =
            GlobalData.language model.globalData

        formErrors =
            getFieldErrors Form model.errors

        passwordErrors =
            getFieldErrors Password model.errors

        passwordConfirmErrors =
            getFieldErrors PasswordConfirm model.errors
    in
    div [ id "form-page" ]
        [ div [ class "brand" ]
            [ img [ id "logo", src "gingko-leaf-logo.svg" ] []
            , h1 [] [ text "Gingko" ]
            ]
        , div [ class "page-bg" ] []
        , h1 [ class "headline" ] [ text (Translation.enKo lang "Write better, faster." "더 잘, 더 빠르게 쓰세요.") ]
        , div [ class "header" ] [ span [ class "alt-action" ] [ text (Translation.enKo lang "Already have an account? " "이미 계정이 있으신가요? "), a [ href "/login" ] [ text (Translation.enKo lang "Login" "로그인") ] ] ]
        , div [ class "center-form" ]
            [ form [ onSubmit SubmittedForm ]
                [ div [] [ text (String.join "\n" formErrors) ]
                , div [ class "input-error" ] [ text (String.join "\n" passwordErrors) ]
                , input
                    [ id "signup-password"
                    , placeholder (Translation.enKo lang "Password (min. 7 characters)" "비밀번호 (7자 이상)")
                    , onInput EnteredPassword
                    , type_ "password"
                    , value model.password
                    , onBlur (Blurred Password)
                    ]
                    []
                , div [ class "input-error" ] [ text (String.join "\n" passwordConfirmErrors) ]
                , input
                    [ id "signup-password-confirm"
                    , placeholder (Translation.enKo lang "Confirm Password" "비밀번호 확인")
                    , onInput EnteredPassConfirm
                    , type_ "password"
                    , value model.passwordConfirm
                    , onBlur (Blurred PasswordConfirm)
                    ]
                    []
                , button [ class "cta" ] [ text (Translation.enKo lang "Reset Password" "비밀번호 재설정") ]
                ]
            ]
        ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Session.userLoggedIn UserSaved
