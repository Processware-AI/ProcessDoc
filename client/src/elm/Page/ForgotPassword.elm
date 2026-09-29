module Page.ForgotPassword exposing (Model, Msg, globalData, init, navKey, subscriptions, toSession, transition, update, view)

import Browser.Navigation as Nav
import GlobalData exposing (GlobalData)
import Html exposing (..)
import Html.Attributes exposing (autofocus, class, href, id, placeholder, src, type_, value)
import Html.Events exposing (onInput, onSubmit)
import Http exposing (Error(..))
import Result exposing (Result)
import Route
import Session exposing (Guest, LoggedIn, Session(..))
import Translation exposing (Language)
import Utils exposing (getFieldErrors)
import Validate exposing (Valid, Validator, ifBlank, ifInvalidEmail, validate)



-- MODEL


type alias Model =
    { globalData : GlobalData
    , session : Guest
    , transition : Maybe LoggedIn
    , email : String
    , errors : List ( Field, String )
    , sent : Bool
    , navKey : Nav.Key
    }


type Field
    = Form
    | Email


init : Nav.Key -> GlobalData -> Guest -> Maybe String -> ( Model, Cmd msg )
init nKey gData session email_ =
    ( { globalData = gData
      , session = session
      , transition = Nothing
      , email = email_ |> Maybe.withDefault ""
      , errors = []
      , sent = False
      , navKey = nKey
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
    = SubmittedForm
    | EnteredEmail String
    | CompletedForgotPassword (Result Http.Error ( LoggedIn, Language ))
    | UserSaved


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SubmittedForm ->
            case validate (modelValidator (GlobalData.language model.globalData)) model of
                Ok validModel ->
                    ( model
                    , sendForgotPasswordRequest validModel
                    )

                Err errs ->
                    ( { model | errors = errs }, Cmd.none )

        EnteredEmail email ->
            ( { model | email = email }, Cmd.none )

        CompletedForgotPassword (Ok ( user, lang )) ->
            ( { model | transition = Just user, sent = True }, Cmd.none )

        CompletedForgotPassword (Err error) ->
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
                                404 ->
                                    ( Form, Translation.enKo lang "Email not found.\n\nAre you trying to login with your gingkoapp.com username/password?\n\nYou'll need to create a new account to use the new version." "이메일을 찾을 수 없습니다.\n\ngingkoapp.com 사용자 이름/비밀번호로 로그인하려고 하시나요?\n\n새 버전을 사용하려면 새 계정을 만들어야 합니다." )

                                _ ->
                                    fallbackMsg

                        _ ->
                            fallbackMsg
            in
            ( { model | errors = [ errorMsg ] }, Cmd.none )

        UserSaved ->
            ( model, Route.pushUrl model.navKey Route.Root )


modelValidator : Language -> Validator ( Field, String ) Model
modelValidator lang =
    Validate.all
        [ Validate.firstError
            [ ifBlank .email ( Email, Translation.enKo lang "Please enter an email address." "이메일 주소를 입력해 주세요." )
            , ifInvalidEmail .email (\_ -> ( Email, Translation.enKo lang "This does not seem to be a valid email." "올바른 이메일 주소가 아닌 것 같습니다." ))
            ]
        ]


sendForgotPasswordRequest : Valid Model -> Cmd Msg
sendForgotPasswordRequest validModel =
    let
        { email, session } =
            Validate.fromValid validModel
    in
    Session.requestForgotPassword CompletedForgotPassword email session



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
    in
    div [ id "form-page" ]
        ([ div [ class "brand" ]
            [ img [ id "logo", src "gingko-leaf-logo.svg" ] []
            , h1 [] [ text "Gingko" ]
            ]
         , div [ class "page-bg" ] []
         ]
            ++ (if not model.sent then
                    [ h1 [ class "headline" ] [ text (Translation.enKo lang "Password Reset" "비밀번호 재설정") ]
                    , div [ class "header" ] [ span [ class "alt-action" ] [ text (Translation.enKo lang "New to Gingko? " "Gingko가 처음이신가요? "), a [ href "/signup" ] [ text (Translation.enKo lang "Signup" "회원가입") ] ] ]
                    , div [ class "center-form" ]
                        [ form [ onSubmit SubmittedForm ]
                            [ div [ id "form-error" ] [ text (String.join "\n" formErrors) ]
                            , div [ class "input-error" ] [ text (String.join "\n" emailErrors) ]
                            , input
                                [ onInput EnteredEmail
                                , placeholder (Translation.enKo lang "Email" "이메일")
                                , type_ "email"
                                , value model.email
                                , autofocus True
                                ]
                                []
                            , button [ class "cta" ] [ text (Translation.enKo lang "Send Reset Token" "재설정 링크 보내기") ]
                            ]
                        ]
                    ]

                else
                    [ h1 [ class "headline" ] [ text (Translation.enKo lang "Reset Email Sent" "재설정 이메일을 보냈습니다") ]
                    , div [ class "center-form" ]
                        [ text (Translation.enKo lang "Check your email for your password reset link." "이메일에서 비밀번호 재설정 링크를 확인해 주세요.")
                        , br [] []
                        , br [] []
                        , small [] [ text (Translation.enKo lang "No email received? " "이메일을 받지 못하셨나요? "), a [ href <| Route.toString (Route.ForgotPassword Nothing) ] [ text (Translation.enKo lang "Try again" "다시 시도") ] ]
                        ]
                    ]
               )
        )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Session.userLoggedIn UserSaved
