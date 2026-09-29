module Page.Signup exposing (Model, Msg, globalData, init, navKey, subscriptions, toSession, transition, update, view)

import Ant.Icons.Svg as AntIcons
import Browser.Dom
import Browser.Navigation as Nav
import GlobalData exposing (GlobalData)
import Html exposing (..)
import Html.Attributes exposing (autocomplete, autofocus, checked, class, classList, for, href, id, src, style, type_, value)
import Html.Events exposing (onCheck, onClick, onInput, onSubmit)
import Html.Extra exposing (viewIf)
import Http exposing (Error(..))
import Import.Template as Template
import Route
import Session exposing (Guest, LoggedIn, Session(..))
import Svg.Attributes
import Task
import Translation exposing (Language)
import Utils exposing (getFieldErrors)
import Validate exposing (Valid, Validator, ifBlank, ifInvalidEmail, ifTrue, validate)



-- MODEL


type alias Model =
    { globalData : GlobalData
    , session : Guest
    , transition : Maybe LoggedIn
    , email : String
    , password : String
    , showPassword : Bool
    , didOptIn : Bool
    , errors : List ( Field, FieldError )
    , navKey : Nav.Key
    }


type Field
    = Form
    | Email
    | Password


init : Nav.Key -> GlobalData -> Guest -> ( Model, Cmd Msg )
init nKey gData session =
    ( { globalData = gData
      , session = session
      , transition = Nothing
      , email = ""
      , password = ""
      , showPassword = False
      , didOptIn = False
      , errors = []
      , navKey = nKey
      }
    , Task.attempt (\_ -> NoOp) <| Browser.Dom.focus "signup-email"
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
    | EnteredEmail String
    | EnteredPassword String
    | ToggleShowPassword
    | ToggledOptIn Bool
    | CompletedSignup (Result Http.Error ( LoggedIn, Language ))
    | UserSaved


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        SubmittedForm ->
            case validate modelValidator model of
                Ok validModel ->
                    ( model
                    , sendSignupRequest validModel
                    )

                Err errs ->
                    ( { model | errors = errs }, Cmd.none )

        EnteredEmail email ->
            ( { model | email = email }, Cmd.none )

        EnteredPassword password ->
            ( { model | password = password }, Cmd.none )

        ToggleShowPassword ->
            ( { model | showPassword = not model.showPassword }, Task.attempt (\_ -> NoOp) (Browser.Dom.focus "signup-password") )

        ToggledOptIn isOptedIn ->
            ( { model | didOptIn = isOptedIn }, Cmd.none )

        CompletedSignup (Ok ( user, lang )) ->
            ( { model | transition = Just user }, Session.storeSignup lang user )

        CompletedSignup (Err error) ->
            let
                fallbackMsg =
                    ( Form, ServerIssue )

                errorMsg =
                    case error of
                        Timeout ->
                            ( Form, TimeoutError )

                        NetworkError ->
                            ( Form, NetworkErrorMsg )

                        BadStatus statusCode ->
                            case statusCode of
                                409 ->
                                    ( Form, UsernameExists )

                                _ ->
                                    fallbackMsg

                        _ ->
                            fallbackMsg
            in
            ( { model | errors = [ errorMsg ], password = "" }, Cmd.none )

        UserSaved ->
            -- If I want to route to different welcome trees, based on e.g. isMac or language, here is where I can.
            ( model, Route.replaceUrl model.navKey (Route.Import Template.WelcomeTree) )


emailValidator : Validator ( Field, FieldError ) Model
emailValidator =
    Validate.firstError
        [ ifBlank .email ( Email, BlankEmail )
        , ifInvalidEmail .email (\eml -> ( Email, InvalidEmail eml ))
        ]


passwordValidator : Validator ( Field, FieldError ) Model
passwordValidator =
    Validate.firstError
        [ ifBlank .password ( Password, BlankPassword )
        , ifTrue (\model -> String.length model.password < 7) ( Password, InvalidPassword )
        ]


modelValidator : Validator ( Field, FieldError ) Model
modelValidator =
    Validate.all
        [ emailValidator
        , passwordValidator
        ]


sendSignupRequest : Valid Model -> Cmd Msg
sendSignupRequest validModel =
    let
        { email, password, session, didOptIn } =
            Validate.fromValid validModel
    in
    Session.requestSignup CompletedSignup email password didOptIn session



-- VIEW


view : Model -> Html Msg
view model =
    let
        lang =
            GlobalData.language model.globalData

        emailErrors =
            getFieldErrors Email model.errors

        passwordErrors =
            getFieldErrors Password model.errors

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
            [ h1 [ class "headline" ] [ text (Translation.enKo lang "Signup to Gingko Writer" "Gingko Writer 회원가입") ]
            , p [ class "subtitle" ] [ text (Translation.enKo lang "Write the way you think, organize as you go, and let your words flow." "생각하는 방식 그대로 쓰고, 쓰면서 정리하고, 글이 자연스럽게 흘러가게 하세요.") ]
            ]
        , div [ class "center-form" ]
            [ form [ onSubmit SubmittedForm ]
                [ label [ for "singup-email" ] [ text (Translation.enKo lang "Email" "이메일") ]
                , input
                    [ id "signup-email"
                    , classList [ ( "has-error", List.length emailErrors > 0 ) ]
                    , onInput EnteredEmail
                    , type_ "email"
                    , value model.email
                    , autofocus True
                    , autocomplete True
                    ]
                    []
                , viewErrors lang Email model.errors
                , label [ for "singup-password" ] [ text (Translation.enKo lang "Password (7+ characters)" "비밀번호 (7자 이상)"), showHidePassword ]
                , input
                    [ id "signup-password"
                    , onInput EnteredPassword
                    , type_
                        (if model.showPassword then
                            "text"

                         else
                            "password"
                        )
                    , classList [ ( "has-error", List.length passwordErrors > 0 ) ]
                    , value model.password
                    , autocomplete True
                    ]
                    []
                , viewErrors lang Password model.errors
                , div [ style "display" "flex", style "gap" "6px" ]
                    [ input [ type_ "checkbox", id "email-optin", checked model.didOptIn, onCheck ToggledOptIn ] []
                    , label [ for "email-optin" ] [ text (Translation.enKo lang "Email me help & tips (~6 emails)" "도움말과 팁(약 6통)"), br [] [], text (Translation.enKo lang "and product news (every ~2 months)." "및 제품 소식(약 2개월마다)을 이메일로 받겠습니다.") ]
                    ]
                , viewErrors lang Form model.errors
                , button [ id "signup-button", class "cta" ] [ text (Translation.enKo lang "Start Writing" "글쓰기 시작") ]
                , div [ id "post-cta-divider" ] [ hr [] [], div [] [ text (Translation.enKo lang "or" "또는") ], hr [] [] ]
                , span [ class "alt-action" ] [ text (Translation.enKo lang "Already have an account? " "이미 계정이 있으신가요? "), a [ href "/login" ] [ text (Translation.enKo lang "Login" "로그인") ] ]
                ]
            ]
        ]


type FieldError
    = ServerIssue
    | TimeoutError
    | NetworkErrorMsg
    | UsernameExists
    | BlankEmail
    | InvalidEmail String
    | BlankPassword
    | InvalidPassword


viewErrors : Language -> Field -> List ( Field, FieldError ) -> Html msg
viewErrors lang field errors =
    case field of
        Form ->
            let
                formErrors =
                    getFieldErrors Form errors
            in
            viewIf (not <| List.isEmpty formErrors) (div [ id "form-errors" ] (List.map (viewError lang field) formErrors))

        Email ->
            let
                emailErrors =
                    getFieldErrors Email errors
            in
            viewIf (not <| List.isEmpty emailErrors) (div [ class "input-errors" ] (List.map (viewError lang field) emailErrors))

        Password ->
            let
                passwordErrors =
                    getFieldErrors Password errors
            in
            viewIf (not <| List.isEmpty passwordErrors) (div [ class "input-errors" ] (List.map (viewError lang field) passwordErrors))


viewError : Language -> Field -> FieldError -> Html msg
viewError lang field error =
    case ( field, error ) of
        ( Form, ServerIssue ) ->
            text (Translation.enKo lang "Server Issue. Something wrong on our end. Please let us know!" "서버 문제가 발생했습니다. 저희 쪽에 문제가 있는 것 같습니다. 알려 주세요!")

        ( Form, TimeoutError ) ->
            text (Translation.enKo lang "Timed out. Maybe there's a server issue?" "시간이 초과되었습니다. 서버에 문제가 있는 것 같습니다.")

        ( Form, NetworkErrorMsg ) ->
            text (Translation.enKo lang "Network error. Maybe you're offline?" "네트워크 오류입니다. 오프라인 상태인지 확인해 주세요.")

        ( Form, UsernameExists ) ->
            span [] [ text (Translation.enKo lang "Username already exists. " "이미 존재하는 계정입니다. "), a [ href "/login" ] [ text (Translation.enKo lang "Login" "로그인") ], text (Translation.enKo lang "?" "하시겠습니까?") ]

        ( Email, BlankEmail ) ->
            text (Translation.enKo lang "Please enter an email address." "이메일 주소를 입력해 주세요.")

        ( Email, InvalidEmail eml ) ->
            text (eml ++ Translation.enKo lang " does not seem to be a valid email." "은(는) 올바른 이메일 주소가 아닌 것 같습니다.")

        ( Password, BlankPassword ) ->
            text (Translation.enKo lang "Please enter a password." "비밀번호를 입력해 주세요.")

        ( Password, InvalidPassword ) ->
            text (Translation.enKo lang "Passwords should have 7 characters or more." "비밀번호는 7자 이상이어야 합니다.")

        _ ->
            text ""



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Session.userLoggedIn UserSaved
