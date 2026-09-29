module Import.Template exposing (Template(..), docName, fetchJSON, fromString, toString)

import Http
import Json.Decode as Json exposing (Decoder)
import Translation exposing (Language(..))


type Template
    = WelcomeTree
    | Timeline
    | AcademicPaper
    | ProjectBrainstorming
    | HerosJourney


fromString : String -> Maybe Template
fromString str =
    case str of
        "welcome" ->
            Just WelcomeTree

        "timeline" ->
            Just Timeline

        "academic-paper" ->
            Just AcademicPaper

        "project-brainstorming" ->
            Just ProjectBrainstorming

        "heros-journey" ->
            Just HerosJourney

        _ ->
            Nothing


toString : Template -> String
toString template =
    case template of
        WelcomeTree ->
            "welcome"

        Timeline ->
            "timeline"

        AcademicPaper ->
            "academic-paper"

        ProjectBrainstorming ->
            "project-brainstorming"

        HerosJourney ->
            "heros-journey"


{-| Name given to the new document created from a template.
-}
docName : Language -> Template -> String
docName lang template =
    case ( lang, template ) of
        ( Ko, WelcomeTree ) ->
            "시작하기"

        ( Ko, Timeline ) ->
            "타임라인"

        ( Ko, AcademicPaper ) ->
            "학술 논문"

        ( Ko, ProjectBrainstorming ) ->
            "프로젝트 브레인스토밍"

        ( Ko, HerosJourney ) ->
            "영웅의 여정"

        _ ->
            toString template


{-| Korean templates live in /templates/ko/; other languages use the English ones.
-}
srcUrl : Language -> Template -> String
srcUrl lang template =
    case lang of
        Ko ->
            "/templates/ko/" ++ toString template ++ ".json"

        _ ->
            "/templates/" ++ toString template ++ ".json"


fetchJSON : (Result Http.Error a -> msg) -> Decoder a -> Language -> Template -> Cmd msg
fetchJSON toMsg jsonDecoder lang template =
    Http.get { expect = Http.expectJson toMsg jsonDecoder, url = srcUrl lang template }
