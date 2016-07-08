import Html exposing (..)
import Html.App as Html
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as Json exposing (Decoder, (:=))
import Task

-- Main
main =
  Html.program
    { init = init 1
    , view = view
    , update = update
    , subscriptions = subscriptions
    }

init : Int -> (Analysis , Cmd Msg)
init analysis_id =
  (
    Analysis analysis_id "nope" [],
    fetchAnalysis analysis_id 
  )

-- Model

type alias Bug = {
  id: Int,
  bugzilla_id: Int,
  summary: String
}

type alias Analysis = {
  id: Int,
  name: String,
  bugs: List Bug
}

-- Update

type Msg
   = FetchAnalysis Int
   | FetchAnalysisSuccess Analysis
   | FetchAnalysisFailure Http.Error

update : Msg -> Analysis -> (Analysis, Cmd Msg)
update msg analysis =
  case msg of
    FetchAnalysis id ->
      (analysis, fetchAnalysis id)

    FetchAnalysisSuccess newAnalysis ->
      (newAnalysis, Cmd.none)

    FetchAnalysisFailure _ ->
      (analysis, Cmd.none)
    
fetchAnalysis : Int -> Cmd Msg
fetchAnalysis id =
  let 
    url = "http://localhost:5000/analysis/" ++ toString id ++ "/"
  in
    Task.perform FetchAnalysisFailure FetchAnalysisSuccess (Http.get decodeAnalysis url)

decodeAnalysis : Decoder Analysis
decodeAnalysis = 
  Json.object3 Analysis
    ("id" := Json.int)
    ("name" := Json.string)
    ("bugs" := Json.list decodeBug)

decodeBug : Decoder Bug
decodeBug =
  Json.object3 Bug
    ("id" := Json.int)
    ("bugzilla_id" := Json.int)
    (Json.at ["payload", "bug", "summary"] Json.string)

-- Subscriptions

subscriptions : Analysis -> Sub Msg
subscriptions analysis =
  Sub.none

-- Views

view : Analysis -> Html Msg
view analysis =
  div []
    [
      h1 [] [text ("Analysis: " ++ analysis.name)],
      div [] (List.map viewBug analysis.bugs)
    ]

viewBug: Bug -> Html msg
viewBug bug =
  div [class "bug"] [
    h4 [] [text bug.summary],
    p [] [
      span [] [text ("#" ++ (toString bug.bugzilla_id))],
      a [href ("https://bugzilla.mozilla.org/show_bug.cgi?id=" ++ (toString bug.bugzilla_id)), target "_blank"] [text "View on bugzilla"]
    ],
    hr [] []
  ]
