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
    Analysis analysis_id "nope",
    fetchAnalysis analysis_id 
  )

-- Model

type alias Analysis = {
    id: Int,
    name: String
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
  Json.object2 Analysis
    ("id" := Json.int)
    ("name" := Json.string)

-- Subscriptions


subscriptions : Analysis -> Sub Msg
subscriptions analysis =
  Sub.none

-- View

view : Analysis -> Html Msg
view analysis =
  div []
    [
      h1 [] [text ("Analysis: " ++ analysis.name)]
    ]
