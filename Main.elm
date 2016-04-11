module Main where

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (on, onClick, targetValue)
import String
import Signal exposing (Signal, Address)
import StartApp.Simple as StartApp


---- MODEL ----

type alias Model =
    { assignments: List Assignment
    , uid: Int
    , currentGrade: Float
    , completedGrade: Float
    }

type alias Assignment =
    { grade: String
    , weight: String
    , id: Int
    }


newAssignment: Int -> Assignment
newAssignment id =
    { grade = "0.0"
    , weight = "0.0"
    , id = id
    }


emptyModel: Model
emptyModel =
    { assignments = [newAssignment 1]
    , uid = 2
    , currentGrade = 0.0
    , completedGrade = 0.0
    }


---- UPDATE ----

type Action
    = NoOp
    | Add
    | Delete Int
    | Calculate
    | UpdateGrade Int String
    | UpdateWeight Int String

update: Action -> Model -> Model
update action model =
    case action of
        NoOp -> model

        Add ->
            { model
                | uid = model.uid + 1
                , assignments = model.assignments ++ [newAssignment model.uid]
                }

        Delete id -> { model | assignments =  List.filter (\a -> a.id /= id) model.assignments }

        Calculate ->
            let
                toFloat num =
                    case (String.toFloat num) of
                        Err str -> 0.0
                        Ok n -> n
                calculateSum assignment sum =
                    sum + (toFloat assignment.grade / 100 * toFloat assignment.weight)
            in { model
                | currentGrade = (List.foldr calculateSum 0.0 model.assignments) / model.completedGrade * 100
                , completedGrade = List.foldr (\a sum -> sum + toFloat a.weight) 0.0 model.assignments
                }

        UpdateGrade id grade ->
            let updateGrade a =
                if a.id == id
                then { a | grade = grade }
                else a
            in
                { model | assignments = List.map updateGrade model.assignments }

        UpdateWeight id weight ->
            let updateWeight a =
                if a.id == id
                then { a | weight = weight }
                else a
            in
                { model | assignments = List.map updateWeight model.assignments }


---- VIEW ----

view: Address Action -> Model -> Html
view address model =
    div [ class "main" ]
        [ h1 [] [ text "Grade Calculator" ]
        , div [ class "totals" ]
            [ div [] [ text ("Current Grade: " ++ toString(model.currentGrade) ++ "%") ]
            , div [] [ text ("Percent of course completed: " ++ toString(model.completedGrade) ++ "%") ]
            ]
        , button [ onClick address Add ] [ text "Add an assignment"]
        , table [ class "table" ]
          [ thead []
            [ tr []
              [ th [] [ text "Grade" ]
              , th [] [ text "Weight (Out of 100)" ]
              ]
            ]
          , tbody []
            (List.map (viewAssignment address) model.assignments)
          ]
        , button [ onClick address Calculate ] [ text "Calculate your current grade" ]
        ]

viewAssignment: Address Action -> Assignment -> Html
viewAssignment address assignment =
  tr []
    [ td []
      [ input
        [ value (assignment.grade)
        , on "input" targetValue (Signal.message address << UpdateGrade assignment.id)
        ]
        [] ]
    , td []
      [ input
        [ value (assignment.weight)
        , on "input" targetValue (Signal.message address << UpdateWeight assignment.id)
        ]
        [] ]
    , td [] [ button [ onClick address (Delete assignment.id) ] [ text "Remove" ] ]
    ]


main = StartApp.start { model = emptyModel, view = view, update = update }
