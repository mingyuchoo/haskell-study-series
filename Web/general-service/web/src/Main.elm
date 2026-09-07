module Main exposing (main)

import Application.TaskBoard as TaskBoard
import Browser
import Infrastructure.TaskApi as TaskApi
import Presentation.TaskBoard as TaskBoardView
import Process
import Task


main : Program () TaskBoard.Model TaskBoard.Msg
main =
    Browser.element
        { init = \_ -> withEffects TaskBoard.init
        , update = \message model -> withEffects (TaskBoard.update message model)
        , subscriptions = \_ -> Sub.none
        , view = TaskBoardView.view
        }


withEffects : ( TaskBoard.Model, List TaskBoard.Effect ) -> ( TaskBoard.Model, Cmd TaskBoard.Msg )
withEffects ( model, effects ) =
    ( model, Cmd.batch (List.map performEffect effects) )


performEffect : TaskBoard.Effect -> Cmd TaskBoard.Msg
performEffect effect =
    case effect of
        TaskBoard.ClearNoticeAfter version ->
            Process.sleep 3000
                |> Task.perform (\_ -> TaskBoard.DismissNotice version)

        _ ->
            TaskApi.perform effect
