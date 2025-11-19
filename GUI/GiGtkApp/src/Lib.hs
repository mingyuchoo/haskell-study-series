{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}


module Lib
    ( someFunc
    ) where

import qualified GI.Gtk as Gtk
import Data.GI.Base

someFunc :: IO ()
someFunc = do
    -- Application 생성
    app <- new Gtk.Application
        [ #applicationId := "com.example.GiGtkApp" ]

    -- Application activate
    _ <- on app #activate $ do
        window <- new Gtk.ApplicationWindow
            [ #application := app
            , #title := "Hello GTK4"
            , #defaultWidth := 300
            , #defaultHeight := 200
            ]

        button <- new Gtk.Button
            [ #label := "Click me" ]

        -- 클릭 이벤트
        _ <- on button #clicked $
            putStrLn "Button clicked!"

        -- GTK4에서는 Widget upcast 필요
        widget <- Gtk.toWidget button

        #setChild window (Just widget)

        #show window

    _ <- #run app Nothing
    return ()
