{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Name where

import Import
import System.Random


data Options = Options
    { name_field :: Text
     ,      name :: Text 
    }
    deriving Show


getNameR :: Handler Html
getNameR = do
    (widget, enctype) <- generateFormPost randIntForm
    defaultLayout $ do 
        $(widgetFile "name")

postNameR :: Handler Html
postNameR = do
    ((result, widget), enctype) <- runFormPost randIntForm
    case result of 
        FormSuccess options -> do
            setSession (name options) (name_field options)
            redirect SheetR
        _ -> defaultLayout $ do 
            $(widgetFile "name-form")
    

getRand :: (Int, Int) -> IO Int
getRand minmax = randomRIO minmax

fieldinput :: AForm Handler Options
fieldinput= Options
    <$> areq textField "name_field" Nothing
    <*> areq textField "Name"     Nothing

randIntForm :: Html -> MForm Handler (FormResult Options, Widget)
randIntForm = renderTable fieldinput

