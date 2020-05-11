{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Roll where

import Import
import System.Random

data Options = Options
    { minVal :: Int
    , maxVal :: Int
    }
    deriving Show

getRollR :: Handler Html
getRollR = do
    (widget, enctype) <- generateFormPost randIntForm
    defaultLayout $ do 
        $(widgetFile "roll-form")

postRollR :: Handler Html
postRollR = do
    ((result, widget), enctype) <- runFormPost randIntForm
    case result of 
        FormSuccess options -> do
            rand <- liftIO (getRand (minVal options, maxVal options))
            defaultLayout $ do 
                $(widgetFile "roll-result")
        _ -> defaultLayout $ do 
            $(widgetFile "roll-form")
    

getRand :: (Int, Int) -> IO Int
getRand minmax = randomRIO minmax

randIntAForm :: AForm Handler Options
randIntAForm = Options
    <$> areq intField "Min Value" Nothing
    <*> areq intField "Max Value" Nothing

randIntForm :: Html -> MForm Handler (FormResult Options, Widget)
randIntForm = renderTable randIntAForm

