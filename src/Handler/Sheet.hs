{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Sheet where

import Import
import System.Random

data Options = Options
    { minVal :: Int
    , maxVal :: Int
    }
    deriving Show

getSheetR :: Handler Html
getSheetR = do
    (widget, enctype) <- generateFormPost randIntForm
    defaultLayout $ do 
        $(widgetFile "sheet")

postSheetR :: Handler Html
postSheetR = do
    ((result, widget), enctype) <- runFormPost randIntForm
    case result of 
        FormSuccess options -> do
            rand <- liftIO (getRand (minVal options, maxVal options))
            defaultLayout $ do 
                $(widgetFile "show-rand")
        _ -> defaultLayout $ do 
            $(widgetFile "sheet")
    

getRand :: (Int, Int) -> IO Int
getRand minmax = randomRIO minmax

randIntAForm :: AForm Handler Options
randIntAForm = Options
    <$> areq intField "Min Value" Nothing
    <*> areq intField "Max Value" Nothing

randIntForm :: Html -> MForm Handler (FormResult Options, Widget)
randIntForm = renderTable randIntAForm

