{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Sheet where

import Import
import System.Random

getSheetR :: Handler Html
getSheetR = do
    rand <- liftIO getRand
    defaultLayout $ do 
        $(widgetFile "sheet")

getRand :: IO Int
getRand = randomRIO (1, 20)
