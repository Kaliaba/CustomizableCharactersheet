{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Sheet where

import Import

getSheetR :: Handler Html
getSheetR = do
    defaultLayout $ do 
        $(widgetFile "sheet")

