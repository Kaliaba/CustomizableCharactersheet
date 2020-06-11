{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}
module Handler.Sheet where


import Import
import Data.Map (assocs)
import Data.Aeson           (decode)
-- import Data.Aeson.Parser    (json)
import Data.ByteString as BS
import Data.ByteString.Lazy as BL
import System.Random

-- import Handler.Roll
import FieldValueParser as FVParser

data SheetField = SheetField 
  { field_id :: Text
  ,  name :: Text
  ,  value :: Text
  } deriving Show

data OptionsPOST = Options
    { minVal :: Int
    , maxVal :: Int
    }
    deriving Show

instance FromJSON SheetField where
  parseJSON (Object v) = SheetField
        <$> v .: "field_id"
        <*> v .: "name"
        <*> v .: "value"
  parseJSON _ = mzero

-- getRollR :: Handler Html
-- getRollR = do
    -- (widget, enctype) <- generateFormPost randIntForm
    -- defaultLayout $ do 
        -- $(widgetFile "roll-form")

getSheetR :: Handler Html
getSheetR = do
    sess <- getSession
    (widget, enctype) <- generateFormPost randIntForm
    defaultLayout $ do 
        $(widgetFile "sheet")
        $(widgetFile "link")
        $(widgetFile "roll-form")
        

postSheetR :: Handler Html
postSheetR = do
    ((result, widget), enctype) <- runFormPost randIntForm
    sess <- getSession
    case result of 
        FormSuccess options -> do
            rand <- liftIO (getRand (minVal options, maxVal options))
            -- sess <- getSession
            defaultLayout $ do 
                $(widgetFile "sheet")
                $(widgetFile "link")
                $(widgetFile "roll-form")
                $(widgetFile "roll-result")
                -- $(widgetFile "link")
        _ -> defaultLayout $ do 
                $(widgetFile "sheet")
                $(widgetFile "roll-form")
                -- $(widgetFile "link")




-- postRollR :: Handler Html
-- postRollR = do
--     ((result, widget), enctype) <- runFormPost randIntForm
--     case result of 
--         FormSuccess options -> do
--             rand <- liftIO (getRand (minVal options, maxVal options))
--             defaultLayout $ do 
--                 $(widgetFile "roll-result")
--         _ -> defaultLayout $ do 
--             $(widgetFile "roll-form")

getRand :: (Int, Int) -> IO Int
getRand minmax = randomRIO minmax

randIntAForm :: AForm Handler OptionsPOST
randIntAForm = Options
    <$> areq intField "Min Value" Nothing
    <*> areq intField "Max Value" Nothing

randIntForm :: Html -> MForm Handler (FormResult OptionsPOST, Widget)
randIntForm = renderTable randIntAForm

jsonToField :: BS.ByteString -> Maybe SheetField
jsonToField bs = decode (BL.fromStrict bs)





