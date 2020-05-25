{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}
module Handler.Sheet where

import Import
import Data.Map (assocs)
import Text.ParserCombinators.ReadP
import Data.Char
import GHC.List(foldl)
import GHC.Float
import Data.Aeson           (Value, decode, object, (.=))
import Data.Aeson.Parser    (json)
import Data.ByteString.Lazy as BL
import Data.ByteString as BS

data SheetField = SheetField 
  { field_id :: Text
  ,  name :: Text
  ,  value :: Text
  } deriving Show

instance FromJSON SheetField where
  parseJSON (Object v) = SheetField
        <$> v .: "field_id"
        <*> v .: "name"
        <*> v .: "value"
  parseJSON _ = mzero

getSheetR :: Handler Html
getSheetR = do
    sess <- getSession
    defaultLayout $ do 
        $(widgetFile "sheet")

jsonToField :: BS.ByteString -> Maybe SheetField
jsonToField bs = decode (BL.fromStrict bs)

-- evaluate_Expr :: Expression -> String
-- evaluate_Expr( Roll l_roll r_roll ) = show l_roll ++ "d" ++ show r_roll
-- evaluate_Expr( Parenthesized expression ) = "(" ++ evaluate_Expr expression ++ ")"
-- evaluate_Expr( Number_Expr i ) = show i
-- evaluate_Expr( BinaryOperator( Number_Expr l ) op ( Number_Expr r ) ) = case op of
--     '+' -> show( l + r )
--     '-' -> show( l - r )
--     '/' -> show( l `div` r )
-- evaluate_Expr( BinaryOperator l_expr op r_expr ) =
--     evaluate_Expr l_expr ++ [op] ++ evaluate_Expr r_expr
-- 
-- parse_Expr name_field =
--     let parsed = readP_to_S readExpr (unpack(decodeUtf8 name_field))
--     in fst <$> (find (null . snd) (parsed))
--
--readNumber :: ReadP Int
--readNumber = do
--  let combineDigits = \ l r -> 10*l + r
--  numStr <- munch1 isDigit
--  let numDigits = map digitToInt numStr
--  pure( foldl combineDigits 0 numDigits )
--
--readTerm = choice [ readRoll, readNumberExpr, readVariable ]
--
--readNumberExpr = Number_Expr <$> readNumber
--
--readRoll = do
--  n <- readNumber
--  char 'd'
--  s <- readNumber
--  pure( Roll n s )
--
--readParens = Parenthesized <$> between( char '(' )( char ')' ) readExpr
--
--data Expression = Number_Expr Int
--                | BinaryOperator Expression Char Expression
--                | Parenthesized Expression
--                | Roll Int Int
--                | Variable String
--  deriving (Show)
--
--readExpr = readOp <++ readParens <++ readRoll <++ readNumberExpr
--
--
--readOp :: ReadP Expression
--readOp = BinaryOperator
--  <$> ( readParens <++ readTerm )
--  <*> choice[ char '+', char '-', char '/' ]
--  <*> ( readParens <++ readTerm )
--
--readVariable = do
--  string "${"
--  name <- munch1 isAlpha
--  char '}'
--  pure( Variable name )
--
--
--













