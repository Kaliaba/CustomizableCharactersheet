{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
module FieldValueParser where

import Import
import Data.Map (assocs)
import GHC.List(foldl)
import GHC.Float
import Text.ParserCombinators.ReadP
import Data.Char

data Expression = Literal Int
                | BinaryOperator Expression Char Expression
                | Parenthesized Expression
                | Roll Int Int
                | Variable String
            deriving Show

evaluate_Expr :: Expression -> String
evaluate_Expr( Roll l_roll r_roll ) = show l_roll ++ "d" ++ show r_roll
evaluate_Expr( Parenthesized expression ) = "(" ++ evaluate_Expr expression ++ ")"
evaluate_Expr( Literal i ) = show i
evaluate_Expr( BinaryOperator( Literal l ) op ( Literal r ) ) = case op of
    '+' -> show( l + r )
    '-' -> show( l - r )
    '/' -> show( l `div` r )
evaluate_Expr( BinaryOperator l_expr op r_expr ) =
    evaluate_Expr l_expr ++ [op] ++ evaluate_Expr r_expr

parse_Expr :: Text -> Maybe Expression
parse_Expr t =
    let parsed = readP_to_S readExpr (filter (/=' ') (unpack t))
    in fst <$> (find (null . snd) (parsed))

readNumber :: ReadP Int
readNumber = do
  let combineDigits = \ l r -> 10*l + r
  numStr <- munch1 isDigit
  let numDigits = map digitToInt numStr
  pure( foldl combineDigits 0 numDigits )

readTerm = choice [ readRoll, readNumberExpr, readVariable ]

readNumberExpr = Literal <$> readNumber

readRoll = do
  n <- readNumber
  char 'd'
  s <- readNumber
  pure( Roll n s )

readParens = Parenthesized <$> between( char '(' )( char ')' ) readExpr

readExpr :: ReadP Expression
readExpr = readOp <++ readParens <++ readRoll <++ readNumberExpr

readOp :: ReadP Expression
readOp = BinaryOperator
  <$> ( readParens <++ readTerm )
  <*> choice[ char '+', char '-', char '/' ]
  <*> ( readParens <++ readTerm )

readVariable = do
  string "${"
  name <- munch1 isAlpha
  char '}'
  pure( Variable name )