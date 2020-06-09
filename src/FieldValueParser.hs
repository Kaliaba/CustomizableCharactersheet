{-# LANGUAGE OverloadedStrings #-}
module FieldValueParser where

import Import
import Data.Map (assocs)
import GHC.List(foldl)
import GHC.Float
import Text.ParserCombinators.ReadP as RP
import Data.Char

import SheetField

data Expression a = Lit a
                | Operator Char (Expression a) (Expression a)
                deriving Show

evaluateFieldValue :: Text -> Maybe Double
evaluateFieldValue value = evalExpr (parseTextToExpr value)

evalExpr :: Maybe (Expression Double) -> Maybe Double
evalExpr e = case e of
    Nothing               -> Nothing
    Just (Lit i)          -> Just i
    Just (Operator f l r) -> liftM2 (evalOp f) (evalExpr (Just l)) (evalExpr (Just r))

evalOp :: Char -> (Double -> Double -> Double)
evalOp c = case c of
    '+' -> (+)
    '-' -> (-)
    '/' -> (/)
    '*' -> (*)

parseTextToExpr :: Text -> Maybe(Expression Double)
parseTextToExpr t = readExpr (readP_to_S parseExpr (unpack (Import.filter (/=' ') t))) where
    readExpr [] = Nothing
    readExpr ((e, s) : _) = if s == "" then (Just e) else Nothing

parseExpr :: ReadP (Expression Double)
parseExpr = parseOp <++ parseOpPrio <++ parsePar <++ parseInt

-- ParseOp will parse any + and - operator with terms on each side
-- The resulting tree of 2 + 3 + 4 - 5 would be
--          +
--         2 \
--            +
--           3 \
--              -
--             4 5
parseOp :: ReadP (Expression Double)
parseOp = do
    skipSpaces
    expr1 <- parseOpPrio <++ parsePar <++ parseInt
    skipSpaces
    op    <- choice (Import.map char ['+', '-'])
    skipSpaces
    expr2 <- parseExpr
    skipSpaces
    return (Operator op expr1 expr2)

-- parseOpPrio parses all * and / Operations that are prioritized over + and -
-- => these operations have to be the leafs of the tree (only overwritten by parantheses)
-- => The only possible terms to the left and right of such a operator are parantheses or numbers
-- The resulting tree of 2 * 3 + 4 is       -- The resulting tree of 2 * (3 + 4) is
--          +                               --          * 
--         / 2                              --         2 \ 
--        *                                 --            +
--       4 5                                --           3 4
parseOpPrio :: ReadP (Expression Double)
parseOpPrio = do
    skipSpaces
    expr1 <- parsePar <++ parseInt
    skipSpaces
    op    <- choice (Import.map char ['*', '/'])
    skipSpaces
    expr2 <- parseOpPrio <++ parsePar <++ parseInt
    skipSpaces
    return (Operator op expr1 expr2)

parsePar :: ReadP (Expression Double)
parsePar = do
    skipSpaces
    expr <- between (char '(') (char ')') parseExpr
    skipSpaces
    return expr    

parseInt :: ReadP (Expression Double)
parseInt = do
    sign <- option '+' (char '-')
    n <- munch1 isDigit
    return $ let num = (read n) in
        if sign == '-' then Lit (-1 * num) else Lit num

-- Takes a Char that indicates variables in the String
-- eg: #MyVar; %MyVar
-- parseVar :: Char -> ReadP (Expression Double)
-- parseVar c sm = do
--     char c
--     varKey <- many1 $ satisfy isAlpha
--     return (Var varKey)

-- parseRoll :: ReadP (Expression Double)
-- parseRoll = do
--     dCount <- parseInt
--     char 'd'
--     dSides <- parseInt
--     return (Roll dCount dSides)

