{-# LANGUAGE OverloadedStrings #-}
module FieldValueParser where

import Import
import Data.Map
import Data.Text.Read
--import GHC.List(foldl)
--import GHC.Float
import Text.ParserCombinators.ReadP as RP
import Data.Char

import SheetField as SF

data Expression a = Lit a
                | Operator Char (Expression a) (Expression a)
                | Var Text
                deriving Show

-- The character that indicates a variable
varChar :: Char
varChar = '%'

evaluateFieldValue :: SessionMap -> Text -> Maybe Double
evaluateFieldValue sm v = evalExpr sm (parseTextToExpr v)

evalExpr :: SessionMap -> Maybe (Expression Double) -> Maybe Double
evalExpr sm e = case e of
    Nothing               -> Nothing
    Just (Lit i)          -> Just i
    Just (Operator f l r) -> liftM2 (evalOp f) (evalExpr sm (Just l)) (evalExpr sm (Just r))
    Just (Var s)          -> evalVar sm s

evalOp :: Char -> (Double -> Double -> Double)
evalOp c = case c of
    '+' -> (+)
    '-' -> (-)
    '/' -> (/)
    '*' -> (*)
    _   -> \_ _ -> 0

evalVar :: SessionMap -> Text -> Maybe Double
evalVar sm k = getField sm k >>= fieldToNum

getField :: SessionMap -> Text -> Maybe SheetField
getField sm t = jsonToField (sm ! t)

fieldToNum :: SheetField -> Maybe Double
fieldToNum sf = getd (double (value sf)) where
    getd :: Either String (Double, Text) -> Maybe Double
    getd (Left _) = Nothing
    getd (Right (d, _)) = Just d

parseTextToExpr :: Text -> Maybe(Expression Double)
parseTextToExpr t = readExpr (readP_to_S parseExpr (unpack (Import.filter (/=' ') t))) where
    readExpr [] = Nothing
    readExpr ((e, s) : _) = if s == "" then (Just e) else Nothing

parseExpr :: ReadP (Expression Double)
parseExpr = opParser

opParser :: ReadP (Expression Double)
opParser = parseOp <++ parseOpPrio <++ parseVar <++ parsePar <++ parseInt

opPrioParser :: ReadP (Expression Double)
opPrioParser = parseOpPrio <++ parseVar <++ parsePar <++ parseInt

parParser :: ReadP (Expression Double)
parParser = parsePar <++ parseVar <++ parseInt

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
    expr1 <- opPrioParser
    skipSpaces
    op    <- choice (Import.map char ['+', '-'])
    skipSpaces
    expr2 <- opParser
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
    expr1 <- parParser
    skipSpaces
    op    <- choice (Import.map char ['*', '/'])
    skipSpaces
    expr2 <- opPrioParser
    skipSpaces
    return (Operator op expr1 expr2)

parsePar :: ReadP (Expression Double)
parsePar = do
    skipSpaces
    expr <- between (char '(') (char ')') parseExpr
    skipSpaces
    return expr  
    
parseVar :: ReadP (Expression Double)
parseVar = do
    _ <- char varChar
    varkey <- munch1 isAlpha
    return (Var (pack varkey))

parseInt :: ReadP (Expression Double)
parseInt = do
    sign <- option '+' (char '-')
    n <- munch1 isDigit
    return $ let num = (read n) in
        if sign == '-' then Lit (-1 * num) else Lit num

-- parseRoll :: ReadP (Expression Double)
-- parseRoll = do
--     dCount <- parseInt
--     char 'd'
--     dSides <- parseInt
--     return (Roll dCount dSides)

