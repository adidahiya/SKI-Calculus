-- SKI-calculus interpreter
-- Author: Adi Dahiya (adahiya@seas.upenn.edu)

{-# OPTIONS -Wall #-}

module SKICalculus where

import Control.Monad
import Test.HUnit
import ParserTrans
import ParserCombinators

-- SKI-calculus terms consist of either an S, a K, an I, or the "application"
-- of one term to another. Applications are usually written without a space;
-- parentheses can be used for grouping; and application associates to the
-- left.
--
-- For example,
--  SKK(IS)
-- denotes
--  ((S K) K) (I S)

data SKI = S
         | K
         | I
         | App SKI SKI
    deriving Eq

instance Show SKI where
  show S = "S"
  show K = "K"
  show I = "I"
  show (App t (App x y))  = show t ++ "(" ++ show x ++ show y ++ ")"
  show (App t1 t2)        = show t1 ++ show t2

------------------------------------------------------------------------------
-- Parsing functions (note: currently don't accept whitespace)

-- | Accept a value surrounded by parentheses
parenP :: Parser a -> Parser a
parenP p = between (char '(') p (char ')')

-- | Accept a particular string s as a given value x
constP :: String -> a -> Parser a
constP s x = string s >> return x


-- | Syntax tree node
nodeP :: Parser SKI
nodeP = choice [ constP "S" S
               , constP "K" K
               , constP "I" I ]

-- | Generate a list of terms by applying the given parser @p@ as many times as
--   possible. Then, use `App` to combine the terms using left-association.
appP :: Parser SKI -> Parser SKI
appP p = liftM2 (foldl App) nodeP (many p)

-- | Parse a SKI syntax tree
skiP :: Parser SKI
skiP = appP termP where
  -- A term is either a proper App subtree (S with 3 args, K with 2 args, I with
  -- 1 arg), a simple sequence (any improper subtree), or a single node.
  termP   = (parenP $ skiP <|> simpleP) <|> nodeP
  simpleP = appP nodeP

------------------------------------------------------------------------------
-- Evaluation of SKI-calculus terms proceeds by applying the following
-- rewriting rules, where X, Y, Z stand for arbitrary terms:
--
--  IX   --1--> X
--  KXY  --2--> X
--  SXYZ --3--> XZ(YZ)
--
-- These rewriting rules can be applied to any matching subterm anywhere
-- in a term. For example, the example term given above can be evaluated
-- as follows:
--
--  SKK(IS) --1--> SKKS --3--> KS(KS) --2--> S
--
-- When multiple rules apply, any rule may be chosen.  (For example,
-- SKK(IS) can also be rewritten using rule 3 to  K(IS)(K(IS)).)

eval :: SKI -> SKI
eval (App I x)                 = eval x
eval (App (App K x) _)         = eval x
eval (App (App (App S x) y) z) = eval $ App (App (eval x) ez) (App (eval y) ez)
  where ez = eval z
eval (App x y)                 = App (eval x) (eval y)
eval x                         = x

test :: String -> SKI
test ski = case (doParse skiP ski) of
             ((x, _) : _) -> x
             _            -> error "Failed to parse expression"

-- Run the SKI parser, evaluate the expression, and print the result
evalSKI :: String -> String
evalSKI ski = case (doParse parser ski) of
                ((ast, _) : _) -> show $ loop ast
                _            -> "Failed to parse expression"
  where
    parser = parenP skiP <|> skiP
    loop :: SKI -> SKI
    loop ast | (length $ show e) == (length $ show ast) = e
             | otherwise                                = loop e
      where e = eval ast

------------------------------------------------------------------------------
-- Testing

test1, test2, test3, test4, test5, test6, test7, test8, test9, test10 :: Test
test1 = "t1" ~: evalSKI "SKK(IS)" ~?= "S"
test2 = "t2" ~: evalSKI "SIIK" ~?= "KK"
test3 = "t3" ~: evalSKI "SII(KS)" ~?= "S"
test4 = "t4" ~: evalSKI "SII(SK)" ~?= "SK(SK)"
test5 = "t5" ~: evalSKI "KKKSKS" ~?= "SS"
test6 = "t6" ~: evalSKI "S(K(SI))KKS" ~?= "SK"
test7 = "t7" ~: evalSKI "S(K(SI))K(KS)(SK)" ~?= "SK(KS)"
test8 = "t8" ~: evalSKI "SKSK" ~?= "K"
test9 = "t9" ~: evalSKI "SII(SII)" ~?= "SII(SII)"
test10 = "t10" ~: evalSKI "K(IS)(K(IS))" ~?= "S"

main :: IO ()
main = do
  _ <- runTestTT $ TestList [ test1, test2, test3, test4, test5
                            , test6, test7, test8, test9, test10 ]
  return ()
