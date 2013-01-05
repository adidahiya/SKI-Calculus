-- Generic Parser implementation
-- Author: Adi Dahiya (adahiya@seas.upenn.edu)

{-# OPTIONS -Wall -fwarn-tabs -fno-warn-type-defaults  #-}

module ParserTrans ( GenParser
                   , Parser
                   , getC
                   , choose
                   , (<|>)
                   , satisfy
                   , doParse
                   ) where

import Control.Monad.State

newtype GenParser e a = P (StateT [e] [] a)

type Parser a = GenParser Char a

doParse :: GenParser e a -> [e] -> [(a,[e])]
doParse (P p) st = runStateT p st

instance Monad (GenParser e) where
  p1 >>= f = P $ StateT (\st -> do (x, st') <- doParse p1 st
                                   doParse (f x) st')
  return x = P $ return x
  fail s   = P $ put [] >> fail s

-- | Return the next character
getC :: GenParser e e 
getC = P $ do (c:cs) <- get
              put cs
              return c

-- | Return the next character if it satisfies the given predicate
satisfy :: (e -> Bool) -> GenParser e e 
satisfy p = do c <- getC
               if (p c) then return c else fail "End of input"


-- | Combine two parsers together in parallel, producing all
--   possible results from either parser.
choose :: GenParser e a -> GenParser e a -> GenParser e a
p1 `choose` p2 = P $ StateT (\st -> doParse p1 st ++ doParse p2 st)


-- | Combine two parsers together in parallel, but only use the
--   first result. This means that the second parser is used only
--   if the first parser completely fails.
(<|>) :: GenParser e a -> GenParser e a -> GenParser e a
p1 <|> p2 = P $ StateT (\st -> case doParse (p1 `choose` p2) st of
                                 []  -> []
                                 x:_ -> [x])
