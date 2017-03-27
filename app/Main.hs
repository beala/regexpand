{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Control.Monad.State as St
import qualified System.Random as Rand
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Text.Parsec ((<|>))
import qualified Text.Parsec as P
import qualified Text.Parsec.Expr as P
import Control.Monad.Catch (throwM, Exception)
import Data.Foldable (traverse_)
import System.Environment (getArgs)

main :: IO ()
main = do
  -- entropy <- getStdGen
  input <- getArgs
  input <- case input of
    arg : _ -> return (T.pack arg)
    [] -> throwM (RegexParseError "FAIL")
  parsed <- case P.parse parseRegex "" input of
    Right p -> return $ p
    Left e -> throwM (RegexParseError (show e))
  traverse_ T.putStrLn (go 3 parsed)

data RegexParseError = RegexParseError String deriving (Show, Eq)
                     
instance Exception RegexParseError
    
data Regex = Lit Char
           | Empty
           | Concat Regex Regex
           | Alt Regex Regex
           | Kleene Regex
             deriving (Show, Eq)

-- Regex without the Kleene star.
data ReducedRegex = RLit Char
                  | REmpty
                  | RConcat ReducedRegex ReducedRegex
                  | RAlt ReducedRegex ReducedRegex
                    deriving (Show, Eq)

parseRegex :: P.Parsec T.Text () Regex
parseRegex = P.buildExpressionParser table term

table :: Monad m => P.OperatorTable T.Text () m Regex
table = [ [ P.Postfix (P.char '*' *> return Kleene) ]
        , [ P.Infix (return Concat) P.AssocLeft ]
        , [ P.Infix (P.char '|' *> return Alt) P.AssocLeft ]]

term :: P.Parsec T.Text () Regex
term = escaped
       <|> Lit <$> P.noneOf "*()|?."
       <|> P.between (P.char '(') (P.char ')') parseRegex

escaped :: P.Parsec T.Text () Regex
escaped = do
  _ <- P.char '\\'
  e <- P.oneOf reserved
  return $ Lit e

reserved :: String
reserved = "*()|?."

-- Use a source of randomness to produce a single string.
produceRandom :: (St.MonadState Rand.StdGen m) => Regex -> m T.Text
produceRandom Empty = return ""
produceRandom (Lit s) = return (T.pack [s])
produceRandom (Concat r1 r2) = (T.append) <$> produceRandom r1 <*> produceRandom r2
produceRandom (Alt r1 r2) = do
  b <- St.state Rand.random
  if b
  then produceRandom r1
  else produceRandom r2
produceRandom (Kleene r) = do
  b <- St.state Rand.random
  if b then (T.append) <$> produceRandom r <*> produceRandom (Kleene r)
  else return ""

-- Produce all strings on a ReducedRegex. Lack of a Kleene star ensures this
-- terminates.
produceAll :: ReducedRegex -> [T.Text]
produceAll (RLit s) = [T.pack [s]]
produceAll REmpty = [""]
produceAll (RConcat r1 r2) = do
  a <- produceAll r1
  b <- produceAll r2
  return $ a `T.append` b
produceAll (RAlt r1 r2) =
  produceAll r1 ++ produceAll r2

-- Expand a regex out to some maximum amount of repetition.
-- expandKleene 3 (a*) -> "", a, aa, aaa
expandKleene :: Int -> Regex -> Regex
expandKleene maxLoop r = (Alt Empty (loop 0))
    where loop i =
              if i < (maxLoop - 1)
              then Alt (repeatConcat r i) (loop (i+1))
              else (repeatConcat r i)

repeatConcat :: Regex -> Int -> Regex
repeatConcat r stop = applyN stop (Concat r) r

applyN :: Int -> (a -> a) -> a -> a
applyN 0 _ z = z
applyN n f z = f (applyN (n-1) f z)
                      
optimize :: Regex -> Regex
optimize (Kleene (Kleene r)) = optimize (Kleene r)
-- optimize (Concat (Lit l1) (Lit l2)) = Lit (l1 `T.append` l2)
optimize (Alt r1 r2) = let r1' = optimize r1
                           r2' = optimize r2 in
                       if r1' == r2'
                       then  r1'
                       else (Alt r1' r2')
optimize (Lit r) = Lit r
optimize Empty = Empty
optimize (Kleene r) = Kleene (optimize r)
optimize (Concat r1 r2) = Concat (optimize r1) (optimize r2)

-- Apply optimize until the AST doesn't change. Ie, fixpoint.
optimizeFix :: Regex -> Regex
optimizeFix r = let r' = optimize r in
                if r == r'
                then r'
                else optimizeFix r'
                          
reduceRegex :: Int -> Regex -> ReducedRegex
reduceRegex _ (Lit r) = RLit r
reduceRegex _ Empty = REmpty
reduceRegex i (Kleene r) = reduceRegex i (expandKleene i r)
reduceRegex i (Concat r1 r2) = RConcat (reduceRegex i r1) (reduceRegex i r2)
reduceRegex i (Alt r1 r2) = RAlt (reduceRegex i r1) (reduceRegex i r2)

go :: Int -> Regex -> [T.Text]
go i = produceAll . reduceRegex i . optimizeFix
