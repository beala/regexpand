{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Regex
  ( reduceRichRegex
  , produceAll
  , produceRandom
  , optimizeFix
  , reduceRegex
  )
where

import qualified Control.Monad.State as St
import qualified System.Random as Rand
import qualified Data.Text as T

import Types

reduceRichRegex :: RichRegex -> Regex
reduceRichRegex (RichLit c) = Lit c
reduceRichRegex RichEmpty = Empty
reduceRichRegex RichAny = Any
reduceRichRegex (RichConcat r1 r2) = Concat (reduceRichRegex r1) (reduceRichRegex r2)
reduceRichRegex (RichAlt r1 r2) = Alt (reduceRichRegex r1) (reduceRichRegex r2)
reduceRichRegex (RichKleene r) = Kleene (reduceRichRegex r)
reduceRichRegex (RichOneOrMore r) =
  let rr = reduceRichRegex r in
    Concat rr (Kleene rr)
reduceRichRegex (RichZeroOrOne r) =
  let rr = reduceRichRegex r in
    Alt Empty rr
reduceRichRegex (RichCharClass Num) = numClass
reduceRichRegex (RichCharClass Alpha) = alphaClass
reduceRichRegex (RichCharClass AlphaNum) = Alt numClass alphaClass
reduceRichRegex (RichExactly c r) = foldr1 Concat (replicate c (reduceRichRegex r))

numClass :: Regex
numClass = foldr1 Alt (fmap Lit ['0', '1','2','3','4','5','6','7','8','9'])

alphaClass :: Regex
alphaClass = foldr1 Alt (fmap Lit ("abcdefghijklmnopqrstuvwxyz" :: String))

-- Use a source of randomness to produce a single string.
produceRandom :: (St.MonadState Rand.StdGen m) => Regex -> m T.Text
produceRandom Empty = return ""
produceRandom Any = do
  r <- St.state Rand.random
  return $ T.pack [r]
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
              if i < maxLoop-1
              then Alt (repeatConcat r i) (loop (i+1))
              else (repeatConcat r i)

repeatConcat :: Regex -> Int -> Regex
repeatConcat r stop = applyN stop (Concat r) r

-- Apply a function to itself n times.
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
optimize Any = Any

-- Apply optimize until the AST doesn't change. Ie, fixpoint.
optimizeFix :: Regex -> Regex
optimizeFix r = let r' = optimize r in
                if r == r'
                then r'
                else optimizeFix r'
                          
reduceRegex :: Int -> Regex -> ReducedRegex
reduceRegex _ (Lit r) = RLit r
reduceRegex _ Any = alphaNum
reduceRegex _ Empty = REmpty
reduceRegex i (Kleene r) = reduceRegex i (expandKleene i r)
reduceRegex i (Concat r1 r2) = RConcat (reduceRegex i r1) (reduceRegex i r2)
reduceRegex i (Alt r1 r2) = RAlt (reduceRegex i r1) (reduceRegex i r2)

alphaNum :: ReducedRegex
alphaNum = foldr
           RAlt
           (RLit '9')
           (fmap RLit ("abcdefghijklmnopqrstuvwxyz012345678" :: String))
