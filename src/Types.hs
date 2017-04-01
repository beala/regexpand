module Types where

import Control.Monad.Catch (Exception)
import Data.Text

data RegexParseError = RegexParseError String deriving (Eq)
data ArgsError = ArgsError String deriving (Show, Eq)

instance Show RegexParseError where
  show (RegexParseError msg) = "RegexParseError: " ++ msg
  
                     
instance Exception RegexParseError

instance Exception ArgsError

data ArgOptions = ArgOptions
                  { argRandom :: Maybe Int
                  , argRegex :: Text
                  } deriving (Eq, Show)


data RichRegex = RichLit Char
               | RichEmpty
               | RichAny
               | RichConcat RichRegex RichRegex
               | RichAlt RichRegex RichRegex
               | RichKleene RichRegex
               | RichOneOrMore RichRegex
               | RichZeroOrOne RichRegex
               | RichCharClass CharClass
               | RichExactly Int RichRegex

data CharClass = Alpha | Num | AlphaNum

data Regex = Lit Char
           | Empty
           | Concat Regex Regex
           | Alt Regex Regex
           | Kleene Regex
           | Any
             deriving (Show, Eq)

-- Regex without the Kleene star.
data ReducedRegex = RLit Char
                  | REmpty
                  | RConcat ReducedRegex ReducedRegex
                  | RAlt ReducedRegex ReducedRegex
                    deriving (Show, Eq)
