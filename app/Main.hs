{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Text.Parsec as P
import Data.Foldable (traverse_)
import qualified Options.Applicative as Args
import Control.Monad.Catch (throwM)

import Types
import Parser
import Regex

main :: IO ()
main = do
  args <- Args.execParser argParserInfo
  let inputRegex = argRegex args
  parsed <- case P.parse parseRegex "" inputRegex of
    Right p -> return $ p
    Left e -> throwM (RegexParseError (show e))
  traverse_ T.putStrLn (go 3 parsed)
    
go :: Int -> RichRegex -> [T.Text]
go i = produceAll . reduceRegex i . optimizeFix . reduceRichRegex

argParser :: Args.Parser ArgOptions
argParser = ArgOptions
            <$> Args.optional (Args.option Args.auto
            (Args.short 'r'
              `Args.mappend` Args.help "Sample N random words from the language."
              `Args.mappend` Args.metavar "N"))
            <*> (T.pack <$> Args.strArgument
                  (Args.metavar "REGEX"
                  `Args.mappend` Args.help "Regex")
                )

argParserInfo :: Args.ParserInfo ArgOptions
argParserInfo = Args.info parser options
  where parser = argParser Args.<**> Args.helper
        options = (Args.fullDesc `Args.mappend` Args.progDesc "Expand a regular expression.")
