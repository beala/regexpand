module Parser
  ( parseRegex
  )
where

import Text.Parsec
import Text.Parsec.Expr
import qualified Data.Text as T
import Types
import Data.Functor.Identity (Identity)

parseRegex :: Parsec T.Text () RichRegex
parseRegex = parseRegex_ <* eof
                             
parseRegex_ :: Parsec T.Text () RichRegex
parseRegex_ = buildExpressionParser table term

table :: OperatorTable T.Text () Identity RichRegex
table = [ [ Postfix (char '*' *> return RichKleene)
          , Postfix (char '+' *> return RichOneOrMore)
          , Postfix (char '?' *> return RichZeroOrOne)
          , Postfix (between (char '{') (char '}') parseCount) ]
        , [ Infix (return RichConcat) AssocLeft ]
        , [ Infix (char '|' *> return RichAlt) AssocLeft ]]

term :: Parsec T.Text () RichRegex
term = escaped
       <|> between (string "[:") (string ":]") parseClass
       <|> RichLit <$> noneOf reserved
       <|> char '.' *> return RichAny
       <|> between (char '(') (char ')') parseRegex_

escaped :: Parsec T.Text () RichRegex
escaped = do
  _ <- char '\\'
  e <- oneOf reserved
  return $ RichLit e

reserved :: String
reserved = "*()|?.+[]{}\\"

parseClass :: Parsec T.Text () RichRegex
parseClass =
  string "alpha" *> pure ( RichCharClass Alpha)
  <|> string "num" *> pure (RichCharClass Num)
  <|> string "alnum" *> pure (RichCharClass AlphaNum)

parseCount :: Parsec T.Text () (RichRegex -> RichRegex)
parseCount = do
  digits <- many1 digit
  return $ RichExactly (read digits)
