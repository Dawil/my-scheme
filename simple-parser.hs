import Control.Monad
import System.Environment
import Text.ParserCombinators.Parsec

main :: IO ()
main = getArgs >>= print . eval . readExpr . head
	
eval :: LispValue -> LispValue
eval val@(String _) = val
eval val@(Number _) = val
eval val@(Bool _) = val
eval (List [Atom "quote", val]) = val

parseExpr :: Parser LispValue
parseExpr = parseString
        <|> parseNumber
        <|> parseAtom
        <|> parseQuoted
        <|> do char '('
               x <- try (parseList <|> parseDottedList)
               char ')'
               return x

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

parseString :: Parser LispValue
parseString = do char '"'
                 x <- many (noneOf "\"")
                 char '"'
                 return $ String x

parseAtom :: Parser LispValue
parseAtom = do first <- letter <|> symbol
               rest <- many (letter <|> digit <|> symbol)
               let atom = first:rest
               return $ case atom of
                 "#t" -> Bool True
                 "#f" -> Bool False
                 _ -> Atom atom

parseNumber :: Parser LispValue
parseNumber = liftM (Number . read) $ many1 digit

parseList :: Parser LispValue
parseList = liftM List $ parseExpr `sepBy` spaces

parseDottedList :: Parser LispValue
parseDottedList = do head <- endBy parseExpr spaces
                     tail <- char '.' >> spaces >> parseExpr
                     return $ DottedList head tail

parseQuoted :: Parser LispValue
parseQuoted = do char '\''
                 x <- parseExpr
                 return $ List [Atom "quote", x]

readExpr :: String -> LispValue
readExpr input = case parse parseExpr "lisp" input of
  Left err -> String $ "No match: " ++ show err
  Right val -> val

data LispValue = Atom String
  | List [LispValue]
  | DottedList [LispValue] LispValue
  | Number Integer
  | String String
  | Bool Bool
  deriving (Show)
