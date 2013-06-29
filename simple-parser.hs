import Control.Monad
import System.Environment
import Text.ParserCombinators.Parsec

main :: IO ()
main = do
	args <- getArgs
	putStrLn (readExpr (head args))
	

myParser :: Parser LispValue
myParser = parseString <|> parseAtom 

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

readExpr :: String -> String
readExpr input = case parse myParser "lisp" input of
  Left err -> "No match: " ++ show err
  Right val -> "Found value: " ++ show val

data LispValue = Atom String
  | List [LispValue]
  | DottedList [LispValue] LispValue
  | Number Integer
  | String String
  | Bool Bool
  deriving (Show)
