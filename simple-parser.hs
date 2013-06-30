module Main where 

import Control.Monad
import System.Environment
import System.Exit
import Text.ParserCombinators.Parsec

main :: IO ()
main = getArgs >>= print . eval . readExpr . head

runPrompt :: IO ()
runPrompt = forever $ do
              putStr ">> "
              line <- getLine
              case line of
                "(quit)" -> exitSuccess
                val      -> putStrLn . ("=> " ++) . show . eval . readExpr $ val
	
eval :: LispValue -> LispValue
eval val@(String _) = val
eval val@(Number _) = val
eval val@(Bool _) = val
eval (List [Atom "quote", val]) = val
eval (List (Atom func : args)) = apply func $ map eval args

apply :: String -> [LispValue] -> LispValue
apply func args = maybe (Bool False) ($ args) $ lookup func primitives

primitives :: [(String, [LispValue]-> LispValue)]
primitives = [ (        "+", numericBinOp (+)  )
             , (        "-", numericBinOp (-)  )
             , (        "*", numericBinOp (*)  )
             , (        "/", numericBinOp div  )
             , (      "mod", numericBinOp mod  )
             , ( "quotient", numericBinOp quot )
             , ("remainder", numericBinOp rem  )
             , (  "number?", \val -> case val of
                               [Number _] -> Bool True
                               _          -> Bool False)
             , (  "string?", \val -> case val of
                               [String _] -> Bool True
                               _          -> Bool False)
             , (  "symbol?", \val -> case val of
                               [Atom _]   -> Bool True
                               _          -> Bool False)
             ]

numericBinOp :: (Integer -> Integer -> Integer) -> [LispValue] -> LispValue
numericBinOp op params = Number $ foldl1 op $ map unpackNum params

unpackNum :: LispValue -> Integer
unpackNum (Number n) = n
unpackNum _ = 0

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
