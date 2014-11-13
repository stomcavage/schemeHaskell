module Main where

import Control.Monad
import System.Environment   
import Text.ParserCombinators.Parsec hiding (spaces)

data LispVal = Atom String 
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Bool Bool

symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=?>@^_~#"

spaces :: Parser ()
spaces = skipMany1 space

parseAtom :: Parser LispVal
parseAtom = do first <- letter <|> symbol
               rest  <- many (letter <|> digit <|> symbol)
               let atom = [first] ++ rest
               return $ case atom of
                           "#t"      -> Bool True
                           "#f"      -> Bool False
                           otherwise -> Atom atom

parseNumber :: Parser LispVal
parseNumber = liftM (Number . read) $ many digit

parseString :: Parser LispVal
parseString = do char '"'
                 x <- many (noneOf "\"")
                 char '"'
                 return $ String x

parseExp :: Parser LispVal
parseExp = parseAtom
       <|> parseString
       <|> parseNumber

readExpr :: String -> String
readExpr input = case parse parseExp "lisp" input of
                    Left err  -> "No match: " ++ show err
                    Right val -> "Found value"

main :: IO ()
main = do args <- getArgs
          putStrLn (readExpr ( args !! 0 ))

