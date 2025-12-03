module Main where

import Parsing

import Test.QuickCheck
import Debug.Trace (trace)

import Prelude hiding (concat)
import Data.Char (isAlphaNum)

newtype RegexStr = RegexStr String

instance Arbitrary RegexStr where
    arbitrary = undefined

data Regex
    = Epsilon
    | Literal Char
    | Kleene Regex
    | Concat Regex Regex
    deriving (Eq, Show)

-- Kleene (Concat r1 r2)
-- (ab)* (ab, ab, ab, ab,...)

{-
- eps
- literal X
- Kleene (eps)
- Kleene (Literal X)
- Kleene (Kleene (Kleene ... (eps | Literal X)))
-}

parseReg :: String -> Maybe(Regex,String)
parseReg = parse reg

reg :: Parser Regex
reg  =  concat
    <|> kleene
    <|> literal
    <|> eps

regWithOutConcat :: Parser Regex
regWithOutConcat = kleene
                <|> literal
                <|> eps

eps :: Parser Regex
eps = do
    char '('
    char ')'
    return Epsilon

literal :: Parser Regex
literal = do 
    c <- sat isAlphaNum
    return $ Literal c

kleene :: Parser Regex
kleene = kleene1 <|> kleene2

kleene1 :: Parser Regex
kleene1 = do
    char '('
    r <- reg
    char ')'
    char '*'
    return $ Kleene r

kleene2 :: Parser Regex
kleene2 = do
    r <- literal
    char '*'
    return $ Kleene r

concat :: Parser Regex
concat = do
    head_r  <- regWithOutConcat
    tail_rs <- zeroOrMore reg
    return $ foldl Concat head_r tail_rs

main :: IO ()
main = do
    putStr ">>> "
    p <- getLine
    case parseReg p of
        Nothing      -> putStrLn "Failed to parse."
        Just (p',"") -> print p'
        Just (p', rest) -> putStrLn $ "Error: Input not read fully. Rest: " ++ show rest
    main