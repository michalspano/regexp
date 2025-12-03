module Main where

import Parsing

-- import Test.QuickCheck
import Debug.Trace (trace)

import Prelude hiding (concat)
import Data.Char (isAlphaNum)


data Regex
    = Epsilon
    | Literal Char
    | Kleene Regex
    | Concat Regex Regex
    -- | Optional Regex
    -- | Plus Regex
    deriving (Eq, Show)

pp :: Regex -> String 
pp Epsilon = "()"
pp (Literal l) = [l]
pp (Kleene l@(Literal _)) = pp l ++ "*"
pp (Kleene r) = "(" ++ pp r ++ ")*"
pp (Concat r1 r2) = pp r1 ++ pp r2

-- instance Arbitrary Regex where
--    arbitrary = undefined

-- TODO: allow extra brackets around expression even if unnecessary

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
reg = reg1 <|> reg2

reg1 :: Parser Regex
reg1  =  concat
    <|> kleene
    <|> plus
    <|> optional 
    <|> literal
    <|> eps

reg2 :: Parser Regex
reg2  = do 
    char '('
    r <- reg1 <|> reg2
    char ')'
    return r

regWithOutConcat :: Parser Regex
regWithOutConcat = kleene
                <|> plus
                <|> optional 
                <|> literal
                <|> eps

regWithoutEpsilon :: Parser Regex
regWithoutEpsilon  =  concat
    <|> kleene
    <|> plus
    <|> optional 
    <|> literal

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

optional :: Parser Regex
optional = do 
    literal <|> eps 

plus :: Parser Regex 
plus = plus1 <|> plus2

plus1 :: Parser Regex
plus1 = do
    char '('
    r <- regWithoutEpsilon
    char ')'
    char '+'
    return $ Concat r (Kleene r) 

plus2 :: Parser Regex
plus2 = do
    r <- literal
    char '+'
    return $ Concat r (Kleene r)

main :: IO ()
main = do
    putStr ">>> "
    p <- getLine
    case parseReg p of
        Nothing      -> putStrLn "Failed to parse."
        Just (p',"") -> do { print p' ; putStrLn $ pp p' }
        Just (p', rest) -> do
            putStrLn $ "Error: Input not read fully. Rest: " ++ show rest 
            putStrLn $ pp p'
    main