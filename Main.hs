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
    | Dot
    | Optional Regex
    -- | Plus Regex
    deriving (Eq, Show)

pp :: Regex -> String 
pp Epsilon = "ε"
pp Dot     = "."
pp (Literal l) = [l]
pp (Kleene l@(Literal _)) = pp l ++ "*"
pp (Kleene d@Dot) = pp d ++ "*"
pp (Kleene r) = "(" ++ pp r ++ ")*"
pp (Concat r1 r2) = pp r1 ++ pp r2
pp (Optional r)   = pp r ++ "?"

-- instance Arbitrary Regex where
--    arbitrary = undefined

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
reg1  = concat
    <|> kleene
    <|> plus
    <|> optional
    <|> dot
    <|> literal
    <|> eps

reg2 :: Parser Regex
reg2  = do 
    char '('
    r <- reg1 <|> reg2
    char ')'
    return r


regWithoutEpsilon :: Parser Regex
regWithoutEpsilon = regWithoutEpsilon1 <|> regWithoutEpsilon2

regWithoutEpsilon1 :: Parser Regex
regWithoutEpsilon1 = concat
    <|> kleene
    <|> plus
    <|> optional 
    <|> dot
    <|> literal

regWithoutEpsilon2 :: Parser Regex
regWithoutEpsilon2 = do
    char '('
    r <- regWithoutEpsilon1 <|> regWithoutEpsilon2
    char ')'
    return r


regWithOutConcat :: Parser Regex
regWithOutConcat = kleene 
                <|> plus
                <|> optional 
                <|> dot
                <|> literal
                <|> eps

regWithOutEpsilonAndOptional :: Parser Regex
regWithOutEpsilonAndOptional = concat
    <|> kleene
    <|> plus
    <|> dot
    <|> literal

literal :: Parser Regex
literal = do 
    c <- sat isAlphaNum
    return $ Literal c

eps :: Parser Regex
eps = do
    char '('
    char ')'
    return Epsilon

dot :: Parser Regex
dot = do 
    c <- char '.'
    return Dot

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
    r <- dot <|> literal 
    char '*'
    return $ Kleene r

concat :: Parser Regex
concat = do
    head_r  <- regWithOutConcat
    tail_rs <- zeroOrMore reg
    return $ foldl Concat head_r tail_rs

optional :: Parser Regex
optional = do 
    -- r <- literal -- TODO: make this take any regex expression
    r <- regWithOutEpsilonAndOptional
    char '?'
    return $ Optional r

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
    r <- dot <|> literal
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