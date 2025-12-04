module Main where

import Parsing

-- import Test.QuickCheck
import Debug.Trace (trace)
import Data.Maybe (isJust)

import Prelude hiding (concat)
import Data.Char (isAlphaNum)
import Testing (testSuite)

{-
Current alphabet:
Literal, Epsilon, . , *, +, Concat 
-}
data Regex
    = Epsilon
    | Literal Char
    | Kleene Regex
    | Concat Regex Regex
    | Dot
    -- | Optional Regex
    -- | Plus Regex
    deriving (Eq, Show)

test :: Bool 
test = process testSuite 1 True
    where
        process []     _ success = success
        process (s:ss) k success = case parseReg s of
            Just (s',"") -> trace (show k ++ ") [OK]\t" ++ s) process ss (k+1) success
            Just (s',rest) -> trace (show k ++ ") [Failed]\t" ++ s ++ " with rest: " ++ rest) process ss (k+1) False
            Nothing -> trace (show k++ ") [FAILED]\t: " ++ s) $ process ss (k+1) False

atom = dot
    <|> literal
    <|> eps -- TODO: move last?
    <|> char '(' *> atom <* char ')'

pp :: Regex -> String 
pp Epsilon = "ε"
pp Dot     = "."
pp (Literal l) = [l]
pp (Kleene l@(Literal _)) = pp l ++ "*"
pp (Kleene d@Dot) = pp d ++ "*"
pp (Kleene r) = "(" ++ pp r ++ ")*"
pp (Concat r1 r2) = pp r1 ++ pp r2
-- pp (Optional r)   = pp r ++ "?"

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
-- TODO: see if we can move this to eps
parseReg "" = Just (Epsilon,"")
parseReg s  = parse reg s

reg :: Parser Regex
reg = reg1 -- <|> reg2

reg1 :: Parser Regex
reg1  = concat
    <|> kleene
    <|> plus
    <|> dot
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
literal = literal1 <|> literal2

literal1 :: Parser Regex
literal1 = do 
    c <- sat isAlphaNum
    return $ Literal c

-- foo :: Parser Regex -> Parser Regex
-- foo p = char '(' *> p <* char ')'

literal2 :: Parser Regex
literal2 = do
    char '('
    c <- literal1 <|> literal2 
    char ')'
    return c

eps :: Parser Regex
eps = do
    char '('
    char ')'
    return Epsilon

dot :: Parser Regex
dot = dot1 <|> dot2

dot1 :: Parser Regex
dot1 = do
    char '.'
    return Dot

dot2 = do 
    char '('
    c <- dot1 <|> dot2
    char ')'
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
    r <- dot <|> literal <|> eps
    char '*'
    return $ Kleene r

concat :: Parser Regex
concat = concat1 <|> concat2

concat1 :: Parser Regex
concat1 = do
    head_r  <- regWithOutConcat
    tail_rs <- zeroOrMore reg
    return $ foldl Concat head_r tail_rs

concat2 :: Parser Regex
concat2 = do
    char '('
    c <- concat1 <|> concat2
    char ')'
    
    tail_rs <- zeroOrMore reg 
    return $ foldl Concat c tail_rs


{-
   | ignored for now

optional :: Parser Regex
optional = do 
    -- r <- literal -- TODO: make this take any regex expression
    r <- literal
    char '?'
    return $ Optional r
-}

plus :: Parser Regex 
plus = plus1 <|> plus2

plus1 :: Parser Regex
plus1 = do
    char '('
    r <- reg
    char ')'
    char '+'
    return $ Concat r (Kleene r) 

plus2 :: Parser Regex
plus2 = do
    r <- dot <|> literal <|> eps
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