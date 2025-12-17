module Main where

import Datatypes
import Parser (parseReg)
import DMap (toString)
import DFA (fromNFAMulti, flattenToDFA)
import NFA (epsilonClosure, fromRegex)
import qualified Regex

main :: IO () -- TODO: use MaybeT IO ()
main = do
    putStr ">>> "
    p <- getLine
    case parseReg p of
        Just reg -> do 
            putStrLn $ "Token received: " ++ show reg
            let nfa = fromRegex reg
            let epsClosure = epsilonClosure nfa
            let powerSetDFA = fromNFAMulti nfa
            let dfa = flattenToDFA powerSetDFA
            writeFile "nfa.dot" $ show nfa
            writeFile "powerSetDFA.dot" $ show powerSetDFA
            writeFile "dfa.dot" $ show dfa
            putStr ">? "
            input <- getLine
            let matched = Regex.match1 reg input
            let (_, trace) = Regex.checkWithTrace dfa input
            putStrLn $ "1. Checking " ++ show input ++ " on " ++ show p ++" results in: " ++ show matched
            putStrLn $ "2. The trace is: " ++ show trace
        Nothing      -> putStrLn "Failed to parse."
    main
