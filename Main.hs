module Main where

import Datatypes
import Parser
import DMap (toString)
import DFA (fromNFAMulti, flattenToDFA)
import NFA (epsilonClosure, fromRegex)
import qualified Regex

-- TODO: make this a IO-Maybe-T
main :: IO ()
main = do
    putStr ">>> "
    p <- getLine
    let regex = parseReg p
    case regex of
        Nothing      -> putStrLn "Failed to parse."
        Just (p',"") -> do 
            -- putStrLn $ "Token received: " ++ show p'
            let nfa = fromRegex p'
            let epsClosure = epsilonClosure nfa
            let powerSetDFA = fromNFAMulti nfa
            let dfa = flattenToDFA powerSetDFA
            --putStrLn $ "Our NFA: " ++ show nfa ++ "\n"
            --putStrLn $ "Our PowerSetDFA: " ++ show powerSetDFA ++ "\n"
            --putStrLn $ "With the epsilon-clojure: " ++ toString epsClosure
            --putStrLn $ "With the dfa: " ++ show dfa
            writeFile "nfa.dot" $ show nfa
            writeFile "powerSetDFA.dot" $ show powerSetDFA
            writeFile "dfa.dot" $ show dfa
            putStr ">? "
            input <- getLine
            let res = Regex.check dfa input
            putStrLn $ "Checking " ++ show input ++ " on " ++ show p ++" results in: " ++ show res
        Just (p', rest) -> do
            putStrLn $ "Error: Input not read fully. Rest: " ++ show rest 
            putStrLn $ pp p'
    main