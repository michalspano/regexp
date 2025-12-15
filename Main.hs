module Main where

import Parser
import NFA     -- * @constructNFA@
import DMap (toString)
import DFA (fromNFAMulti, stringifyPowerSetDFA)


-- TODO: make this a IO-Maybe-T
main :: IO ()
main = do
    putStr ">>> "
    p <- getLine
    let regex = parseReg p
    case regex of
        Nothing      -> putStrLn "Failed to parse."
        Just (p',"") -> do 
            putStrLn $ "Token received: " ++ show p'
            let nfa = getNFA p'
            let epsClosure = epsilonClosure nfa
            let powerSetDFA = fromNFAMulti (nfa, epsClosure)
            putStrLn $ "Our NFA: " ++ show nfa ++ "\n"
            putStrLn $ "Our PowerSetDFA: " ++ show powerSetDFA ++ "\n"
            putStrLn $ "With the epsilon-clojure: " ++ toString epsClosure
            writeFile "nfa.dot" (stringifyNFA nfa)
            writeFile "powerSetDFA.dot" (stringifyPowerSetDFA powerSetDFA)
        Just (p', rest) -> do
            putStrLn $ "Error: Input not read fully. Rest: " ++ show rest 
            putStrLn $ pp p'
    main