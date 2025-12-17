-- Main.hs for the Regex module
module Main where

import Datatypes
import Parser (parseReg)
import DMap   (toString)
import DFA    (fromNFAMulti, flattenToDFA)
import NFA    (epsilonClosure, fromRegex)

import qualified Regex

main :: IO () -- TODO: use MaybeT IO ()
main = do
    putStr ">> "
    p <- getLine
    case parseReg p of
        Just reg -> do 
            putStrLn $ "Token received: " ++ show reg

            let nfa         = fromRegex reg
            let epsClosure  = epsilonClosure nfa
            let powerSetDFA = fromNFAMulti nfa
            let dfa         = flattenToDFA powerSetDFA

            -- Save to files (locally)
            writeFile "nfa.dot" $ show nfa
            writeFile "powerSetDFA.dot" $ show powerSetDFA
            writeFile "dfa.dot" $ show dfa

            putStr ">? "
            input <- getLine
            let matched = Regex.match1 reg input

            -- Enables trace (experimental)
            -- let (_, trace) = Regex.checkWithTrace dfa input
            putStrLn $ "Checking " ++ show input ++ " on " ++ show p ++
                       " results in: " ++ show matched
            -- putStrLn $ "The trace is: " ++ show trace
        Nothing -> putStrLn "Failed to parse."
    main -- continue
