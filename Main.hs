module Main where

import Parser
import NFA     -- * @constructNFA@


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
            putStrLn $ "Our NFA: " ++ show nfa
            writeFile "tmp.dot" (stringifyNFA nfa)
        Just (p', rest) -> do
            putStrLn $ "Error: Input not read fully. Rest: " ++ show rest 
            putStrLn $ pp p'
    main