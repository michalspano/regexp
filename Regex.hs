module Regex (check) where

import Datatypes
import qualified Parser as P
import qualified DFA (fromNFAMulti, flattenToDFA, fromNFA)
import qualified NFA (epsilonClosure, fromRegex)
import qualified DMap
import qualified Data.Map as Map

type CompiledRegex = DFA

match :: P.Regex -> String -> Bool
match pattern input = let dfa = (DFA.fromNFA . NFA.fromRegex) pattern in 
    check dfa input


-- type DFATransitions = DefaultMap State (Map Char State)
check :: DFA -> String -> Bool
check dfa@(DFA start accepts ts) input = go start input
    where
        go :: State -> String -> Bool
        go current []     = current `elem` accepts
        go current (c:cs) = parseChar || parseDot
            where 
                parseChar = case Map.lookup c $ DMap.lookup current ts of
                    Nothing        -> False
                    Just nextState -> go nextState cs

                parseDot = case Map.lookup '.' $ DMap.lookup current ts of
                    Nothing        -> False 
                    Just nextState -> go nextState cs

-- TODO: consider using Reader monad
{-
matchC :: CompiledRegex -> String -> Bool
matchC = undefined

compile :: Pattern -> CompiledRegex
compile = undefined
-}