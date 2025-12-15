module DFA (fromNFA, fromNFAMulti, stringifyPowerSetDFA) where

import qualified DMap
import qualified Data.Map as Map
import NFA (epsilonClosure)
import Data.List (union, intercalate)
import Datatypes
import Control.Monad (when)
import qualified Control.Monad.State as S
import Control.Monad.RWS (modify)
import Debug.Trace

{-
Algorithm:



1. Start with eps(start) as new start
2. 



-}

-- type DFATransitions = DefaultMap State (DefaultMap Char State)
-- data DFA = DFA State State DFATransitions deriving Show 

fromNFA :: (NFA, EpsClosure) -> DFA
fromNFA (nfa, epsClosure) = undefined




eps :: Char
eps = '\949'




fromNFAMulti :: (NFA, EpsClosure) -> PowerSetDFA
-- fromNFAMulti pair@(nfa@(NFA start _ ts),epsClosure) = uncurry (PowerSetDFA [start]) finalState
fromNFAMulti pair@(nfa@(NFA start _ ts),epsClosure) = PowerSetDFA initStart (frst finalState) (scnd finalState)
    where
        initStart  = DMap.lookup start epsClosure
        emptyState = ([], DMap.empty Map.empty, [])
        finalState = S.execState (fromNFAMulti' pair initStart) emptyState

-- type PowerSetDFATransitions = DefaultMap MultiState (DefaultMap Char State)

{-
Helper-functions needed:
- get all the symbols used by a multi-state
- For one symbol, get all the states reachable as a multi-state
- For all states in a multi-state, get the epsilon-clojure
- Check if the final multi-state contains the accepting-state
-}
fromNFAMulti' :: (NFA, EpsClosure) -> MultiState -> S.State ([MultiState], PowerSetDFATransitions, [MultiState]) ()
fromNFAMulti' pair@(nfa@(NFA start _ ts), epsClosure) currentState = do
    -- call recursively on new added multistates
    S.modify (\(a, b, seen) -> (a, b, currentState:seen))
    stat <- S.get
    let symbols = getSymbols ts currentState
    let reachableStates = foldr (\char accu -> Map.insert char (getReachableStates (ts, currentState) char) accu) Map.empty symbols
    let reachableStatesWithClosure = Map.map (\multiState -> getMSClosure (epsClosure, multiState)) reachableStates 
    -- trace ("Reachable states: " ++ show reachableStates ++ "\n") $ return ()
    -- trace ("Reachables states with Epsclosure: " ++ show reachableStatesWithClosure ++ "\n") $ return ()
    -- put this into transitions
    S.modify (\(as, ts, seen) -> (as, DMap.insert currentState (const reachableStatesWithClosure) ts, seen))
    -- if hasAcceptState, put this into acceptStates
    when (hasAcceptState (nfa, currentState)) $ S.modify (\(acceptStates, ts, seen) -> (currentState:acceptStates, ts, seen) )
    let newStates = filter (\e -> not $ e `elem` (thrd stat) ) (Map.elems reachableStatesWithClosure)
    trace ("CurrentState " ++ show currentState ++ "\n with new states: " ++ (show newStates) ++ (show $ hasAcceptState (nfa, currentState)))
        $ mapM_ (fromNFAMulti' pair) newStates


frst :: (a, b, c) -> a
frst (e, _, _) = e

scnd :: (a, b, c) -> b
scnd (_, e, _) = e

thrd :: (a, b, c) -> c
thrd (_, _, e) = e

getSymbols :: NFATransitions -> MultiState -> [Char]
getSymbols nfaTs = foldr (\state accu -> accu ++ filter (/= eps) (DMap.keys (DMap.lookup state nfaTs))) []

getReachableStates :: (NFATransitions, MultiState) -> Char -> MultiState
getReachableStates (nfaTs, ms) c = foldr (\state accu -> DMap.lookup c (DMap.lookup state nfaTs) ++ accu) [] ms

getMSClosure :: (EpsClosure, MultiState) -> MultiState 
getMSClosure (epsC, ms) = foldr (\state accu -> accu `union` DMap.lookup state epsC) [] ms

hasAcceptState :: (NFA, MultiState) -> Bool
hasAcceptState (_, [])                                   = False
hasAcceptState (nfa@(NFA _ accept _),s:ss) | accept == s = True
                                           | otherwise   = hasAcceptState (nfa, ss)

-- create graphviz-compatible string from an NFA
stringifyPowerSetDFA :: PowerSetDFA -> String
stringifyPowerSetDFA (PowerSetDFA startMs endMss ts) =
    unlines [ "digraph G{"
           , headers
           , staticBody
           , body, "}" ]

    where
        headers = unlines $ map indent
            [ "{"
            , concatMap (\ms -> indent $ "S" ++ formatMultiState ms ++ "[shape=doublecircle]") endMss
            , "}"
            ]

        staticBody = unlines $ map indent
            [ "empty [label=\"\", shape=none,height=.0,width=.0]"
            , "empty -> S" ++ formatMultiState startMs ]

        indent xs = if null xs then "" else "\t" ++ xs

        body = transitionsToString ts
        transitionsToString :: PowerSetDFATransitions -> String
        transitionsToString ts = go (DMap.toList ts) ""
            where
                go :: [(MultiState, Map.Map Char MultiState)] -> String -> String
                go [] accu       = accu
                go ((state, dMap):tss) accu = go tss (accu ++ inner state (Map.toList dMap))

        -- TODO: use unlines
        inner :: MultiState -> [(Char, MultiState)] -> String 
        inner start xs = foldr (\(ch,ms) acc -> acc ++ createEdge start ms ch) "" xs

        -- TODO: prettify this shit
        helper li = if null li then "" else "\n"
        createEdge :: MultiState -> MultiState -> Char -> String
        createEdge s t l = "\tS" ++ formatMultiState s ++ " -> S" ++ formatMultiState t ++ " [label=" ++ formatChar l ++ "]\n"

dot :: Char
dot = '•'

formatMultiState :: MultiState -> String
formatMultiState = concatMap show
formatChar :: Char -> String
formatChar '.' = [dot]
formatChar l = [l]

