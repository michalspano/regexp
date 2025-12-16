module DFA (fromNFA, fromNFAMulti) where

import qualified DMap
import Data.Map (Map)
import qualified Data.Map as Map
import NFA (epsilonClosure)
import Data.List (union, intercalate)
import Datatypes

import Control.Monad (when)
import qualified Control.Monad.State as S
import Control.Monad.RWS (modify)


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
    -- put this into transitions
    S.modify (\(as, ts, seen) -> (as, DMap.insert currentState (const reachableStatesWithClosure) ts, seen))
    -- if hasAcceptState, put this into acceptStates
    when (hasAcceptState (nfa, currentState)) $ S.modify (\(acceptStates, ts, seen) -> (currentState:acceptStates, ts, seen) )
    let newStates = filter (\e -> not $ e `elem` (thrd stat) ) (Map.elems reachableStatesWithClosure)
    mapM_ (fromNFAMulti' pair) newStates


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

