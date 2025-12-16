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



-- S.State ([MultiState], PowerSetDFATransitions, [MultiState]) ()
data Env = Env
    { acceptingStates :: [MultiState]
    , transitions     :: PowerSetDFATransitions
    , seen            :: [MultiState] }

emptyEnv :: Env
emptyEnv = Env {
    acceptingStates = [],
    transitions     = DMap.empty Map.empty,
    seen            = []
}
eps :: Char
eps = '\949'

fromNFAMulti :: (NFA, EpsClosure) -> PowerSetDFA
fromNFAMulti pair@(nfa@(NFA start _ ts),epsClosure) =
    PowerSetDFA initStart (acceptingStates finalState) (transitions finalState)
    where
        initStart  = DMap.lookup start epsClosure
        finalState = S.execState (fromNFAMulti' pair initStart) emptyEnv

{-
Helper-functions needed:
- get all the symbols used by a multi-state
- For one symbol, get all the states reachable as a multi-state
- For all states in a multi-state, get the epsilon-clojure
- Check if the final multi-state contains the accepting-state
-}

fromNFAMulti' :: (NFA, EpsClosure) -> MultiState -> S.State Env ()
fromNFAMulti' dtype@(nfa@(NFA start _ ts),epsClosure) currentState = do
    -- mark current state as seen
    S.modify (\s -> s{seen = currentState:seen s})
    let symbols = getSymbols ts currentState
    let reachableStates = foldr (\char accu -> Map.insert char (getReachableStates (ts, currentState) char) accu) Map.empty symbols
    let reachableStatesWithClosure = Map.map (\multiState -> getMSClosure (epsClosure, multiState)) reachableStates 
    -- mark all the computed multistates as reachable for the current state by putting them into `transitions`
    S.modify(\s -> s{transitions = DMap.insert currentState (const reachableStatesWithClosure) (transitions s)})
    -- if an accepting state is part of the `currentState`, put `currentstate` into env
    when (hasAcceptState (nfa, currentState)) $ S.modify (\s-> s{ acceptingStates = currentState : acceptingStates s } )
    -- Call recursively on reachable multistates which have not been seen yet
    seen' <- S.gets seen
    let newStates = filter (`notElem` seen') (Map.elems reachableStatesWithClosure)
    mapM_ (fromNFAMulti' dtype) newStates

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

