module NFA ( getNFA
           , stringify
           )
where

import Parser
import qualified Data.Map as Map

import qualified Control.Monad.State as S

type State = Int
type Transitions = DefaultMap State (DefaultMap Char [State])

-- Start-State, Accept-State, Transitions
data NFA = NFA State State Transitions deriving Show
data Env = Env
    { transitions :: Transitions
    , stateCount  :: Int }

type DefaultMap k v = (Map.Map k v, v)

insert' :: Ord k => k -> (v -> v) -> DefaultMap k v -> DefaultMap k v
insert' k f (map, d) = case Map.lookup k map of
    Nothing    -> (Map.insert k (f d) map, d)
    Just found -> (Map.insert k (f found) map, d)

empty' :: v -> DefaultMap k v
empty' d = (Map.empty, d)

emptyEnv :: Env
emptyEnv = Env
    { transitions = empty' (empty' [])
    , stateCount  = 0 }

-- Kleene Regex
-- Concat Regex Regex

compileNFA :: Regex -> S.State Env NFA
compileNFA reg = do
    init <- getNextState
    (start, end) <- constructNFA init reg
    ts <- S.gets transitions
    return $ NFA start end ts

getNFA :: Regex -> NFA
getNFA reg = fst $ S.runState (compileNFA reg) emptyEnv


{-
TODO:
put start/end into the state monad
=> constructNFA returns the start and end of its expression
=> when we enter a sub-automaton, we can use these returned values to make our logic sound
    => so we kind of have to reset them when we enter a new automaton

=====> not sure if we actually have to put it in the state monad; I think we can actually just keep them as function parameters/return values in constructNFA
-}

constructNFA :: State -> Regex -> S.State Env (State, State)
constructNFA currentState Epsilon = do
    nextState <- getNextState
    S.modify(\e -> e{ transitions = insert' currentState ( insert' 'ε' (\nextStates -> nextState : nextStates) ) (transitions e) })
    return (currentState, nextState)

constructNFA currentState Dot = do
    nextState <- getNextState
    S.modify(\e -> e{ transitions = insert' currentState ( insert' '.' (\nextStates -> nextState : nextStates) ) (transitions e) })
    return (currentState, nextState)

constructNFA currentState (Literal l) = do
    nextState <- getNextState
    S.modify(\e -> e{ transitions = insert' currentState ( insert' l (\nextStates -> nextState : nextStates) ) (transitions e) })
    return (currentState, nextState)

constructNFA currentState (Kleene exp) = do
    localFinalState <- getNextState
    kleeneStart <- getNextState

    -- connect current state with kleeneStart
    S.modify(\e -> e{ transitions = insert' currentState ( insert' 'ε' (\nextStates -> kleeneStart : nextStates) ) (transitions e) })
    
    -- create the kleeneEnd
    (_, innerEnd) <- constructNFA kleeneStart exp

    -- connect innerEnd with kleeneStart
    S.modify(\e -> e{ transitions = insert' innerEnd ( insert' 'ε' (\nextStates -> kleeneStart : nextStates) ) (transitions e) })

    -- connect innerEnd with localFinal
    S.modify(\e -> e{ transitions = insert' innerEnd ( insert' 'ε' (\nextStates -> localFinalState : nextStates) ) (transitions e) })

    -- connect current state with localFinal
    S.modify(\e -> e{ transitions = insert' currentState ( insert' 'ε' (\nextStates -> localFinalState : nextStates) ) (transitions e) })
    

    return (currentState, localFinalState)


    {-
    4 <- getNextState
    2 <- constructNFA epsilon 
    3 <- constructNFA exp
    make transition from 3 to 2
    make transition from 3 to 4
    make transition from 1 to 4
    -}


getNextState :: S.State Env State
getNextState = do
    s <- S.get
    let c = stateCount s
    S.modify (const s{ stateCount = c + 1})
    return c

stringify :: Regex -> String
stringify = undefined