module NFA ( getNFA
           , stringifyNFA
           )
where

import Parser
import qualified Data.Map as Map

import qualified Control.Monad.State as S
import Data.Type.Equality (outer)

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

toList' :: Ord k => DefaultMap k v -> [(k, v)]
toList' = Map.toList . fst

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
    (start, end) <- constructNFA reg
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

constructNFA :: Regex -> S.State Env (State, State)
constructNFA Epsilon = do
    currentState <- getNextState
    nextState <- getNextState
    S.modify(\e -> e{ transitions = insert' currentState ( insert' 'ε' (\nextStates -> nextState : nextStates) ) (transitions e) })
    return (currentState, nextState)

constructNFA Dot = do
    currentState <- getNextState
    nextState <- getNextState
    S.modify(\e -> e{ transitions = insert' currentState ( insert' '.' (\nextStates -> nextState : nextStates) ) (transitions e) })
    return (currentState, nextState)

constructNFA (Literal l) = do
    currentState <- getNextState
    nextState <- getNextState
    S.modify(\e -> e{ transitions = insert' currentState ( insert' l (\nextStates -> nextState : nextStates) ) (transitions e) })
    return (currentState, nextState)

constructNFA (Kleene exp) = do
    currentState <- getNextState
    localFinalState <- getNextState
    -- kleeneStart <- getNextState

    -- create the kleeneEnd
    (innerStart, innerEnd) <- constructNFA exp

    -- connect current state with kleeneStart
    S.modify(\e -> e{ transitions = insert' currentState ( insert' 'ε' (\nextStates -> innerStart : nextStates) ) (transitions e) })

    -- connect innerEnd with kleeneStart
    S.modify(\e -> e{ transitions = insert' innerEnd ( insert' 'ε' (\nextStates -> innerStart : nextStates) ) (transitions e) })

    -- connect innerEnd with localFinal
    S.modify(\e -> e{ transitions = insert' innerEnd ( insert' 'ε' (\nextStates -> localFinalState : nextStates) ) (transitions e) })

    -- connect current state with localFinal
    S.modify(\e -> e{ transitions = insert' currentState ( insert' 'ε' (\nextStates -> localFinalState : nextStates) ) (transitions e) })
    

    return (currentState, localFinalState)

constructNFA (Concat l r) = do
    lStart <- getNextState
    rStart <- getNextState

    (lStart, lEnd) <- constructNFA l
    (rStart, rEnd) <- constructNFA r

    -- localAccept <- getNextState

    -- connect this state with lStart
    -- S.modify(\e -> e{ transitions = insert' lStart ( insert' 'ε' (\nextStates -> lStart : nextStates) ) (transitions e) })

    -- connect lEnd to rStart
    S.modify(\e -> e{ transitions = insert' lEnd ( insert' 'ε' (\nextStates -> rStart : nextStates) ) (transitions e) })

    -- connect rEnd to localAccept
    -- S.modify(\e -> e{ transitions = insert' rEnd ( insert' 'ε' (\nextStates -> localAccept : nextStates) ) (transitions e) })

    return (lStart, rEnd)


getNextState :: S.State Env State
getNextState = do
    s <- S.get
    let c = stateCount s
    S.modify (const s{ stateCount = c + 1})
    return c

-- create graphviz-compatible string from an NFA
stringifyNFA :: NFA -> String
stringifyNFA (NFA start end ts) =
    "digraph G{\n" ++ 
        "\t{\n" ++ 
            -- "\t\tN" ++ show start ++ "[shape=star]" ++ "\n" ++
            "\t\tS" ++ show end ++ "[shape=doublecircle]" ++ "\n" ++
        "\t}\n" ++
        "empty [label= \"\", shape=none,height=.0,width=.0]\n" ++
        "\tempty -> S" ++ show start ++ "\n" ++
        body ++
    "}"

    where
        body = transitionsToString ts
        transitionsToString :: Transitions -> String
        transitionsToString ts = go (toList' ts) ""
            where
                go :: [(State, DefaultMap Char [State])] -> String -> String
                go [] accu       = accu
                go ((state, dMap):tss) accu = go tss (accu ++ inner state (toList' dMap))

        inner :: State -> [(Char, [State])] -> String 
        inner start xs =
            foldr
                (\(ch, sts) outerAcc ->
                    outerAcc ++ foldr (\target innerAcc ->
                        innerAcc ++ createEdge start target ch ++ (helper innerAcc)) "" sts ++ (helper outerAcc))
                ""
                xs

        -- TODO: prettify this shit
        helper li = if null li then "" else "\n"
        createEdge :: State -> State -> Char -> String
        createEdge s t l = "\tS" ++ show s ++ " -> S" ++ show t ++ " [label=" ++ [l] ++ "]\n"