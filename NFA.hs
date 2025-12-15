module NFA ( NFA,
             getNFA
           , stringifyNFA
           , epsilonClosure
           )
where

import Parser
import DMap
import Datatypes
import qualified Data.Map as M

import Prelude hiding (lookup)
import qualified Control.Monad.State as S
import Data.Type.Equality (outer)
import qualified Data.IntMap as Map

-- Start-State, Accept-State, Transitions

data Env = Env
    { transitions :: NFATransitions
    , stateCount  :: Int }

emptyEnv :: Env
emptyEnv = Env
    { transitions = empty (empty [])
    , stateCount  = 0 }

compileNFA :: Regex -> S.State Env NFA
compileNFA reg = do
    (start, end) <- constructNFA reg
    ts <- S.gets transitions
    return $ NFA start end ts

getNFA :: Regex -> NFA
getNFA reg = fst $ S.runState (compileNFA reg) emptyEnv

eps :: Char
eps = '\949'

dot :: Char
dot = '•'


-- Construction of Epsilon-NFA
constructNFA :: Regex -> S.State Env (State, State)
constructNFA Epsilon = do
    currentState <- getNextState
    nextState <- getNextState
    createEdge currentState nextState eps
    return (currentState, nextState)

constructNFA Dot = do
    currentState <- getNextState
    nextState <- getNextState
    createEdge currentState nextState '.'
    return (currentState, nextState)

constructNFA (Literal l) = do
    currentState <- getNextState
    nextState <- getNextState
    createEdge currentState nextState l
    return (currentState, nextState)

constructNFA (Kleene exp) = do
    currentState <- getNextState
    localFinalState <- getNextState

    -- create the inner automaton
    (innerStart, innerEnd) <- constructNFA exp

    -- connect current state with inner start
    createEdge currentState innerStart eps

    -- connect innerEnd with inner start
    createEdge innerEnd innerStart eps

    -- connect innerEnd with localFinal
    createEdge innerEnd localFinalState eps

    -- connect current state with localFinal
    createEdge currentState localFinalState eps

    return (currentState, localFinalState)

constructNFA (Concat l r) = do
    (lStart, lEnd) <- constructNFA l
    (rStart, rEnd) <- constructNFA r

    -- connect lEnd to rStart
    createEdge lEnd rStart eps

    return (lStart, rEnd)


createEdge :: State -> State -> Char -> S.State Env ()
createEdge origin target c =
    S.modify(\e ->
        e{ transitions = insert origin
            (insert c (\nextStates -> target : nextStates) )
            (transitions e)
        })

getNextState :: S.State Env State
getNextState = do
    s <- S.get
    let c = stateCount s
    S.modify (const s{ stateCount = c + 1})
    return c

-- create graphviz-compatible string from an NFA
stringifyNFA :: NFA -> String
stringifyNFA (NFA start end ts) =
    unlines [ "digraph G{"
           , headers
           , staticBody
           , body, "}" ]

    where
        headers = unlines $ map indent
            [ "{"
            , "S" ++ show end ++ "[shape=doublecircle]"
            , "}"
            ]

        staticBody = unlines $ map indent
            [ "empty [label=\"\", shape=none,height=.0,width=.0]"
            , "empty -> S" ++ show start ]

        indent xs = if null xs then "" else "\t" ++ xs

        body = transitionsToString ts
        transitionsToString :: NFATransitions -> String
        transitionsToString ts = go (toList ts) ""
            where
                go :: [(State, DefaultMap Char [State])] -> String -> String
                go [] accu       = accu
                go ((state, dMap):tss) accu = go tss (accu ++ inner state (toList dMap))

        -- TODO: use unlines
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
        createEdge s t l = "\tS" ++ show s ++ " -> S" ++ show t ++ " [label=" ++ formatChar l ++ "]\n"
            where
                formatChar :: Char -> String
                formatChar '.' = [dot]
                formatChar l = [l]


-- compute the epsilon clojure of a given Epsilon-NFA
epsilonClosure :: NFA -> EpsClosure
-- TODO: prettify, right now we manually add the AcceptState because it is not in the 
-- transitions; we might have to do the same for the automaton that only contains the starting node (automton for the ""-regex)
epsilonClosure (NFA start end (outer,d)) = insert end (const [end]) (go adjacencyEpsList)
    where 
        adjacencyEpsList :: DefaultMap State [State]
        adjacencyEpsList = create (M.fromList $ map (\(s,ts) -> (s, lookup eps ts)) (M.toList outer)) []

        go :: DefaultMap State [State] -> DefaultMap State [State]
        go m@(map, _) = create (foldr (\elem accu -> M.union accu (fst . fst $ S.execState (dfs elem elem m) (emptyDFSState elem))) M.empty (M.keys map)) []



emptyDFSState :: State -> (DefaultMap State [State], [State])
emptyDFSState initial = (insert initial (const [initial]) (empty []), [])

-- TODO: prettify
dfs :: State -> State -> DefaultMap State [State] -> S.State (DefaultMap State [State], [State]) ()
dfs target current adjacency = do
    (accu, seen) <- S.get
    let neighbours = filter (`notElem` seen) (lookup current adjacency)
    let newAccu = insert target (++ neighbours) accu
    S.modify $ const(newAccu, seen ++ neighbours)
    go neighbours
    where
        go [] = return ()
        go (n:ns) = dfs target n adjacency
