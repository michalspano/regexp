module Datatypes  where

import DMap
import Data.Map as Map

-- TODO: implement instance Show with stringify/pretty prints
type State = Int
type MultiState = [State]
type EpsClosure = DefaultMap State [State]

type DFATransitions = DefaultMap State (Map Char State)
data DFA = DFA State [State] DFATransitions deriving Show 

type NFATransitions = DefaultMap State (DefaultMap Char [State])
data NFA = NFA State State NFATransitions deriving Show

type PowerSetDFATransitions = DefaultMap MultiState (Map Char MultiState)
data PowerSetDFA = PowerSetDFA MultiState [MultiState] PowerSetDFATransitions deriving Show