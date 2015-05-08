module Data.Cfg
       ( Terminal
       , Nonterminal
       , Symbol(..)
       , Rule(..)
       , CFG(..)
       ) where

import qualified Data.Set as S

type Terminal = Char
type Nonterminal = Char
data Symbol = T Terminal | Nt Nonterminal
            deriving (Show, Eq, Ord)
data Rule = Rule Nonterminal [Symbol]
          deriving (Eq, Ord)
data CFG = CFG
           { nonTerminals :: S.Set Nonterminal
           , terminals :: S.Set Terminal
           , productions :: S.Set Rule
           , initial :: Nonterminal
           }

instance Show Rule where
  show (Rule nt rp) = show nt ++ " -> " ++ show rp ++ "\n"

-- TODO: full version
nullables :: CFG -> S.Set Nonterminal
nullables (CFG nts _ rs _) = knownNullables
  where
    knownNullables = S.map (\(Rule nt _) -> nt) knownNullableRules
    knownNullableRules = S.filter (\(Rule nt rp) -> rp == []) rs

noncontractingRules :: S.Set Rule -> S.Set Rule
noncontractingRules rs = undefined

noncontractingCFG :: CFG -> CFG
noncontractingCFG g = undefined
