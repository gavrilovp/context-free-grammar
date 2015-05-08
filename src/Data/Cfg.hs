module Data.Cfg
       ( Terminal
       , Nonterminal
       , Symbol(..)
       , Rule(..)
       , CFG(..)
       , nullables 
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

nullables :: CFG -> S.Set Nonterminal
nullables (CFG nts _ rs _) = go knownNullables
  where
    go :: S.Set Nonterminal -> S.Set Nonterminal
    go nts' = if nts'' == S.empty
              then nts'
              else go $ nts' `S.union` nts''
      where
        nts'' = calculatedNullables nts'
    
    knownNullables = S.map (\(Rule nt _) -> nt) knownNullableRules
    knownNullableRules = S.filter (\(Rule _ rp) -> rp == []) rs

    isKnownNullable :: Symbol -> Bool
    isKnownNullable (Nt nt) = nt `S.member` knownNullables
    isKnownNullable _ = False
    
    calculatedNullables :: S.Set Nonterminal -> S.Set Nonterminal
    calculatedNullables knlls = ns
      where
        nts' = nts S.\\ knlls
        ns = S.filter (\nt -> any (all isKnownNullable) $ rulesRpNt nt) nts'

    rulesRpNt :: Nonterminal -> [[Symbol]]
    rulesRpNt nt = [rp | (Rule nt' rp) <- S.toList rs, nt' == nt]

noncontractingRules :: S.Set Rule -> S.Set Rule
noncontractingRules rs = undefined

noncontractingCFG :: CFG -> CFG
noncontractingCFG g = undefined
