module Data.Cfg
       ( Terminal
       , Nonterminal
       , Symbol(..)
       , Rule(..)
       , CFG(..)
       , nullables 
       , noncontractingRules
       ) where

import qualified Data.Set as S
import qualified Data.List as L

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
    knownNullableRules = nullableRules rs

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

nullableRules :: S.Set Rule -> S.Set Rule
nullableRules rs = S.filter (\(Rule _ rp) -> rp == []) rs

noncontractingRules :: S.Set Rule -> S.Set Nonterminal -> S.Set Rule
noncontractingRules rs nullnts = generatedRs
  where
   knownNullableRules = nullableRules rs
   nrs = rs S.\\ knownNullableRules

   isNtInRP :: Nonterminal -> Rule -> Bool
   isNtInRP nt (Rule _ rp) = elem (Nt nt) rp 
   isNtInRP _ _ = False

   rsContainingNtInRP :: Nonterminal -> S.Set Rule -> S.Set Rule
   rsContainingNtInRP nt = S.filter (isNtInRP nt)

   genRules :: Rule -> Nonterminal -> S.Set Rule 
   genRules (Rule ntr rp) nt
     | not $ elem (Nt nt) rp = S.fromList [Rule ntr rp]
     | otherwise = S.singleton (Rule ntr rp) `S.union`
                   genRules (Rule ntr $ L.delete (Nt nt) rp) nt

   generatedRs = S.unions [S.unions $ map (\r -> genRules r nnt) $
                           S.toList $ rsContainingNtInRP nnt nrs
                 | nnt <- S.toList nullnts]
   
noncontractingCFG :: CFG -> CFG
noncontractingCFG (CFG nts _ rs initial) = 
  undefined
