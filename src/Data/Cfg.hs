module Data.Cfg
       ( Terminal
       , Nonterminal
       , Symbol(..)
       , Rule(..)
       , CFG(..)
       , nullables 
       , noncontractingRules
       , noncontractingCFG
       ) where

import qualified Data.Set as S
import qualified Data.List as L
import Data.Maybe (isNothing)
import Control.Monad (replicateM)

import Debug.Trace

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
         deriving Show

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

   merge :: [Symbol] -> [Maybe Nonterminal] -> Nonterminal -> [Symbol]
   merge ss nts nt = if lp == []
                     then remaining
                     else lp ++ s ++ (merge (tail remaining) (tail nts) nt)
     where
       lp = fst $ L.break (== (Nt nt)) ss 
       remaining = snd $ L.break (== (Nt nt)) ss
       s = if isNothing $ head nts
           then []
           else [Nt nt]

   genRules :: Rule -> Nonterminal -> S.Set Rule 
   genRules (Rule ntr rp) nt = S.fromList [Rule ntr $ merge rp c nt | c <- combinations]
     where
       oc = length $ filter (\x -> x == Nt nt) rp
       combinations = replicateM oc [Just nt, Nothing]
       

   generatedRs = S.unions [S.unions $ map (\r -> genRules r nnt) $
                           S.toList $ rsContainingNtInRP nnt nrs
                 | nnt <- S.toList nullnts]
   
noncontractingCFG :: CFG -> CFG
noncontractingCFG (CFG nts ts rs initial) = if initial `S.member` nts'
                                            then CFG (nts' `S.union` (S.singleton 'N')) ts rs' initial'
                                            else CFG nts' ts rs' initial'
  where
    nts' = nullables (CFG nts ts rs initial)
    rs' = if initial `S.member` nts'
          then S.fromList [Rule 'N' [Nt 'S'], Rule 'N' []] `S.union` noncontractingRules rs nts'
          else noncontractingRules rs nts'
    initial' = if initial `S.member` nts'
               then 'N'
               else initial
