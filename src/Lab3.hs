import Control.Monad.State
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
import Data.Cfg

import Debug.Trace

type St = State (M.Map Int (S.Set (Rule, Int))) Int -- ^ Original position,  

scan :: Terminal -> St 
scan t = do
  st <- get
  let k = maximum $ M.keys st
      rs = st M.! k
      v = S.filter p rs
      p = \((Rule lhs rhs), point) -> length rhs > point && (T t) == rhs !! point
      f = \(r, point) -> (r, point + 1)
      v' = S.map f v
  if null v'
    then put st
    else put $ M.insert (k+1) v' st
  return $ k + 1

predict :: Set Rule -> St
predict rs = do
  st <- get
  let k = maximum $ M.keys st
      rsNt = st M.! k
      nts = [rhs !! pos | ((Rule lhs rhs), pos) <- S.toList rsNt
            , length rhs > pos && case (rhs !! pos) of
               (Nt nt) -> True
               otherwise -> False]
      rsSt = concat [S.toList $ S.filter (\(Rule lhs _) -> (Nt lhs) == nt) rs | nt <- nts]
      newRs = S.fromList $ zip rsSt $ repeat 0
      v = S.union newRs $ st M.! k
      oldSt = st
  put $ M.insert k v st
  st <- get
  if oldSt /= st
    then predict rs
    else return $ maximum $ M.keys st

complete :: St
complete = do
  st <- get
  let k = maximum $ M.keys st
      completeRs = S.filter p $ st M.! k
      p = \(Rule lhs rhs, pos) -> length rhs == pos
      nts = S.map (\(Rule lhs _, _) -> lhs) completeRs
      remainingRs = M.filterWithKey (\key _ -> key /= k) st
      newRs = S.unions [S.filter pred rsSet | (_, rsSet) <- M.toList remainingRs]
      pred = \(Rule lhs rhs, pos) ->
              length rhs > pos && case rhs !! pos of
               (Nt nt) -> nt `elem` nts
               otherwise -> False
      newRs' = S.map (\(r, k') -> (r, k' + 1)) newRs
      v = S.union newRs' $ st M.! k
      oldSt = st
  put $ M.insert k v st
  st <- get
  if oldSt /= st
     then complete
     else return k

earley' :: Set Rule -> [Terminal] -> St
earley' rs [] = do
  st <- get
  let (_, st') = runState (predict rs) st
      (_, st'') = runState complete st'
  put st''
  st <- get
  traceShow st $ return $ maximum $ M.keys st
earley' rs (t:ts) = do
  st <- get
  let (_, st') = runState (predict rs) st
      (_, st'') = runState complete st'
      (_, st''') = runState (scan t) st''
  put st'''
  st <- get
  earley' rs ts

earley :: CFG -> [Terminal] -> Bool
earley (CFG _ _ rs _) words =
  let initialSt = M.singleton 0 (S.singleton (Rule "S" [Nt "E"], 0))
      (k, st) = runState (earley' rs words) initialSt
  in
   traceShow st $ S.member (Rule "S" [Nt "E"], 1) $ st M.! k


main = do
  let nts = S.fromList ["S", "E", "T", "F"]
      ts = S.fromList ["+", "*", "(", ")", "a"]
      rs = S.fromList [ (Rule "S" [Nt "E"])
                      , (Rule "E" [Nt "T", T "+", Nt "E"])
                      , (Rule "E" [Nt "T"])
                      , (Rule "T" [Nt "F", T "*", Nt "T"])
                      , (Rule "T" [Nt "F"])
                      , (Rule "F" [T "(", Nt "E", T ")"])
                      , (Rule "F" [T "a"])
                      ]
      cfg = CFG nts ts rs "E"
      words = ["(", "a", "+", "a", ")", "*", "a"]
      nts2 = S.fromList ["S", "E", "M", "T"]
      ts2 = S.fromList ["+", "*", "1", "2", "3", "4"]
      rs2 = S.fromList [ (Rule "S" [Nt "E"])
                       , (Rule "E" [Nt "E", T "+", Nt "M"])
                       , (Rule "E" [Nt "M"])
                       , (Rule "M" [Nt "M", T "*", Nt "T"])
                       , (Rule "M" [Nt "T"])
                       , (Rule "T" [T "1"])
                       , (Rule "T" [T "2"])
                       , (Rule "T" [T "3"])
                       , (Rule "T" [T "4"])
                       ]
      cfg2 = CFG nts2 ts2 rs2 "E"
      words2 = ["2", "+", "3", "*", "4"]
  print words2
  print $ earley cfg2 words2
  print words
  print $ earley cfg words
