import Text.ParserCombinators.Parsec hiding (State)
import Text.ParserCombinators.Parsec.Char
import qualified Data.Set as S
import Data.Char
import Data.Cfg

ruleParser :: Parser Rule
ruleParser = do
  let c2Symbol :: Char -> Symbol
      c2Symbol c = if isUpper c
                   then Nt c
                   else T c
  t <- upper
  between spaces spaces $ string "->"
  chain <- many letter
  return $ Rule t $ map c2Symbol chain

main = do
  let nonterminals = S.fromList ['S', 'A', 'B', 'C']
      terminals = S.fromList ['a', 'b', 'c']
      rules =
        [ "S -> BC"
        , "S -> Ab"
        , "B -> "
        , "C -> c"
        , "A -> Aa"
        , "A -> "
        , "S -> AB"
        ]
      nonterminals2 = S.fromList ['S']
      terminals2 = S.fromList ['a', 'b']
      rules2 =
        [ "S -> aSbS"
        , "S -> bSaS"
        , "S -> "
        ]
      rs = S.fromList $ map (\s -> case parse ruleParser "test" s of
                                    Left err -> undefined
                                    Right res -> res) rules
      rs2 = S.fromList $ map (\s -> case parse ruleParser "test" s of
                                     Left err -> undefined
                                     Right res -> res) rules2
      initial = 'S'
      cfg = CFG nonterminals terminals rs initial
      cfg2 = CFG nonterminals2 terminals2 rs2 initial
  print rs
  print $ noncontractingCFG cfg2

