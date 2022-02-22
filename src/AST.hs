module AST where

import JSParser
import Text.ParserCombinators.ReadP

getAST str = case readP_to_S parseProgram str of
  [] -> Nothing
  [a] -> Just $ fst a
  _ -> Nothing