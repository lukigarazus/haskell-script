module JSInterpreter where

import AST
import JSTokens

getExpressionChildren :: Expression -> [Expression]
getExpressionChildren expr = case expr of
  ArrayExpression elements -> elements
  ArrowFunctionExpression _ _ -> []
  BinaryExpression l r _ -> [l, r]
  BracketedExpression expr -> [expr]
  CallExpression callee args -> args ++ [callee]
  EmptyExpression -> []
  FunctionExpression _ _ -> []
  Identifier _ -> []
  Literal _ -> []
  MemberExpression arg mem -> [arg]
  ObjectExpression pairs -> concatMap (\(k, v) -> [k, v]) pairs
  UpdateExpression name _ -> [name]

allEvaluated :: [Expression] -> Bool
allEvaluated = all isExpressionFullyEvaluated

isExpressionFullyEvaluated :: Expression -> Bool
isExpressionFullyEvaluated expr = case expr of
  --   ArrayExpression elements -> elements
  --   ArrowFunctionExpression _ _ -> []
  --   BinaryExpression l r _ -> [l, r]
  --   BracketedExpression expr -> [expr]
  CallExpression callee args -> False
  --   EmptyExpression -> []
  --   FunctionExpression _ _ -> []
  Identifier _ -> False
  --   Literal _ -> []
  MemberExpression arg mem -> False
  ObjectExpression pairs -> False
  UpdateExpression name _ -> False
  _ -> allEvaluated (getExpressionChildren expr)

isStatementFullyEvaluated :: Statement -> Bool
isStatementFullyEvaluated stm = case stm of
  ReturnStatement expr -> isExpressionFullyEvaluated expr
  ContinueStatement -> False
  BreakStatement -> False
  ForStatement {} -> False
  ExpressionStatement expr -> isExpressionFullyEvaluated expr
  VariableDeclaration decs -> all isStatementFullyEvaluated decs
  VariableDeclarator {} -> False
  FunctionDeclaration {} -> False
  IfStatement pre body alter -> isExpressionFullyEvaluated pre && False
  BlockStatement sts -> all isStatementFullyEvaluated sts
  EmptyStatement -> True

simplifyExpression :: Expression -> Expression
simplifyExpression expr = case expr of
  _ -> expr

typeOfExpression :: Expression -> Type
typeOfExpression expr = case expr of
  Literal l -> case l of
    StringLiteral _ -> String
    NumericLiteral _ -> Number
    NullLiteral -> Null
    BooleanLiteral _ -> Boolean
  Identifier str -> case str of
    "undefined" -> Undefined
    _ -> error "typeOf Identifier should not happen at all, but for undefined"

castType :: Expression -> Expression -> Expression
castType e1 e2 = e1

interpretJavaScript jscode = case getAST jscode of
  Nothing -> True
  Just (Program sts) -> case sts of
    [a] -> isStatementFullyEvaluated a
    _ -> True