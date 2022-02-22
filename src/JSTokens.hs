module JSTokens where

data Type = Number | String | Null | Undefined | Boolean | Symbol deriving (Eq, Show)

--------------------------------------------------------------------------

data Literal = BooleanLiteral Bool | StringLiteral String | NumericLiteral Int | NullLiteral | TemplateLiteral {templateLiteral :: String} deriving (Show, Eq)

data Operator = Plus | Minus | Multiply | Divide deriving (Show, Eq)

data UpdateOperator = Increment | Decrement deriving (Show, Eq)

data Expression = MemberExpression {memberArgument :: Expression, member :: String} | UpdateExpression {updateName :: Expression, updateOperator :: UpdateOperator} | Identifier {identifierName :: String} | BracketedExpression Expression | Literal Literal | ObjectExpression {objectPairs :: [(Expression, Expression)]} | CallExpression {callee :: Expression, callArgs :: [Expression]} | ArrayExpression {arrayElements :: [Expression]} | FunctionExpression {args :: [String], functionBody :: Statement} | ArrowFunctionExpression {args :: [String], arrowBody :: Statement} | BinaryExpression {left :: Expression, right :: Expression, operator :: Operator} | EmptyExpression deriving (Show, Eq)

data VariableDeclaratorType = Const | Var | Let deriving (Show, Eq)

data Statement = ReturnStatement {returnArgument :: Expression} | ContinueStatement | BreakStatement | ForStatement {forTest :: Maybe Expression, forInit :: Maybe Expression, forUpdate :: Maybe Expression, forBody :: Statement} | ExpressionStatement Expression | VariableDeclaration {declarators :: [Statement]} | VariableDeclarator {declaratorType :: VariableDeclaratorType, variableName :: Expression, value :: Maybe Expression} | FunctionDeclaration {functionDeclarationName :: String, declarationArgs :: [String], declarationBody :: Statement} | IfStatement {predicate :: Expression, ifBody :: Statement, alternate :: Maybe Statement} | BlockStatement {blockBody :: [Statement]} | EmptyStatement deriving (Show, Eq)

newtype Program = Program [Statement]

instance Show Program where
  show (Program statements) = "Program\n" ++ concatMap (("    " ++) . show) statements