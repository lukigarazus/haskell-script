module JSParser where

import Control.Applicative (Alternative ((<|>)))
import Data.Char
import Debug.Trace
import JSTokens
import Text.ParserCombinators.ReadP

---------------------------------------------------------------------------------

data ProgramContext = ProgramContext {isFunction :: Bool, isClassMethod :: Bool, isLoop :: Bool}

----------------------------------------------------------------------------------

setIsFunction ctx val = ProgramContext val (isClassMethod ctx) (isLoop ctx)

setIsLoop ctx = ProgramContext (isFunction ctx) (isClassMethod ctx)

----------------------------------------------------------------------------------

parseStringLike ch = between (char ch) (char ch) (munch (/= ch))

endsWith :: ReadP a -> ReadP a -> ReadP a
endsWith p e = p <* e

openingBracket = choice $ map char "({[<"

closingBracket = choice $ map char ")}]>"

brackets ob cb = between (char ob *> skipNewlinesAndSpaces) (skipNewlinesAndSpaces *> char cb)

bracketsAndCommas ob cb p = brackets ob cb (sepBy p (char ','))

bracketedExpression :: ProgramContext -> ReadP Expression
bracketedExpression ctx = BracketedExpression <$> brackets '(' ')' (parseExpression ctx)

parseBlockableStatement :: ProgramContext -> ReadP Statement
parseBlockableStatement ctx = parseBlockStatement ctx <++ parseExpressionStatement ctx

nothingAfter = (() <$ (char ';' <|> char '\n')) <|> eof

skipNewlines = skipMany (char '\n')

-- | Always use with /*> or </*!
skipNewlinesAndSpaces = munch (\x -> x == ' ' || x == '\n')

------------------------------------------------------------------------------------

variableDeclarators = ["const", "let", "var"]

unaryOperators = ["delete", "typeof", "instanceof", "await"]

loopKeywords = ["continue", "break"]

functionKeywords = ["return", "yield"]

controlKeywords = ["if", "while", "for", "in", "do"]

declarationKeywords = ["function", "class"]

values = ["true", "false"]

misc = ["null"]

keywords = concat [values, unaryOperators, variableDeclarators, declarationKeywords, controlKeywords, misc, loopKeywords, functionKeywords]

parseKeyword = choice $ map string keywords

------------------------------------------------------------------------------------
parseStringLiteral = StringLiteral <$> (parseStringLike '\'' <|> parseStringLike '"')

-- parseTemplateLiteral ctx = TemplateLiteral <$> (char '`' *> (many ((StringLiteral <$> satisfy isAlphaNum) <|> (char '$' *> char '{' *> parseExpression ctx <* char '}'))) <* char '`')

parseNumericLiteral = NumericLiteral . read <$> munch1 isDigit

parseNullLiteral = NullLiteral <$ string "null"

parseBooleanLiteral = (BooleanLiteral True <$ string "true") <|> (BooleanLiteral False <$ string "false")

parseLiteral :: ReadP Literal
parseLiteral = parseStringLiteral <|> parseNumericLiteral <|> parseNullLiteral <|> parseBooleanLiteral

parseExpressionLiteral = Literal <$> parseLiteral

parseFunctionExpression :: ProgramContext -> ReadP Expression
parseFunctionExpression ctx = do
  args <- string "function" *> bracketsAndCommas '(' ')' (munch1 isAlphaNum)
  body <- parseBlockStatement ctx
  return $ FunctionExpression args body

parseArrowFunctionExpression :: ProgramContext -> ReadP Expression
parseArrowFunctionExpression ctx = do
  args <- bracketsAndCommas '(' ')' (munch1 isAlphaNum)
  skipSpaces
  string "=>"
  skipSpaces
  body <- parseBlockableStatement (setIsFunction ctx True)
  return $ ArrowFunctionExpression args body

parseCallExpression :: ProgramContext -> ReadP Expression
parseCallExpression ctx = do
  callee <- parseIdentifier <++ bracketedExpression ctx -- or member, or another call expression...
  args <- bracketsAndCommas '(' ')' (parseExpression ctx)
  return $ CallExpression callee args

parseIdentifier = do
  keyword <- parseKeyword <++ string ""
  if null keyword then string "" else pfail
  start <- satisfy isAlpha
  rest <- munch isAlphaNum
  return $ Identifier (start : rest)

b1 :: ProgramContext -> ReadP Expression
b1 ctx =
  do
    left <- b2 ctx <++ parseExpressionNoBinary ctx
    skipSpaces
    operator <-
      ( \x -> case x of
          '+' -> Plus
          '-' -> Minus
        )
        <$> choice (map char "+-")
    skipSpaces
    right <- b1 ctx <++ parseExpressionNoBinary ctx
    return $ BinaryExpression left right operator
    <++ b2 ctx

b2 :: ProgramContext -> ReadP Expression
b2 ctx =
  do
    left <- parseExpressionNoBinary ctx
    skipSpaces
    operator <-
      ( \x -> case x of
          '*' -> Multiply
          '/' -> Divide
        )
        <$> choice (map char "*/")
    skipSpaces
    right <- parseExpressionNoBinary ctx
    return $ BinaryExpression left right operator

parseBinaryExpression ctx = b1 ctx

parseEmptyExpression = EmptyExpression <$ (skipSpaces *> string "")

parseUpdateExpression = do
  name <- parseIdentifier -- or member
  operator <-
    ( \x -> case x of
        "++" -> Increment
        _ -> Decrement
      )
      <$> ( choice $
              map string ["++", "--"]
          )
  return $ UpdateExpression name operator

-- constructMember steps = case steps of
--   (x:y:r) -> MemberExpression
--   [x,y] -> Me

parseMemberExpression :: ProgramContext -> ReadP Expression
parseMemberExpression ctx = do
  steps <- sepBy1 (parseExpressionNoMember ctx) (char '.')
  if length steps > 1 then string "" else pfail

  return EmptyExpression

parseObjectExpressionKeyValue :: ProgramContext -> ReadP (Expression, Expression)
parseObjectExpressionKeyValue ctx = do
  skipNewlinesAndSpaces
  key1 <- option EmptyExpression (Literal . StringLiteral <$> munch1 isAlphaNum <* skipSpaces <* char ':')
  key2 <- if key1 == EmptyExpression then brackets '[' ']' (parseExpression ctx) <* skipSpaces <* char ':' else EmptyExpression <$ string ""
  value <- skipSpaces *> parseExpression ctx
  option ' ' (char ',')
  let key = if key2 /= EmptyExpression then key2 else key1
  return (key, value)

parseObjectExpression :: ProgramContext -> ReadP Expression
parseObjectExpression ctx = do
  char '{' *> skipNewlinesAndSpaces
  pairs <- many (parseObjectExpressionKeyValue ctx)
  skipNewlinesAndSpaces *> char '}'
  return $ ObjectExpression pairs

parseArrayElement :: ProgramContext -> ReadP Expression
parseArrayElement ctx = do
  expression <- parseExpression ctx
  option "" (skipNewlinesAndSpaces *> char ',' *> skipNewlinesAndSpaces)
  return expression

parseArrayExpression :: ProgramContext -> ReadP Expression
parseArrayExpression ctx = do
  char '[' *> skipNewlinesAndSpaces
  elements <- many $ parseArrayElement ctx
  skipNewlinesAndSpaces *> char ']'
  return $ ArrayExpression elements

parseExpression :: ProgramContext -> ReadP Expression
parseExpression ctx = choice [parseArrayExpression ctx, parseObjectExpression ctx, parseUpdateExpression, parseExpressionLiteral, parseFunctionExpression ctx, parseArrowFunctionExpression ctx, parseCallExpression ctx, parseIdentifier, parseBinaryExpression ctx, bracketedExpression ctx]

parseExpressionNoBinary :: ProgramContext -> ReadP Expression
parseExpressionNoBinary ctx = choice [parseArrayExpression ctx, parseObjectExpression ctx, parseUpdateExpression, parseExpressionLiteral, parseFunctionExpression ctx, parseArrowFunctionExpression ctx, parseCallExpression ctx, parseIdentifier, bracketedExpression ctx]

parseExpressionNoMember :: ProgramContext -> ReadP Expression
parseExpressionNoMember ctx = choice [parseArrayExpression ctx, parseObjectExpression ctx, parseUpdateExpression, parseExpressionLiteral, parseFunctionExpression ctx, parseArrowFunctionExpression ctx, parseCallExpression ctx, parseIdentifier, parseBinaryExpression ctx, bracketedExpression ctx]

---------------------------------------------------------------------------------------------

parseExpressionStatement :: ProgramContext -> ReadP Statement
parseExpressionStatement ctx = ExpressionStatement <$> (parseExpression ctx <* (nothingAfter <|> (() <$ closingBracket)))

parseBlockStatement :: ProgramContext -> ReadP Statement
parseBlockStatement ctx = BlockStatement <$> brackets '{' '}' (many $ parseStatement ctx)

parseFunctionDeclaration :: ProgramContext -> ReadP Statement
parseFunctionDeclaration ctx = do
  name <- string "function" *> skipSpaces *> munch1 isAlphaNum
  args <- brackets '(' ')' (sepBy (munch (const True)) (char ','))
  skipSpaces
  body <- parseBlockStatement (setIsFunction ctx True)
  return $ FunctionDeclaration name args body

parseVariableDeclaratorWithValue :: ProgramContext -> ReadP Expression
parseVariableDeclaratorWithValue ctx = do
  skipSpaces
  char '='
  skipSpaces
  value <- parseExpression ctx
  return value

parseVariableDeclarator :: VariableDeclaratorType -> ProgramContext -> ReadP Statement
parseVariableDeclarator declaratorType ctx = do
  name <- parseIdentifier
  value <-
    ( \x -> case x of
        EmptyExpression -> Nothing
        _ -> Just x
      )
      <$> if declaratorType /= Const then option EmptyExpression (parseVariableDeclaratorWithValue ctx) else parseVariableDeclaratorWithValue ctx
  return $ VariableDeclarator declaratorType name value

parseVariableDeclaration :: ProgramContext -> ReadP Statement
parseVariableDeclaration ctx = do
  declaratorType <-
    ( \x -> case x of
        "const" -> Const
        "let" -> Let
        "var" -> Var
      )
      <$> choice
        (map string variableDeclarators)
  skipSpaces
  declarators <- sepBy1 (parseVariableDeclarator declaratorType ctx) (skipSpaces *> char ',' *> skipSpaces)
  skipSpaces
  nothingAfter
  return $ VariableDeclaration declarators

parseForStatement :: ProgramContext -> ReadP Statement
parseForStatement ctx = do
  string "for"
  skipSpaces
  char '('
  skipSpaces
  init <- option Nothing (Just <$> parseExpression ctx)
  skipSpaces
  char ';'
  skipSpaces
  test <- option Nothing (Just <$> parseExpression ctx)
  skipSpaces
  char ';'
  skipSpaces
  update <- option Nothing (Just <$> parseExpression ctx)
  skipSpaces
  char ')'
  skipSpaces
  body <- parseBlockStatement (setIsLoop ctx True)
  return $ ForStatement init test update body

parseIfStatement :: ProgramContext -> ReadP Statement
parseIfStatement ctx = do
  predicate <- string "if" *> skipSpaces *> bracketedExpression ctx
  skipSpaces
  body <- parseBlockableStatement ctx
  alternate <-
    option Nothing $ Just <$> (string "else" *> skipSpaces *> (parseBlockableStatement ctx <|> parseIfStatement ctx))
  return $ IfStatement predicate body alternate

parseContinueStatement = ContinueStatement <$ string "continue"

parseBreakStatement = ContinueStatement <$ string "break"

parseReturnStatement :: ProgramContext -> ReadP Statement
parseReturnStatement ctx = do
  string "return"
  skipSpaces
  argument <- parseExpression ctx
  return $ ReturnStatement argument

parseStatement :: ProgramContext -> ReadP Statement
parseStatement ctx = choice ([parseForStatement ctx, parseExpressionStatement ctx, parseIfStatement ctx, parseFunctionDeclaration ctx, parseVariableDeclaration ctx, parseBlockStatement ctx] ++ (concat $ map snd $ filter (fst) [(isLoop ctx, [parseContinueStatement, parseBreakStatement]), (isFunction ctx, [parseReturnStatement ctx])])) <* skipSpaces <* option ' ' (char ';' <|> char '\n')

parseProgram :: ReadP Program
parseProgram = do
  let initContext = ProgramContext False False False
  statements <- many (parseStatement initContext)
  eof
  return $ Program statements
