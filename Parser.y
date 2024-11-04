import Control.Applicative (Alternative(..), many)
import Control.Monad (MonadPlus(..))


-- Define token data type (based on the lexer output)
data TokenType = VAL | VAR | PRINTLN | READLN | IF | ELSE | WHILE | IDENTIFIER | NUMBER
               | PLUS | MINUS | MULT | DIV | LEQ | EQ | LEFT_PAREN | RIGHT_PAREN | EOF
               deriving (Show, Eq)

data Token = Token { tokenType :: TokenType, tokenValue :: String } deriving (Show)

-- Define AST Nodes
data AST
  = NumberNode Int
  | VariableNode String
  | BinaryOpNode AST TokenType AST
  | PrintNode AST
  | ReadlnNode
  | VarDeclarationNode TokenType String AST
  | AssignmentNode String AST
  | IfNode AST AST (Maybe AST)
  | WhileNode AST AST
  | Block [AST]
  deriving (Show)

-- Define the Parser type and instances
newtype Parser a = Parser { parse :: [Token] -> Maybe (a, [Token]) }

instance Functor Parser where
  fmap f (Parser p) = Parser $ \input -> case p input of
      Nothing -> Nothing
      Just (result, rest) -> Just (f result, rest)

instance Applicative Parser where
  pure x = Parser $ \input -> Just (x, input)
  (Parser pf) <*> (Parser pa) = Parser $ \input -> case pf input of
      Nothing -> Nothing
      Just (f, rest) -> case pa rest of
          Nothing -> Nothing
          Just (a, rest') -> Just (f a, rest')

instance Monad Parser where
  (Parser p) >>= f = Parser $ \input -> case p input of
      Nothing -> Nothing
      Just (result, rest) -> parse (f result) rest

-- Make Parser an instance of Alternative
instance Alternative Parser where
  empty = Parser $ const Nothing
  (Parser p1) <|> (Parser p2) = Parser $ \input -> case p1 input of
      Nothing -> p2 input
      success -> success

-- Make Parser an instance of MonadPlus
instance MonadPlus Parser where
  mzero = empty
  mplus = (<|>)

-- Now <|> and many should work as expected

-- Parser utilities
match :: TokenType -> Parser Token
match expected = Parser $ \tokens -> case tokens of
    (t:ts) | tokenType t == expected -> Just (t, ts)
    _ -> Nothing

consume :: TokenType -> String -> Parser Token
consume expected errorMsg = Parser $ \tokens -> case tokens of
    (t:ts) | tokenType t == expected -> Just (t, ts)
    _ -> Nothing

-- Basic expression parsers
number :: Parser AST
number = do
    tok <- match NUMBER
    return $ NumberNode (read (tokenValue tok))

identifier :: Parser AST
identifier = do
    tok <- match IDENTIFIER
    return $ VariableNode (tokenValue tok)

-- Higher-order parsers
parseExpression :: Parser AST
parseExpression = parseTerm

parseTerm :: Parser AST
parseTerm = do
    left <- parseFactor
    rest <- many parseBinaryOp
    return $ foldl (flip ($)) left rest
  where
    parseBinaryOp = do
        op <- match PLUS <|> match MINUS
        right <- parseFactor
        return $ \left -> BinaryOpNode left (tokenType op) right

parseFactor :: Parser AST
parseFactor = do
    left <- parsePrimary
    rest <- many parseBinaryOp
    return $ foldl (flip ($)) left rest
  where
    parseBinaryOp = do
        op <- match MULT <|> match DIV
        right <- parsePrimary
        return $ \left -> BinaryOpNode left (tokenType op) right

parsePrimary :: Parser AST
parsePrimary = number <|> identifier <|> parseParentheses

parseParentheses :: Parser AST
parseParentheses = do
    _ <- match LEFT_PAREN
    expr <- parseExpression
    _ <- consume RIGHT_PAREN "Expected ')'"
    return expr

-- Command Parsers
parseStatement :: Parser AST
parseStatement = parseVarDeclaration <|> parseAssignment <|> parseIf <|> parseWhile <|> parsePrint <|> parseReadln

parseVarDeclaration :: Parser AST
parseVarDeclaration = do
    varType <- match VAL <|> match VAR
    tok <- consume IDENTIFIER "Expected variable name after 'val' or 'var'"
    _ <- consume LEQ "Expected '=' after variable name"
    value <- parseExpression
    return $ VarDeclarationNode (tokenType varType) (tokenValue tok) value

parseAssignment :: Parser AST
parseAssignment = do
    var <- match IDENTIFIER
    _ <- consume LEQ "Expected '=' in assignment"
    value <- parseExpression
    return $ AssignmentNode (tokenValue var) value

parseIf :: Parser AST
parseIf = do
    _ <- match IF
    _ <- consume LEFT_PAREN "Expected '(' after 'if'"
    condition <- parseExpression
    _ <- consume RIGHT_PAREN "Expected ')' after if condition"
    thenBranch <- parseStatement
    elseBranch <- parseOptionalElse
    return $ IfNode condition thenBranch elseBranch
  where
    parseOptionalElse :: Parser (Maybe AST)
    parseOptionalElse = (match ELSE >> (Just <$> parseStatement)) <|> pure Nothing


parseWhile :: Parser AST
parseWhile = do
    _ <- match WHILE
    _ <- consume LEFT_PAREN "Expected '(' after 'while'"
    condition <- parseExpression
    _ <- consume RIGHT_PAREN "Expected ')' after while condition"
    body <- parseStatement
    return $ WhileNode condition body

parsePrint :: Parser AST
parsePrint = do
    _ <- match PRINTLN
    _ <- consume LEFT_PAREN "Expected '(' after 'println'"
    expression <- parseExpression
    _ <- consume RIGHT_PAREN "Expected ')'"
    return $ PrintNode expression

parseReadln :: Parser AST
parseReadln = do
    _ <- match READLN
    return ReadlnNode

-- Entry point for the parser
parseMain :: Parser AST
parseMain = do
    statements <- many parseStatement
    return $ Block statements

-- Run the parser
runParser :: Parser a -> [Token] -> Maybe a
runParser p input = case parse p input of
    Just (result, []) -> Just result
    _ -> Nothing

