module MP4 where

import Data.Char


data State s a = State { run :: s -> Maybe (a, s) }


instance Functor (State s) where
  fmap f st = State $ \s -> case run st s of
                              Nothing -> Nothing
                              Just (x, s') -> Just (f x, s')


instance Applicative (State s) where
  pure x = State $ \s -> Just (x, s)
  stf <*> stx = State $ \s -> case run stf s of
                                Nothing -> Nothing
                                Just (f, s') -> run (f <$> stx) s'


instance Monad (State s) where
  st >>= f = State $ \s -> case run st s of
                             Nothing -> Nothing
                             Just (x, s') -> run (f x) s'


class Applicative f => Alternative f where
  empty :: f a
  (<|>) :: f a -> f a -> f a

  many :: f a -> f [a]
  some :: f a -> f [a]

  many x = some x <|> pure []
  some x = pure (:) <*> x <*> many x


instance Alternative (State s) where
  empty = State $ \_ -> Nothing
  p <|> q = State $ \s -> case run p s of
                            Nothing -> run q s
                            r -> r


type Parser a = State String a


item :: Parser Char
item = State $ \input -> case input of "" -> Nothing
                                       (x:xs) -> Just (x, xs)


sat :: (Char -> Bool) -> Parser Char
sat p = do x <- item
           if p x then return x else State (\_ -> Nothing)


char :: Char -> Parser Char
char c = sat (==c)


string :: String -> Parser String
string "" = return ""
string (x:xs) = do char x
                   string xs
                   return $ x:xs


-- Accepts any kind of parser with a comma after it, see below for the general
-- case.
commaAfter :: Parser a -> Parser a
commaAfter p = do
  x <- p
  symbol ","
  return x


-- A comma-delimited list with at least one entry.  We attempt to match the
-- input with a comma, but ultimately, we only require a single element
-- _without_ a comma after.  All parses are just appended together and
-- returned.
commaDelimited :: Parser a -> Parser [a]
commaDelimited p = do
  xs <- many (commaAfter p)
  x <- p
  return (mappend xs [x])


space :: Parser ()
space = do many $ sat isSpace
           return ()


token :: Parser a -> Parser a
token p = do space
             x <- p
             space
             return x


symbol :: String -> Parser String
symbol s = token (string s)


-- Well a digit is simply one of the characters `0` to `9` - with the `sat`
-- building block we just have to check whether the input is in a sequence, so
-- a string of the characters is the obvious choice here, even if not
-- efficient.
digit :: Parser Char
digit = sat (\c -> elem c "0123456789")


negativeInteger :: Parser String
negativeInteger = do
  char '-'
  x <- integer
  pure ('-' : x)


-- Obviously, once we have a single digit, we just want one or more to form an
-- integer.  N.b. we could also ensure that we don't start with a zero, but
-- that wasn't specified, neither a negative form.
integer :: Parser String
integer = some digit


-- We only accept `int` and `char` as types.  Easy enough, since `symbol`
-- already gives us whitespace-delimited matching, we match either `int` or
-- `char` with the same combinator.
type' :: Parser String
type' = symbol "int" <|> symbol "char"


-- An identifier is a non-empty string, starting with a lowercase character and
-- none or more alphanumeric characters afterwards.  We have to split it
-- because of the lowercase constraint, otherwise we could've used `some` here.
identifier :: Parser String
identifier = do
  first <- sat isLower
  rest <- many $ sat isAlphaNum
  pure (first : rest)


-- Accepts an identifier, which might also have whitespace before or after it,
-- which is usually the case.
identifier' :: Parser String
identifier' = token identifier


-- A parameter is again just a type and an identifier - we return both even if
-- the call site just uses the name later.
parameter :: Parser (String, String)
parameter = do
  type'' <- type'
  name <- identifier'
  return (type'', name)


-- A parameter list can also contain no elements, thus the alternative branch
-- to just return an empty list in case we can't parse it as a comma-delimited
-- list (which we've defined as having at least one entry for convenience
-- elsewhere).
parameterList :: Parser [(String, String)]
parameterList = do
  symbol "("
  parameters <- commaDelimited parameter <|> pure []
  symbol ")"
  return parameters


-- Making things a bit type-safe:  These are just containers for the three
-- things a function body can contain.  Add a `deriving Show` to print them
-- too.
data BodyStatement = LocalVariable String [String] | Assignment String String | Return String


-- Selector to extract the return value from any kind of body statement, c.f.
-- the call site for how this makes sense.
returnValue' :: BodyStatement -> Maybe String
returnValue' (Return x) = Just x
returnValue' _ = Nothing


-- A local variable has a type and can, interestingly, have one or more names
-- declared.  We return everything, even if the type ends up not being used.
localVariable :: Parser BodyStatement
localVariable = do
  type'' <- type'
  names <- commaDelimited identifier
  return $ LocalVariable type'' names


-- Easy again, either we have an identifier, or an integer.
varOrValue :: Parser String
varOrValue = identifier' <|> integer <|> negativeInteger


-- An assignment is just identifier, a symbol and a value, best via the monad
-- interface.
assignment :: Parser BodyStatement
assignment = do
  name <- identifier'
  symbol "="
  value <- varOrValue
  return (Assignment name value)


-- Statements _always_ have a semicolon, making this very easy to parse.
statement :: Parser a -> Parser a
statement p = do x <- p
                 symbol ";"
                 return x


-- Return statements just use a symbol and a value.
returnStatement :: Parser BodyStatement
returnStatement = do
  symbol "return"
  x <- varOrValue
  return (Return x)


-- A body statement can thus be one of the above, all via alternatives.
bodyStatement :: Parser BodyStatement
bodyStatement = localVariable <|> assignment <|> returnStatement


-- Selector to extract variable names - we end up concatenating them at the
-- call site, since we just want all the variable names in the ene.
variables :: BodyStatement -> [String]
variables (LocalVariable _ names) = names
variables (Assignment _ _) = []
variables (Return _) = []


-- This is a bit tricky:  We get a list of body statements, which might or
-- might not contain a return statement.  If we have one (that is,
-- `returnValue'` didn't return `Nothing), we simply take the first, otherwise
-- just an empty string is returned.
maybeReturnValue :: [BodyStatement] -> String
maybeReturnValue xs =
  case filter (/= Nothing) (map returnValue' xs) of
    (Just x) : _ -> x
    _ -> ""


-- We only accept local variables before assignments and assignments before
-- returns.  Keeping in mind that we also need to consider multiple body
-- statements of the same type next to each other, and only local variables and
-- returns without any assignments.
validStatements :: BodyStatement -> BodyStatement -> Bool
validStatements (LocalVariable _ _) (LocalVariable _ _) = True
validStatements (Assignment _ _) (Assignment _ _) = True
validStatements (LocalVariable _ _) (Assignment _ _) = True
validStatements (LocalVariable _ _) (Return _) = True
validStatements (Assignment _ _) (Return _) = True
validStatements _ _ = False


-- For multiple statements they all have to satisfy the pair-wise constraint,
-- so zipping is easiest here.
validStatementOrder :: [BodyStatement] -> Bool
validStatementOrder xs = all (uncurry validStatements) (zip xs (tail xs))


-- Special handling here:  We only accept if the order of statements is valid.
bodyStatements :: Parser [BodyStatement]
bodyStatements = do
  xs <- many (statement bodyStatement)
  if validStatementOrder xs then pure xs else empty
-- Without do-notation this is the same as this:
-- bodyStatements = (many (statement bodyStatement)) >>= (\xs -> if validStatementOrder xs then pure xs else empty)


-- A body block is delimited by curly braces and can contain zero or more
-- statements.  As said we concatenate all variables and extract the return
-- value.
bodyBlock :: Parser ([String], String)
bodyBlock = do
  symbol "{"
  statements <- bodyStatements
  symbol "}"
  return (concat $ map variables statements, maybeReturnValue statements)


{-
  Parses a limited C function in order to obtain:

  1. The name of the function
  2. A list of the names of the parameters of the function
  3. A list of the names of the local variables declared within the function
  4. The variable name or integer value returned by the function (as a string),
     or the empty string if there is no return statement.

  See the writeup for examples.
-}
funcDef :: Parser (String,[String],[String],String)
funcDef = do
  -- firstly we accept the type, but since we don't use it, its result is
  -- discarded
  type'
  -- the function name is simply an identifier
  functionName <- identifier'
  -- while parameters are in a parameter list, delimited by parenthesis
  parameters <- parameterList
  -- the body is its own complex object
  (variables, returnValue) <- bodyBlock
  -- finally we extract the variable names from the parameters; for the body we
  -- already get a parsed results back though
  return (functionName, (map (\(_, x) -> x) parameters), variables, returnValue)
