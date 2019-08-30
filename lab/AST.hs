module AST where

type Env = [(String, AST)]

data AST =
-- simple value
    Void
  | Bool Bool
  | Int Int
  | Float Double
  | String String
  | Func Env AST
  | List [AST]
  | Class Env
-- expression
  | Block [AST]
  | Assign String AST
  | Call String [AST] -- ref, exp, assign and update
  | Match [([AST], AST)]
  deriving (Show, Eq)

data Source = Source { original :: String, source :: String, indentation :: Int, line :: Int, column :: Int } deriving (Show)

data Parser a = Parser { runParser :: Source -> Maybe (a, Source) }

instance Functor Parser where
  fmap f p = Parser $ \s -> fmap (\(a, ss) -> (f a, ss)) (runParser p s)

instance Applicative Parser where
  pure v = Parser $ \s -> Just (v, s)
  l <*> r = Parser $ \s -> case (runParser l s, runParser r s) of
    (Just (f, _), Just (a, _)) -> Just (f a, s)
    _ -> Nothing

instance Monad Parser where
  return = pure
  l >>= f = Parser $ \s -> case runParser l s of
    Just (a, ss) -> runParser (f a) ss
    _ -> Nothing
