module AST where

type Env = [(String, AST)]

data AST =
-- value
    Void
  | Bool Bool
  | Int Int
  | String String
-- container
  | List [AST]
-- expression
  | Ref String
  | Op2 String AST AST
  | Apply AST [AST]
  | Method AST String [AST]
  | Match [(Pattern, AST)]
-- define
  | Struct Env
  | Enum String AST
  | Func [String] AST
-- flow
  | Steps [AST]
  | Throw String
  | Assign String AST
  | Return AST
  deriving (Show, Eq)

data Pattern = EnumPattern String deriving (Show, Eq)

data Source = Source { source :: String, indent :: Int } deriving (Show)

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
