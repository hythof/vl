module Origin where

import           Debug.Trace               (trace)

--( Structure )------------------------------------------------------
data AST =
-- value
    Char Char
  | String String
  | Int Int
  | Real Double
  | Bool Bool
-- container
  | List [AST]
-- expression
  | Func [String] AST
  | Ref String
  | Op2 String AST AST
  | Apply AST [AST]
  | Stmt [Line]
-- enum
  | Enum String AST
-- state
  | Throw [AST]
  | State [(String, AST)]
-- void
  | Void
-- runtime only
  | Closure [(String, AST)] AST
  deriving (Show, Eq, Ord)

data Line =
    Call AST
  | Assign String AST
  deriving (Show, Eq, Ord)

type Env = [(String, AST)]

--( Parser )---------------------------------------------------------
data Parser a = Parser { runParser :: String -> Maybe (a, String) }

instance Functor Parser where
    fmap f p = Parser $ \s ->
                fmap (\(s, v) -> (f s, v)) $ runParser p s

instance Applicative Parser where
    pure v = Parser $ \s -> Just (v, s)
    --l <*> r = Parser $ \s -> case runParser r s of
    --    Just (v, s') -> fmap (\(f, s'') -> (f v, s'')) $ runParser l s
    --    Nothing -> Nothing
    l <*> r = Parser $ \s -> do
      (v, s') <- runParser r s
      fmap (\(f, s'') -> (f v, s'')) $ runParser l s

instance Monad Parser where
    return = pure
    --m >>= f = Parser $ \s -> case runParser m s of
    --    Just (v, s') -> runParser (f v) s'
    --    Nothing -> Nothing
    m >>= f = Parser $ \s -> do
      (v, s') <- runParser m s
      runParser (f v) s'
    fail _ = Parser $ \_ -> Nothing

l <|> r = Parser $ \s -> case runParser l s of
  Just x -> return x
  Nothing -> runParser r s

guard False = Nothing
guard True = Just 0

satisfy f = Parser $ \s -> do
  guard $ (length s) > 0
  let c = s !! 0
  guard $ f c
  Just (c, tail s)

lexeme f = do
  many $ ignore " \t\r\n"
  satisfy f

ignore [] = Parser $ \s -> Nothing
ignore (x:xs) = satisfy (== x) <|> ignore xs

oneOf [] = Parser $ \_ -> Nothing
oneOf (x:xs) = lexeme (== x) <|> oneOf xs

many1 f = do
  x <- f
  xs <- many f
  return $ x : xs

many f = many_r f []
many_r f acc = (many_acc f acc) <|> (return $ reverse acc)
many_acc f acc = do
  x <- f
  many_r f (x : acc)

parse :: String -> AST
parse input = case runParser parse_root input of
  Just (ast, "") -> ast
  Just (_, left) -> String $ "left: " ++ left
  Nothing -> String $ "failed: " ++ input

read_op = many1 $ oneOf "+-*/"

parse_root = do
  parse_op2

parse_op2 = do
  l <- parse_int
  o <- read_op <|> return ""
  case o of
    "" -> return l
    _ -> do
      r <- parse_op2
      return $ Op2 o l r

parse_int = do
  s <- many1 (oneOf "0123456789")
  return $ Int (read s :: Int)

--( Evaluator )-------------------------------------------------------

eval x@(Int _) = x
eval (Op2 op left right) = f el er
 where
  f (Int l) (Int r) = Int $ fi op l r
  fi "+" = (+)
  fi "-" = (-)
  el = eval left
  er = eval right

--( Main )------------------------------------------------------------
main = do
  let src = "1 + 2 + 3"
  let ast = parse src
  let ret = eval ast
  line "src: " src
  line "ast: " $ show ast
  line "ret: " $ show ret
 where
  line title body = do
    putStr title
    putStrLn body

debug x = trace (show x) (return 0)
