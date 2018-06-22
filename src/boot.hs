module Boot where

import Control.Monad
import Control.Monad.Trans.State

--( Syntax tree )------------------------------------------
type Scope = [(String, Exp)]
data Exp = Text String
  | Number Double
  | Closure [String] [Exp] Exp -- args, binds, body
  | Ref String
  | Apply Exp [Exp]
  deriving Show

--( Parser )-----------------------------------------------

type Parser a = StateT String Maybe a

satisfy :: (Char -> Bool) -> Parser Char
satisfy f = do
  (x:xs) <- get
  guard $ f x
  put xs
  return x

char :: Char -> Parser Char
char c = satisfy (== c)

noneOf :: String -> Parser Char
noneOf xs = satisfy $ \x -> not $ elem x xs

string :: String -> Parser String
string text = check text
  where
    check [] = return text
    check (x:xs) = satisfy (== x) >> check xs

between :: Parser a -> Parser b -> Parser c -> Parser c
between left right center = do
  left
  matched <- center
  right
  return matched

many :: Parser a -> Parser [a]
many p = many_r p []
  where
    many_r :: Parser a -> [a] -> Parser [a]
    many_r f acc = (f >>= \x -> many_r f $ x : acc) `mplus` return acc


--( parse and eval )---------------------------------------
run :: String -> String
run src = case lookup "main" env of
    Just main -> display $ eval env main
    Nothing -> show env
  where
    env = parse src
    display (Text text) = text
    display exp_ = show exp_

parse :: String -> Scope
parse s = [("main", Text s)]

parse_text :: Parser Exp
parse_text = Text <$> between (char '"') (char '"') (many $ noneOf "\"")

eval :: Scope -> Exp -> Exp
eval _ e@(Text _) = e
eval _ e@(Number _) = e
eval _ e@(Closure _ _ _) = e
eval scope (Ref name) = case lookup name scope of
    Just exp_ -> exp_
    Nothing -> error $ "Not found " ++ name ++ " in " ++ show scope
eval scope (Apply exp_ params) = case eval scope exp_ of
    Closure args binds body -> eval ((zip args (binds ++ params)) ++ scope) body
    _ -> error $ "Panic " ++ show exp_ ++ " with " ++ show params

--( main )-------------------------------------------------
main = do
  print $ run "\"hi\""
  print $ run "123"
