module Boot where

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
data Source = Source {
  source :: String,
  position :: Int
} deriving (Show)

type Parser a = StateT Source Maybe a

satisfy :: (Char -> Bool) -> Parser Char
satisfy f = StateT check
  where
    check s = if (length src) < pos && f char
        then Just $ (char, succ)
        else Nothing
      where
        src = source s
        pos = position s
        char = src !! pos
        succ = Source src (1 + pos)

char :: Char -> Parser Char
char c = satisfy (== c)

noneOf :: String -> Parser Char
noneOf xs = satisfy $ \x -> not $ elem x xs

string :: String -> Parser String
string text = check text
  where
    check [] = return text
    check (x:xs) = satisfy (== x) >> check xs

--many f = many_r []
--  where
--    many_r acc = (satisfy f >>= \c -> many_r $ c : acc) <|> return $ reverse acc

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
