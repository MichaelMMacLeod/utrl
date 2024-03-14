{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}

module Main (main) where

import Ast0
import Control.Applicative (liftA, liftA2)
import Data.Char (isSpace)
import Data.Functor.Foldable (Base, Corecursive, ListF (Cons, Nil), Recursive, ana, cata, embed, project)
import Data.HashMap.Strict qualified as H
import Data.Kind (Type)
import Data.List (intercalate, partition)

-- (fn xs (flatten (list (list xs ..) ..)) -> (list xs .. ..))
main :: IO ()
main =
  print
    . displayC1
    . compileC0toC1
    . compile1toC0 (H.fromList [("xs", [ZeroPlusC0 1, BetweenC0 1 0, BetweenC0 1 0])])
    . compile0to1
    $ read0 "(list xs .. ..)"

-- -- (fn xs (flatten ((xs ..) ..)) -> (xs .. ..))
-- main :: IO ()
-- main =
--   print
--     . displayC1
--     . compileC0toC1
--     . compile1toC0 (H.fromList [("xs", [BetweenC0 0 0, BetweenC0 0 0])])
--     . compile0to1
--     $ read0 "(xs .. ..)"

-- main = print . display1 $ compile0to1 (Compound0 [Symbol0 "a", Symbol0 "x", Symbol0 "..", Symbol0 "..", Symbol0 "q"])

-- type family Base t :: Type -> Type

-- class (Functor (Base t)) => Recursive t where
--   project :: t -> Base t t

-- class (Functor (Base t)) => Corecursive t where
--   embed :: Base t t -> t

-- data ListF a r = Nil | Cons a r

-- instance Functor (ListF a) where
--   fmap _ Nil = Nil
--   fmap f (Cons a r) = Cons a (f r)

-- type instance Base [a] = ListF a

-- instance Recursive [a] where
--   project [] = Nil
--   project (x : xs) = Cons x xs

-- instance Corecursive [a] where
--   embed Nil = []
--   embed (Cons x xs) = x : xs

-- cata :: (Recursive t) => (Base t a -> a) -> t -> a
-- cata f = f . fmap (cata f) . project

-- ana :: (Corecursive t) => (a -> Base t a) -> a -> t
-- ana f = embed . fmap (ana f) . f

-- hylo :: (Functor f) => (f b -> b) -> (a -> f a) -> a -> b
-- hylo f g = h where h = f . fmap h . g

data Ast1
  = Symbol1 String
  | Compound1 [Ast1]
  | Ellipses1 Ast1
  deriving (Show)

data Ast1F r
  = Symbol1F String
  | Compound1F [r]
  | Ellipses1F r
  deriving (Show, Functor)

type instance Base Ast1 = Ast1F

instance Recursive Ast1 where
  project (Symbol1 s) = Symbol1F s
  project (Compound1 xs) = Compound1F xs
  project (Ellipses1 x) = Ellipses1F x

instance Corecursive Ast1 where
  embed (Symbol1F s) = Symbol1 s
  embed (Compound1F xs) = Compound1 xs
  embed (Ellipses1F x) = Ellipses1 x

data IndexElementC0
  = ZeroPlusC0 Integer
  | LenMinusC0 Integer
  | BetweenC0 {zeroPlusC0 :: Integer, lenMinusC0 :: Integer}
  deriving (Eq)

type IndexC0 = [IndexElementC0]

c1Tail :: IndexC0 -> IndexC1
c1Tail = reverse . go . reverse
  where
    go :: IndexC0 -> IndexC1
    go ((ZeroPlusC0 i) : xs) = ZeroPlusC1 i : go xs
    go ((LenMinusC0 i) : xs) = LenMinusC1 i : go xs
    go _ = []

c0Head :: IndexC0 -> IndexC0
c0Head = reverse . go . reverse
  where
    go :: IndexC0 -> IndexC0
    go all@(BetweenC0 zeroPlusC0 lenMinusC0 : xs) = all
    go (x : xs) = go xs
    go [] = []

cutC0 :: IndexC0 -> (IndexC0, IndexC1)
cutC0 c0 = (c0Head c0, c1Tail c0)

cutC0Between :: IndexC0 -> (IndexC0, Maybe (Integer, Integer))
cutC0Between = go . reverse
  where
    go (BetweenC0 zp lm : others) = (reverse others, Just (zp, lm))
    go others = (others, Nothing)

displayIndexElementC0 :: IndexElementC0 -> String
displayIndexElementC0 (ZeroPlusC0 i) = show i
displayIndexElementC0 (LenMinusC0 i) = "(len-" ++ show i ++ ")"
displayIndexElementC0 (BetweenC0 zeroPlusC0 lenMinusC0) = show zeroPlusC0 ++ ".." ++ show lenMinusC0

displayIndexC0 :: IndexC0 -> String
displayIndexC0 index = "[" ++ intercalate "," (map displayIndexElementC0 index) ++ "]"

displayIndexElementC1 :: IndexElementC1 -> String
displayIndexElementC1 (ZeroPlusC1 i) = show i
displayIndexElementC1 (LenMinusC1 i) = "(len-" ++ show i ++ ")"

displayIndexC1 :: IndexC1 -> String
displayIndexC1 index = "[" ++ intercalate "," (map displayIndexElementC1 index) ++ "]"

data AstC0
  = SymbolC0 String
  | CompoundC0 [AstC0]
  | VariableC0 IndexC0
  | EllipsesC0 AstC0

data AstC0F r
  = SymbolC0F String
  | CompoundC0F [r]
  | VariableC0F IndexC0
  | EllipsesC0F r
  deriving (Functor)

type instance Base AstC0 = AstC0F

instance Recursive AstC0 where
  project (SymbolC0 s) = SymbolC0F s
  project (CompoundC0 xs) = CompoundC0F xs
  project (VariableC0 i) = VariableC0F i
  project (EllipsesC0 x) = EllipsesC0F x

instance Corecursive AstC0 where
  embed (SymbolC0F s) = SymbolC0 s
  embed (CompoundC0F xs) = CompoundC0 xs
  embed (VariableC0F i) = VariableC0 i
  embed (EllipsesC0F x) = EllipsesC0 x

data IndexElementC1
  = ZeroPlusC1 Integer
  | LenMinusC1 Integer

type IndexC1 = [IndexElementC1]

data AstC1
  = SymbolC1 String
  | CompoundC1 [AstC1]
  | CopyC1 IndexC1
  | LoopC1 {indexC1 :: IndexC1, startC1 :: Integer, endC1 :: Integer, bodyC1 :: AstC1}

data AstC1F r
  = SymbolC1F String
  | CompoundC1F [r]
  | CopyC1F IndexC1
  | LoopC1F {indexC1F :: IndexC1, startC1F :: Integer, endC1F :: Integer, bodyC1F :: r}
  deriving (Functor)

type instance Base AstC1 = AstC1F

instance Recursive AstC1 where
  project (SymbolC1 s) = SymbolC1F s
  project (CompoundC1 xs) = CompoundC1F xs
  project (CopyC1 i) = CopyC1F i
  project (LoopC1 i s e b) = LoopC1F i s e b

instance Corecursive AstC1 where
  embed (SymbolC1F s) = SymbolC1 s
  embed (CompoundC1F xs) = CompoundC1 xs
  embed (CopyC1F i) = CopyC1 i
  embed (LoopC1F i s e b) = LoopC1 i s e b

data Token = TLeft | TRight | TSymbol String deriving (Show)

lex0 :: String -> [Token]
lex0 = ana $ \case
  [] -> Nil
  xs -> let (token, rest) = parseToken xs in Cons token rest
    where
      parseToken :: String -> (Token, String)
      parseToken (' ' : xs) = parseToken xs
      parseToken ('(' : xs) = (TLeft, xs)
      parseToken (')' : xs) = (TRight, xs)
      parseToken xs = (TSymbol $ takeWhile isSymbolChar xs, dropWhile isSymbolChar xs)

      isSymbolChar :: Char -> Bool
      isSymbolChar =
        notF isSpace
          <&&> notF (== '(')
          <&&> notF (== ')')
        where
          notF = fmap not
          (<&&>) = liftA2 (&&)

parse0 :: [Token] -> Ast0
parse0 xs = go xs []
  where
    go :: [Token] -> [[Ast0]] -> Ast0
    go (TLeft : xs) acc = go xs ([] : acc)
    go (TRight : xs) (a1 : a2 : acc) = let c = Compound0 (reverse a1) in go xs ((c : a2) : acc)
    go (TRight : xs) [a1] = Compound0 (reverse a1)
    go (TRight : xs) [] = error "Expected '('"
    go (TSymbol s : xs) (a : acc) = go xs ((Symbol0 s : a) : acc)
    go (TSymbol s : xs) [] = Symbol0 s

read0 :: String -> Ast0
read0 = parse0 . lex0

display0 :: Ast0 -> String
display0 = cata $ \case
  Symbol0F s -> s
  Compound0F xs -> "(" ++ unwords xs ++ ")"

display1 :: Ast1 -> String
display1 = cata $ \case
  Symbol1F s -> s
  Compound1F xs -> "(" ++ unwords xs ++ ")"
  Ellipses1F x -> x ++ " .."

displayC0 :: AstC0 -> String
displayC0 = cata $ \case
  SymbolC0F s -> s
  VariableC0F i -> displayIndexC0 i
  CompoundC0F xs -> "(" ++ unwords xs ++ ")"
  EllipsesC0F x -> x ++ " .."

displayC1 :: AstC1 -> String
displayC1 = cata $ \case
  SymbolC1F s -> s
  CompoundC1F xs -> "(" ++ unwords xs ++ ")"
  CopyC1F i -> "{copy " ++ displayIndexC1 i ++ "}"
  LoopC1F index start end body ->
    "{loop "
      ++ show start
      ++ ".."
      ++ show end
      ++ " @ "
      ++ displayIndexC1 index
      ++ " body = "
      ++ body
      ++ "}"

compile0to1 :: Ast0 -> Ast1
compile0to1 = cata $ \case
  Symbol0F s -> Symbol1 s
  Compound0F xs -> Compound1 $ go xs
    where
      go :: [Ast1] -> [Ast1]
      go (x : Symbol1 ".." : xs) = go $ Ellipses1 x : xs
      go (x : xs) = x : go xs
      go [] = []

type Variables = H.HashMap String IndexC0

compile1toC0 :: Variables -> Ast1 -> AstC0
compile1toC0 vars = cata $ \case
  Symbol1F s -> case H.lookup s vars of
    Nothing -> SymbolC0 s
    Just index -> VariableC0 index
  Compound1F xs -> CompoundC0 xs
  Ellipses1F x -> EllipsesC0 x

compileC0toC1 :: AstC0 -> AstC1
compileC0toC1 = verify . cata go
  where
    verify :: (AstC1, IndexC0) -> AstC1
    verify (ast, []) = ast
    verify _ = error "Needs more '..'"

    go :: Base AstC0 (AstC1, IndexC0) -> (AstC1, IndexC0)
    go (SymbolC0F s) = (SymbolC1 s, [])
    go (CompoundC0F xs) =
      let indexesAllEqual = allEqual $ map snd xs
          allEqual :: [IndexC0] -> Bool
          allEqual [] = True
          allEqual (x : xs) = all (== x) xs
          sharedIndex :: [(AstC1, IndexC0)] -> IndexC0
          sharedIndex ((_, i) : _) = i
          sharedIndex _ = []
       in if indexesAllEqual
            then (CompoundC1 $ map fst xs, sharedIndex xs)
            else error "Variables not matched under same '..' used under same '..'"
    go (VariableC0F i) =
      let (fst, snd) = cutC0 i
       in (CopyC1 snd, fst)
    go (EllipsesC0F (astC1, indexC0)) = case cutC0Between indexC0 of
      (indexC0', Just (zeroPlus, lenMinus)) ->
        let (fstC0, sndC1) = cutC0 indexC0'
            loopC1 = LoopC1 {indexC1 = sndC1, startC1 = zeroPlus, endC1 = lenMinus, bodyC1 = astC1}
         in (loopC1, fstC0)
      (_, Nothing) -> error "Too many '..'"

data ConstantExpr
  = VarCE Integer
  | ConstantCE Integer

data BinOp = AddOp | SubOp

data Expr
  = ConstantExprE ConstantExpr
  | -- Computes lhs_varE <opE> rhsE, storing the result in lhs_varE.
    BinOpExpr {opE :: BinOp, lhs_varE :: Integer, rhsE :: ConstantExpr}
  | -- Computes the length of the compound term in the input pointed at
    -- by the index stack, storing the result in 'length_varE'.
    LengthOp {length_varE :: Integer}

data Stmt
  = -- Evaluates rhsS and assigns it to lhs_varS
    AssignS {lhs_varS :: Integer, rhsS :: Expr}
  | -- Pushes a string to the top of the data stack
    PushSymbolS String
  | -- Pushes a constant to the top of the index stack
    PushIndexS ConstantExpr
  | -- Removes the first 'count' indices off the index stack.
    PopIndexS {count :: Integer}
  | -- Coppies the portion of the input pointed to by the index stack,
    -- pusing the copied value onto the top of the data stack.
    Copy
  | -- Pops 'length' number of elements off the top of the data stack,
    -- builds a compound term containing them, and pushes the new compound
    -- term back on top of the data stack.
    Build {lengthS :: Expr}
  | -- Jumps to instruction #labelS when 'when_var' is less than 'le_var',
    -- otherwise control continues to the next statement.
    ConditionalJump {labelS :: Integer, when_var :: Integer, le_var :: Integer}
  | -- Jumps always to a specific instruction.
    UnconditionalJump Integer

-- length :: [a] -> Integer
-- length = cata $ \case
--   Nil -> 0
--   Cons _ acc -> 1 + acc

-- filter :: (a -> Bool) -> [a] -> [a]
-- filter p = cata $ \case
--   Nil -> []
--   Cons x acc -> if p x then x : acc else acc

-- zip :: ([a], [b]) -> [(a, b)]
-- zip = ana $ \case
--   (a : as, b : bs) -> Cons (a, b) (as, bs)
--   _ -> Nil

-- zip' :: ([a], [b]) -> [(a, b)]
-- zip' (a : as, b : bs) = (a, b) : zip' (as, bs)
-- zip' _ = []

-- data Tree a = Leaf a | Branch [Tree a] deriving (Show, Functor)

-- data TreeF a r = LeafF a | BranchF [r] deriving (Show, Functor)

-- type instance Base (Tree a) = (TreeF a)

-- instance Recursive (Tree a) where
--   project (Leaf x) = LeafF x
--   project (Branch xs) = BranchF xs

-- instance Corecursive (Tree a) where
--   embed (LeafF a) = Leaf a
--   embed (BranchF xs) = Branch xs

-- countNodes :: Tree a -> Integer
-- countNodes = cata $ \case
--   LeafF _ -> 1
--   BranchF xs -> 1 + sum xs

-- go = ana $ \case
--   [] -> Nil
--   (x : Symbol1 ".." : xs) -> Cons (Ellipses1 x) xs
--   (x : xs) -> Cons x xs
-- go = cata $ \case
--   Nil -> []
--   Cons x (Symbol1 ".." : xs) -> Ellipses1 x : xs
--   Cons x xs -> x : xs

-- -- print . map fromList . fromList . tails . toList . take 8 . fromList . mapByCata factorialByPara $ Main.iterate (+ 1) 0

-- -- print . take 5 . toList . mapByAna (*2) $ Main.iterate (* 2) 1

-- -- print $ Main.zip' (Cons 1 (Cons 2 (Cons 3 Nil)), Cons "a" (Cons "b" (Cons "c" Nil)))

-- data List a = Nil | Cons a (List a)
--   deriving (Show)

-- -- toList :: List a -> [a]
-- -- toList Nil = []
-- -- toList (Cons a as) = a : toList as

-- toList :: [a] -> List a
-- toList = listAna g
--   where
--     g [] = Nothing
--     g (a : as) = Just (a, as)

-- fromList :: List a -> [a]
-- fromList = listCata [] g
--   where
--     g (a, b) = a : b

-- listCata :: b -> ((a, b) -> b) -> List a -> b
-- listCata b _ Nil = b
-- listCata b f (Cons a as) = f (a, listCata b f as)

-- listLength :: List a -> Integer
-- listLength = listCata 0 (\(_, n) -> 1 + n)

-- listFilter :: (a -> Bool) -> List a -> List a
-- listFilter p = listCata Nil (\(a, b) -> if p a then Cons a b else b)

-- listAna :: (b -> Maybe (a, b)) -> b -> List a
-- listAna g b =
--   case g b of
--     Nothing -> Nil
--     Just (a, b') -> Cons a (listAna g b')

-- listAna' :: (b -> Bool) -> (b -> (a, b)) -> b -> List a
-- listAna' p g b =
--   if p b
--     then Nil
--     else
--       let (a, b') = g b
--        in Cons a (listAna' p g b')

-- zip :: (List a, List b) -> List (a, b)
-- zip = listAna g
--   where
--     g :: (List a, List b) -> Maybe ((a, b), (List a, List b))
--     g (Cons a as, Cons b bs) = Just ((a, b), (as, bs))
--     g _ = Nothing

-- zip' :: (List a, List b) -> List (a, b)
-- zip' = listAna' p g
--   where
--     p :: (List a, List b) -> Bool
--     p (Nil, _) = True
--     p (_, Nil) = True
--     p _ = False
--     g :: (List a, List b) -> ((a, b), (List a, List b))
--     g (Cons a as, Cons b bs) = ((a, b), (as, bs))

-- iterate :: (a -> a) -> a -> List a
-- iterate f = listAna g
--   where
--     g a = Just (a, f a)

-- iterate' :: (a -> a) -> a -> List a
-- iterate' f = listAna' (const False) g
--   where
--     g a = (a, f a)

-- mapByCata :: (a -> b) -> List a -> List b
-- mapByCata f = listCata Nil g
--   where
--     g (a, as) = Cons (f a) as

-- mapByAna :: (a -> b) -> List a -> List b
-- mapByAna f = listAna g
--   where
--     g (Cons a as) = Just (f a, as)
--     g _ = Nothing

-- listHylo :: c -> ((b, c) -> c) -> (a -> Maybe (b, a)) -> a -> c
-- listHylo c oplus g a =
--   case g a of
--     Nothing -> c
--     Just (b, a') -> oplus (b, listHylo c oplus g a')

-- listHylo' :: c -> ((b, c) -> c) -> (a -> (b, a)) -> (a -> Bool) -> a -> c
-- listHylo' c oplus g p a =
--   if p a
--     then c
--     else oplus (b, listHylo' c oplus g p a')
--   where
--     (b, a') = g a

-- listHylo'' :: c -> ((b, c) -> c) -> (a -> Maybe (b, a)) -> a -> c
-- listHylo'' c oplus g = listCata c oplus . listAna g

-- factorial :: Integer -> Integer
-- factorial = listHylo c oplus g
--   where
--     g 0 = Nothing
--     g n = Just (n, n - 1)
--     c = 1
--     oplus (n, m) = n * m

-- factorial'' :: Integer -> Integer
-- factorial'' = listCata c oplus . listAna g
--   where
--     g 0 = Nothing
--     g n = Just (n, n - 1)
--     c = 1
--     oplus (n, m) = n * m

-- factorial' :: Integer -> Integer
-- factorial' = listHylo' c oplus g p
--   where
--     c = 1
--     oplus (n, m) = n * m
--     g n = (n, n - 1)
--     p n = n == 0

-- data Nat = Zero | Succ Nat

-- intoNat :: (Eq t, Num t) => t -> Nat
-- intoNat 0 = Zero
-- intoNat n = Succ (intoNat (n - 1))

-- fromNat :: (Num a) => Nat -> a
-- fromNat Zero = 0
-- fromNat (Succ n) = 1 + fromNat n

-- natPara :: b -> ((Nat, b) -> b) -> Nat -> b
-- natPara b oplus Zero = b
-- natPara b oplus (Succ n) = oplus (n, natPara b oplus n)

-- listPara :: b -> ((a, List a, b) -> b) -> List a -> b
-- listPara b oplus Nil = b
-- listPara b oplus (Cons a as) = oplus (a, as, listPara b oplus as)

-- factorialByPara :: Integer -> Integer
-- factorialByPara = fromNat . natPara (Succ Zero) g . intoNat
--   where
--     g (n, m) = intoNat $ fromNat (Succ n) * fromNat m

-- tails :: List a -> List (List a)
-- tails = listPara (Cons Nil Nil) g
--   where
--     g (a, as, tls) = Cons (Cons a as) tls

-- type Algebra f a = f a -> a

-- newtype Mu f = In {out :: f (Mu f)}

-- cata :: (Functor f) => Algebra f a -> Mu f -> a
-- cata f = f . fmap (cata f) . out

-- data List2F a r = Nil2 | Cons2 a r

-- instance Functor (List2F a) where
--   fmap _ Nil2 = Nil2
--   fmap f (Cons2 a r) = Cons2 a (f r)

-- type List2 a = Mu (List2F a)

-- length2 :: List2 a -> Integer
-- length2 = cata $ \case
--   Nil2 -> 0
--   Cons2 _ acc -> 1 + acc

-- sum2 :: (Num a) => List2 a -> a
-- sum2 = cata $ \case
--   Nil2 -> 0
--   Cons2 n acc -> n + acc

-- toList2 :: [a] -> List2 a
-- toList2 [] = In Nil2
-- toList2 (a : as) = In (Cons2 a (toList2 as))

-- fromList2 :: List2 a -> [a]
-- fromList2 = cata $ \case
--   Nil2 -> []
--   Cons2 n acc -> n : acc

-- data Ast0F r = Symbol0 String | Compound0 [r] deriving (Functor, Show)

-- type Ast0 = Mu Ast0F

-- data Ast1F r = Symbol1 String | Compound1 [r] | Ellipses1 r deriving (Functor, Show)

-- type Ast1 = Mu Ast1F

-- ast0to1 :: Ast0 -> Ast1
-- ast0to1 = cata $ \case
--   Symbol0 s -> In $ Symbol1 s
--   Compound0 lst -> In . Compound1 $ go lst
--     where
--       go :: [Ast1] -> [Ast1]
--       go (x : (In (Symbol1 "..")) : xs) = go (In (Ellipses1 x) : xs)
--       go (x : xs) = x : go xs
--       go _ = []

-- x000 = ast0to1 $ In (Compound0 [In $ Symbol0 "A", In $ Symbol0 "..", In $ Symbol0 ".."])

-- instance Recursive [a] where

-- class Bifunctor f where
--   bimap :: (a -> a') -> (b -> b') -> f a b -> f a' b'

-- newtype Fix f = In {out :: f (Fix f)}

-- data ListF a b = NilF | ConsF a b

-- instance Functor (ListF a) where
--   fmap _ NilF = NilF
--   fmap f (ConsF a b) = ConsF a (f b)

-- type ListF' a = Fix (ListF a)

-- (|>) :: (a -> b) -> (b -> c) -> a -> c
-- (|>) f g x = g (f x)

-- (<|) :: (b -> c) -> (a -> b) -> a -> c
-- (<|) f g x = f (g x)

-- -- cata :: (Functor f) => (Fix f -> b) -> Fix f -> b
-- -- cata f =

-- -- cata :: (ListF' Integer -> Integer) -> ListF' Integer -> Integer
-- -- cata = undefined

-- cata :: Functor f => (f a -> a) -> Fix f -> a
-- cata f = f . fmap (cata f) . out

-- lengthByCata :: ListF' Integer -> Integer
-- lengthByCata = cata $ \case
--   NilF -> 0
--   (ConsF _ acc) -> acc + 1

-- listHylo :: (b -> Maybe (a, b)) -> b -> ((b, c) -> c) -> c
-- listHylo g b f =

-- oplus :: (p, Integer) -> Integer
-- oplus (_, n) = 1 + n

-- b :: Integer
-- b = 0

-- f :: Integer -> Integer
-- f n = n * 2

-- c :: Integer
-- c = f b

-- cross :: (p, Integer) -> Integer
-- cross (_, n) = 2 + n

-- e1 :: List a -> Integer
-- e1 = f . listCata b oplus

-- e2 :: List a -> Integer
-- e2 = listCata c cross

-- f (oplus a as) = cross a (f as)
-- 2 * (1 + as) = (cross a (as * 2))
-- 2 + 2 * as = cross a (2 * as)

-- cross a f_as

-- fusion (b : B) (c : C)
-- Let B, C be types
--     (b : B), (c : C
--
-- f : B -> C
--
-- f . (listCata b g) = listCata c h
--

-- data Ast0
--   = Symbol0 String
--   | Compound0 [Ast0]
--   deriving (Show, Eq)

-- data Ast0F a
--   = Symbol0F String
--   | Compound0F [a]
--   deriving (Show, Eq, Functor)

-- data Term f = In { out :: f (Term f) }

-- instance Functor (ListF t) where
--   fmap :: (a -> b) -> ListF t a -> ListF t b
--   fmap _ NilF = NilF
--   fmap f (ConsF x a) = ConsF x (f a)

-- instance Functor (ListF a b) where
--   fmap :: (a -> b) -> ListF a a2 -> ListF a1 b
--   fmap f NilF = NilF
--   fmap f (ConsF a b) = ConsF (f a) (fmap f b)

-- data Term f = In {out :: f (Term f)}

-- l1 :: Term (ListF Int)
-- l1 = In (ConsF 1 (In (ConsF 2 (In NilF))))

-- huh :: Term Maybe
-- huh = In {out = Nothing}

-- -- huh2 :: _1
-- -- huh2 =

-- bottomUp :: (Functor a) => (Term a -> Term a) -> Term a -> Term a
-- bottomUp f = out |> fmap (bottomUp f) |> In |> f

-- where x0 :: Term a
--       x0 = f x1
--       x1 :: Term a
--       x1 = In x2
--       x2 :: a (Term a)
--       x2 = fmap (bottomUp f) x3
--       x3 :: f (Term f)
--       x3 = out input
-- out >>> fmap (bottomUp f) >>> In >>> f

-- data Ast1
--   = Symbol1 String
--   | Compound1 [Ast1]
--   | Ellipses1 Ast1
--   deriving (Show, Eq)

-- data Ast1F a
--   = Symbol1F String
--   | Compound1F [a]
--   | Ellipses1F a
--   deriving (Show, Eq, Functor)

-- type Ast1F' = Term Ast1F

-- flattenTerm :: Ast1F' -> Ast1F'
-- flattenTerm (In (Ellipses1F x)) = x
-- flattenTerm x = x

-- flatten'' :: Ast1F' -> Ast1F'
-- flatten'' = bottomUp flattenTerm

-- intoFrame :: Ast1 -> Ast1F'
-- intoFrame (Symbol1 x) = In (Symbol1F x)
-- intoFrame (Compound1 xs) = In (Compound1F $ map intoFrame xs)
-- intoFrame ()

-- x = flatten'' (In (Symbol1F "Hello"))

-- flattenTerm :: Ast0F' -> Ast0F'
-- flattenTerm (In ())

-- flatten :: Ast0 -> [String]
-- flatten (Symbol s) = [s]
-- flatten (Compound xs) = concatMap flatten xs

-- ast0 :: [String]
-- ast0 = flatten (Compound [Compound [Symbol "a", Symbol "b"], Compound [Symbol "c", Symbol "d"]])

-- -- ast0 :: Ast0F Int
-- -- ast0 = (*2) <$> CompoundF [1, 2, 3]

-- -- ast1 :: Ast0F (Ast0F Int)
-- -- ast1 = fmap (*2) <$> CompoundF [SymbolF "Hello, world!", CompoundF [1, 2, 3], CompoundF [4, 5, 6]]

-- print $ factorial 500
-- main = print $ quicksort [5, 4, 3, 2, 1]
-- factorial :: Integer -> Integer
-- factorial = hylo merge split
--   where
--     split :: Integer -> ListF Integer Integer
--     split 0 = Nil
--     split n = Cons n (n - 1)

--     merge :: ListF Integer Integer -> Integer
--     merge Nil = 1
--     merge (Cons n acc) = n * acc

-- data BinTreeF a b = BTip | BBranch b a b deriving (Functor)

-- quicksort :: (Ord a) => [a] -> [a]
-- quicksort = hylo merge split
--   where
--     split :: (Ord a) => [a] -> BinTreeF a [a]
--     split [] = BTip
--     split (x : xs) = let (l, r) = partition (< x) xs in BBranch l x r

--     merge :: BinTreeF a [a] -> [a]
--     merge BTip = []
--     merge (BBranch l x r) = l ++ [x] ++ r

-- factorial :: Integer -> Integer
-- factorial = cata merge . ana split
--   where
--     -- merge :: ListF Integer Integer -> Integer
--     merge Nil = 0
--     merge (Cons x acc) = x * acc
--     -- split :: Integer -> ListF Integer Integer
--     split 0 = Nil
--     split n = Cons n (n - 1)
-- factorial = hylo merge split
--   where
--     merge [] = 1
--     merge (x : xs) = x * merge xs

--     split 0 = []
--     split n = n : split (n - 1)

-- merge :: Maybe (Integer, Integer) -> Integer
-- merge (Just (a, b)) = a * b
-- merge Nothing = 1

-- split :: Integer -> Maybe (Integer, Integer)
-- split 0 = Nothing
-- split n = Just (n, n-1)

-- merge :: [Integer] -> Integer
-- merge = cata $ \case
--   Nil -> undefined
--   Cons x acc -> x * acc

-- split :: Integer -> [Integer]
-- split = ana $ \x -> if x == 0 then undefined else Cons x (x - 1)
