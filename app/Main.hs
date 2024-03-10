-- {-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DeriveFunctor #-}
-- {-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
-- {-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeFamilies #-}

module Main (main) where

import Data.List qualified

-- import Control.Arrow ((>>>))

main :: IO ()
main = print . display1 $ compile0to1 (Compound0 [Symbol0 "a", Symbol0 "x", Symbol0 "..", Symbol0 "..", Symbol0 "q"])

type family Base t :: * -> *

class (Functor (Base t)) => Recursive t where
  project :: t -> Base t t

class (Functor (Base t)) => Corecursive t where
  embed :: Base t t -> t

data ListF a r = Nil | Cons a r

instance Functor (ListF a) where
  fmap _ Nil = Nil
  fmap f (Cons a r) = Cons a (f r)

type instance Base [a] = ListF a

instance Recursive [a] where
  project [] = Nil
  project (x : xs) = Cons x xs

instance Corecursive [a] where
  embed Nil = []
  embed (Cons x xs) = x : xs

cata :: (Recursive t) => (Base t a -> a) -> t -> a
cata f = f . fmap (cata f) . project

ana :: (Corecursive t) => (a -> Base t a) -> a -> t
ana f = embed . fmap (ana f) . f

hylo :: (Functor f) => (f b -> b) -> (a -> f a) -> a -> b
hylo f g = h where h = f . fmap h . g

length :: [a] -> Integer
length = cata $ \case
  Nil -> 0
  Cons _ acc -> 1 + acc

filter :: (a -> Bool) -> [a] -> [a]
filter p = cata $ \case
  Nil -> []
  Cons x acc -> if p x then x : acc else acc

zip :: ([a], [b]) -> [(a, b)]
zip = ana $ \case
  (a : as, b : bs) -> Cons (a, b) (as, bs)
  _ -> Nil

zip' :: ([a], [b]) -> [(a, b)]
zip' (a : as, b : bs) = (a, b) : zip' (as, bs)
zip' _ = []

data Ast0
  = Symbol0 String
  | Compound0 [Ast0]
  deriving (Show)

data Ast0F r
  = Symbol0F String
  | Compound0F [r]
  deriving (Show, Functor)

type instance Base Ast0 = Ast0F

instance Recursive Ast0 where
  project (Symbol0 s) = Symbol0F s
  project (Compound0 xs) = Compound0F xs

instance Corecursive Ast0 where
  embed (Symbol0F s) = Symbol0 s
  embed (Compound0F xs) = Compound0 xs

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

data Tree a = Leaf a | Branch [Tree a] deriving (Show, Functor)

data TreeF a r = LeafF a | BranchF [r] deriving (Show, Functor)

type instance Base (Tree a) = (TreeF a)

instance Recursive (Tree a) where
  project (Leaf x) = LeafF x
  project (Branch xs) = BranchF xs

instance Corecursive (Tree a) where
  embed (LeafF a) = Leaf a
  embed (BranchF xs) = Branch xs
 
countNodes :: Tree a -> Integer
countNodes = cata $ \case
  LeafF _ -> 1
  BranchF xs -> 1 + sum xs



-- read0 :: String -> Ast0
-- read0 = hylo f g
--   where
--     g :: String -> Tree String
--     g = ana $ \case
--       ['(', xs] -> undefined
--     f :: Tree Ast0 -> Ast0
--     f = undefined

display0 :: Ast0 -> String
display0 = cata $ \case
  Symbol0F s -> s
  Compound0F xs -> "(" ++ unwords xs ++ ")"

display1 :: Ast1 -> String
display1 = cata $ \case
  Symbol1F s -> s
  Compound1F xs -> "(" ++ unwords xs ++ ")"
  Ellipses1F x -> x ++ " .."

compile0to1 :: Ast0 -> Ast1
compile0to1 = cata $ \case
  Symbol0F s -> Symbol1 s
  Compound0F xs -> Compound1 $ go xs
    where
      go :: [Ast1] -> [Ast1]
      go (x : Symbol1 ".." : xs) = go $ Ellipses1 x : xs
      go (x : xs) = x : go xs
      go [] = []

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