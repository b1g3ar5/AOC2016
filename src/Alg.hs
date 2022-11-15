{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Alg where

import Data.List (unfoldr, delete)

newtype Fix f = Fix { unFix :: f (Fix f) } 


type Coalgebra f a = (a -> f a)
ana :: Functor f => Coalgebra f a -> a -> Fix f
ana coalg = Fix . fmap (ana coalg) . coalg
type Algebra f a = f a -> a
cata :: Functor f => Algebra f a -> Fix f -> a
cata alg = alg . fmap (cata alg) . unFix
hylo :: Functor f => Algebra f a -> Coalgebra f b -> b -> a
--hylo f g = f . fmap (hylo f g) . g
hylo f g = h where h = f . fmap h . g
fix :: (a -> a) -> a
fix f = let {x = f x} in x


-- List with the recursion switched for an extra parameter
data ListF a r = NilF | ConsF a r deriving (Show, Functor)
data F1F z = Zero | One | Plus z z deriving (Show, Functor)
data TreeF a r = NodeF a [r] deriving (Show, Functor)

type List a = Fix (ListF a)
type F1 = Fix F1F
type Tree a = Fix (TreeF a)


f :: Algebra F1F Int --F1F Int -> Int
f Zero = 0
f One = 1
f (Plus x y) = x + y


g :: Algebra F1F [Int] --F1F [Int] -> [Int]
g Zero = [0]
g One = [1]
g (Plus x y) = x ++ y


h :: Algebra (ListF a) Int --ListF b Int -> Int
h NilF = 0
h (ConsF x xs) = 1 + xs


ex1 :: F1
ex1 = Fix (Plus (Fix One) (Fix Zero))


ex2 :: List Integer
ex2 = Fix (ConsF 1 (Fix NilF))


fixAlg :: Algebra f (Fix f)
fixAlg = Fix


len :: Num a => List b -> a
len (Fix NilF) = 0
len (Fix (ConsF x xs)) = 1 + len xs


app :: List a -> List a -> List a
app (Fix NilF) xs = xs
app xs (Fix NilF) = xs
app (Fix (ConsF x xs)) ys = Fix (ConsF x (app xs ys))


len2 :: Num a => List b -> a
len2 = cata phi
  where
    phi NilF = 0
    phi (ConsF x n) = 1 + n


--app2 :: Fix (ListF a) -> Fix (ListF a) -> Fix (ListF a)
app2 :: List a -> List a -> List a
app2 x y = cata phi x
  where
    phi NilF = y
    phi (ConsF x xs) = Fix (ConsF x xs)


countDown  :: Coalgebra (ListF Int) Int -- Int -> ListF Int Int
countDown 0 = NilF
countDown n = ConsF n (n-1)


split :: Coalgebra F1F Int -- Int -> FIF Int
split 0 = Zero
split 1 = One
split n = Plus (n-1) (n-2)


-- An f-algebra is a function of type forall z. f z -> z, which intuitively removes the structure of an f. 
-- If you think about it, this is spiritually what a fold does; it removes some structure as it reduces to some value.


length :: [a] -> Int
length = foldr (\_ n -> n + 1) 0


lengthAlgebra :: ListF a Int -> Int
lengthAlgebra NilF        = 0
lengthAlgebra (ConsF _ n) = n + 1



filter :: forall a. (a -> Bool) -> [a] -> [a]
filter p = foldr step []
  where
    step a as =
      if p a then 
        a:as
        else as


filterAlgebra :: (a -> Bool) -> ListF a [a] -> [a]
filterAlgebra _  NilF        = []
filterAlgebra p (ConsF a as) =
  if p a then 
    a:as
    else as


zip :: ([a], [b]) -> [(a, b)]
zip = unfoldr produce
  where
    produce (as, bs) =
      if null as || null bs then 
        Nothing
        else Just ((head as, head bs), (tail as, tail bs))


zipCoalgebra :: ([a], [b]) -> ListF (a, b) ([a], [b])
zipCoalgebra (as, bs) =
  if null as || null bs then 
    NilF
    else ConsF (head as, head bs) (tail as, tail bs)


iterate :: (a -> a) -> a -> [a]
iterate f = unfoldr (\a -> Just (a, f a))


iterateAlgebra :: (a -> a) -> a -> ListF a a
iterateAlgebra f a = ConsF a (f a)


ex10 :: List Integer --Fix (ListF Integer)
ex10 = Fix $ ConsF 1 $ Fix $ ConsF 2 $ Fix $ ConsF 3 $ Fix $ ConsF 4 $ Fix $ ConsF 5 $ Fix $ ConsF 6 $ Fix $ ConsF 7 $ Fix NilF


coAlg :: Coalgebra (ListF Int) [Int]
coAlg [] = NilF
coAlg (n:ns) = ConsF n ns


algMain :: IO ()
algMain = do
  let p = ana coAlg [1,2,3,4,5,6]
  putStrLn $ "p: " ++ show (cata lengthAlgebra p)
  putStrLn $ "p: " ++ show (hylo lengthAlgebra coAlg [1,2,3,4,5])
  