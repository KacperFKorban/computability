{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}

import GHC.Base (assert)
import Data.Foldable (traverse_)

-- Ex 2
data Nat :: * where
  NZero :: Nat
  NSuc :: Nat -> Nat
  deriving (Show, Eq)

data Z :: * where
  ZZero :: Z
  ZPlusSuc :: Nat -> Z
  ZMinusSuc :: Nat -> Z
  deriving (Show, Eq)

minusOne :: Z
minusOne = ZMinusSuc NZero

zero :: Z
zero = ZZero

one :: Z
one = ZPlusSuc NZero

three :: Z
three = ZPlusSuc $ NSuc $ NSuc NZero
-- End Ex 2

-- Ex 3
zrec :: a -> (Nat -> a) -> (Nat -> a) -> Z -> a
zrec fz _ _ ZZero = fz
zrec _ fm _ (ZMinusSuc n) = fm n
zrec _ _ fp (ZPlusSuc n) = fp n

nrec :: a -> (Nat -> a -> a) -> Nat -> a
nrec fz _ NZero = fz
nrec fz fs (NSuc n) = fs n $ nrec fz fs n

nadd :: Nat -> Nat -> Nat
nadd n m = nrec m (\_ r -> NSuc r) n

nfromInt :: Int -> Nat
nfromInt 0 = NZero
nfromInt x | x > 0 = NSuc $ nfromInt $ x - 1

ntoInt :: Nat -> Int
ntoInt NZero = 0
ntoInt (NSuc x) = 1 + ntoInt x

zsuc :: Z -> Z
zsuc = zrec (ZPlusSuc NZero) (nrec ZZero (\b _ -> ZMinusSuc b)) (ZPlusSuc . NSuc)

zfromInt :: Int -> Z
zfromInt 0 = ZZero
zfromInt x | x > 0 = ZPlusSuc $ nfromInt $ x - 1
zfromInt x | x < 0 = ZMinusSuc $ nfromInt $ (-x) - 1

ztoInt :: Z -> Int
ztoInt ZZero = 0
ztoInt (ZPlusSuc x) = 1 + ntoInt x
ztoInt (ZMinusSuc x) = -(ntoInt x) - 1
-- End Ex 3

-- Ex 4
data PRF :: * where
  PZero :: PRF
  PSuc :: PRF
  PProj :: Int -> PRF
  PComp :: PRF -> [PRF] -> PRF
  PRec :: PRF -> PRF -> PRF
  deriving (Show, Eq)

eval :: PRF -> [Int] -> Int
eval PZero [] = 0
eval PSuc [n] = n + 1
eval (PProj i) xs = xs !! i
eval (PComp f gs) xs = eval f $ pmap gs xs
eval (PRec f g) (n:xs) = irec (eval f xs) (\v r -> eval g (r:v:xs)) n

irec :: a -> (Int -> a -> a) -> Int -> a
irec fz _ 0 = fz
irec fz fs x | x > 0 = fs (x - 1) $ irec fz fs (x - 1)

pmap :: [PRF] -> [Int] -> [Int]
pmap [] _ = []
pmap (f:fs) xs = eval f xs : pmap fs xs

padd :: PRF
padd = PRec (PProj 0) (PComp PSuc [PProj 0])
-- End Ex 4

main = do
  -- Ex 2 test
  putStrLn "Nat Addition"
  traverse_ (\(a, b) ->
      print $ assert (a + b == ntoInt (nadd (nfromInt a) (nfromInt b))) (a, b, ntoInt (nadd (nfromInt a) (nfromInt b)))
    ) $ (,) <$> [0..10] <*> [0..10]
  -- End Ex 2 test

  -- Ex 3 test
  putStrLn "Z Suc"
  traverse_ (\a ->
      print $ assert (a + 1 == ztoInt (zsuc $ zfromInt a)) (a + 1, ztoInt (zsuc $ zfromInt a))
    ) [-10..10]
  -- End Ex 3 test

  -- Ex 4 test
  putStrLn "PRF add"
  traverse (\(a, b) ->
      print $ assert (a + b == eval padd [a, b]) (a, b, eval padd [a, b])
    ) $ (,) <$> [0..10] <*> [0..10]
  -- End Ex 4 test
