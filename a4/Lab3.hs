module Lab3 where

import AbsChi

fromNatural :: Int -> Exp
fromNatural 0 = Const (Constructor "Zero") []
fromNatural n | n > 0 = Const (Constructor "Suc") [fromNatural (n - 1)]
fromNatural _ = undefined

toNatural :: Exp -> Maybe Int
toNatural (Const (Constructor "Zero") []) = Just 0
toNatural (Const (Constructor "Suc") [n]) = (+ 1) <$> toNatural n
toNatural _ = Nothing

fromList :: [Int] -> Exp
fromList [] = Const (Constructor "Nil") []
fromList (x:xs) = Const (Constructor "Cons") [fromNatural x, fromList xs]
