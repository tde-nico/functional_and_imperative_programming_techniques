module HW1 where
import Data.List (sort, sortBy)

-- ES 1: Rimozione Duplicati

-- 1.1

myTakeWhile :: (t -> Bool) -> [t] -> [t]
myTakeWhile _ [] = []
myTakeWhile p (x:xs)
	| p x = x : myTakeWhile p xs
	| otherwise = []

myDropWhile :: (t -> Bool) -> [t] -> [t]
myDropWhile _ [] = []
myDropWhile p xs@(x:xs')
	| p x = myDropWhile p xs'
	| otherwise = xs

-- 1.2

myRemoveDupsOrd :: Eq t => [t] -> [t]
myRemoveDupsOrd [] = []
myRemoveDupsOrd [x] = [x]
myRemoveDupsOrd (x:x':xs)
	| x == x' = myRemoveDupsOrd (x':xs)
	| otherwise = x : myRemoveDupsOrd (x':xs)

-- 1.3

myRemoveDups :: (Eq a, Ord a) => [a] -> [a]
myRemoveDups xs = map fst . sortBy (\(_, b1) (_, b2) -> compare b1 b2) . removeDupsWithIndex . sort . zip xs $ [0..] where
	removeDupsWithIndex [] = []
	removeDupsWithIndex [x] = [x]
	removeDupsWithIndex (x:x':xs)
		| fst x == fst x' = removeDupsWithIndex (x:xs)
		| otherwise = x : removeDupsWithIndex (x':xs)

{-
myRemoveDups :: Eq t => [t] -> [t]
myRemoveDups [] = []
myRemoveDups [x] = [x]
myRemoveDups (x:xs) = let y = filter (x /=) xs in x : myRemoveDups y
-}

{-
import qualified Data.Set as Set

_myRemoveDups _ [] = []
_myRemoveDups seen (x:xs)
	| Set.member x seen = _myRemoveDups seen xs
	| otherwise = x : _myRemoveDups (Set.insert x seen) xs

myRemoveDups :: Ord a => [a] -> [a]
myRemoveDups xs = _myRemoveDups Set.empty xs
-}


-- ES 2: Interdefinibilità di Funzionali

zapp :: [a -> b] -> [a] -> [b]
zapp (f:fs)(x:xs) = f x : zapp fs xs
zapp _ _ = []

-- 2.1

myZipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
myZipWith f xs ys = zapp (map f xs) ys

-- 2.2

myZipWith2 :: (a -> b -> c) -> [a] -> [b] -> [c]
myZipWith2 f xs ys = [f x y | (x, y) <- zip xs ys]

-- 2.3

myMap :: (a -> b) -> [a] -> [b]
myMap f xs = foldr (\y zs -> f y : zs) [] xs

myMap2 :: (a -> b) -> [a] -> [b]
myMap2 f xs = foldl (\zs y -> zs ++ [f y]) [] xs

-- 2.4

{-
map non può essere usata per reimplementare foldr e foldl,
questo poichè, già come si può intuire dai tipi:
map :: (a -> b) -> [a] -> [b]
foldr :: (a -> b -> b) -> b -> t a -> b
foldl :: (b -> a -> b) -> b -> t a -> b
da questi possiamo notare che map prende una funzione da
applicare con un solo parametro, e le fold prendono una
funzione con 2 parametri, che basta visto che map non ha
accesso a 2 parametri ma solo ad uno, ed inoltre il secondo
parametro dovrebbe essere l'elemento successivo alla lista
che di base non può quindi prendere, quindi la funzione
map di default non ci permette di implementare le fold.
-}


-- ES 3: Segmenti e Sottoliste

suffissi :: [a] -> [[a]]
suffissi [] = []
suffissi xs@(_:txs) = xs:suffissi txs

-- 3.1

prefissi :: [a] -> [[a]]
prefissi [] = []
prefissi xs = prefissi (init xs) ++ [xs]

-- 3.2

segSommaS :: (Eq a, Num a) => [a] -> a -> [[a]]
segSommaS xs s = concatMap (filter ((== s) . sum) . prefissi) (suffissi xs)

-- 3.3

sublSommaS :: (Eq a, Num a) => [a] -> a -> [[a]]
sublSommaS xs s = filter ((== s) . sum) (mySubsequences xs) where
	mySubsequences :: [a] -> [[a]]
	mySubsequences [] = [[]]
	mySubsequences (x:xs) = let sub = mySubsequences xs in sub ++ map (x:) sub


-- ES 4: Partizioni

-- 4.1

-- https://en.wikipedia.org/wiki/Integer_partition
-- https://en.wikipedia.org/wiki/Triangle_of_partition_numbers
part :: Int -> Integer
part n = toInteger (p n n) where
	p _ 0 = 0
	p n k = pk n k + p n (k-1) where
		pk n k
			| (n == 0) && (k == 0) = 1
			| (n <= 0) || (k <= 0) = 0
			| otherwise = pk (n-1) (k-1) + pk (n-k) k

-- 4.2

part1 :: Int -> Integer
part1 n
	| n == 0 = 1
	| otherwise = 2 ^ (n-1)

-- 4.3

parts :: Int -> [[Int]]
parts 0 = [[]]
parts n = [x:xs | x <- [1..n], xs <- parts (n-x), x <= head (xs ++ [n])]

-- 4.4

part2 :: Int -> Integer
part2 n = toInteger (length (parts n))

{-
A livello di efficienza, la versione standard è migliore, visto che
a meno di iplementazioni prettamente matematiche, permetterebbe di sfruttare
la laziness di Haskell, e quindi di non computare realmente le partizioni
che a differenza di questa versione deve obbligatoriamente computare a priori.
-}

