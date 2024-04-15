module HW2 where
import Data.List (tails)

-- ES 1: mergeSort "Iterativo"

-- 1.1

merge :: Ord a => [a] -> [a] -> [a]
merge xs [] = xs
merge [] ys = ys
merge (x:xs) (y:ys)
	| x <= y = x : merge xs (y:ys)
	| otherwise = y : merge (x:xs) ys

mergePairs :: Ord a => [[a]] -> [[a]]
mergePairs [] = []
mergePairs [xs] = [xs]
mergePairs (xs:ys:rest) = merge xs ys : mergePairs rest

iterativeMergeSort :: Ord a => [a] -> [a]
iterativeMergeSort = head . until (null . tail) mergePairs . map (:[])

-- 1.2

isSorted :: Ord a => [a] -> Bool
isSorted [] = True
isSorted [_] = True
isSorted (x:y:xs) = x <= y && isSorted (y:xs)

iterativeMergeSort2 :: Ord a => [a] -> [a]
iterativeMergeSort2 xs
	| isSorted xs = xs
	| otherwise = head . until (null . tail) mergePairs . map (:[]) $ xs


-- ES 2: Alberi & funzionali sugli alberi

data BinTree a = Node a (BinTree a) (BinTree a) | Empty deriving Show
data BinTree' a = Node' (BinTree' a) (BinTree' a) | Leaf a deriving Show

-- 2.1

mapBT :: (a -> b) -> BinTree a -> BinTree b
mapBT _ Empty = Empty
mapBT f (Node x l r) = Node (f x) (mapBT f l) (mapBT f r)

mapBT' :: (a -> b) -> BinTree' a -> BinTree' b
mapBT' f (Leaf x) = Leaf (f x)
mapBT' f (Node' l r) = Node' (mapBT' f l) (mapBT' f r)

foldrBT :: (a -> b -> b) -> b -> BinTree a -> b
foldrBT _ z Empty = z
foldrBT f z (Node x l r) = f x (foldrBT f (foldrBT f z r) l)

foldrBT' :: (a -> b -> b) -> b -> BinTree' a -> b
foldrBT' f z (Leaf x) = f x z
foldrBT' f z (Node' l r) = foldrBT' f (foldrBT' f z r) l

foldlBT :: (b -> a -> b) -> b -> BinTree a -> b
foldlBT _ z Empty = z
foldlBT f z (Node x l r) = foldlBT f (foldlBT f (f z x) l) r

foldlBT' :: (b -> a -> b) -> b -> BinTree' a -> b
foldlBT' f z (Leaf x) = f z x
foldlBT' f z (Node' l r) = foldlBT' f (foldlBT' f z l) r

-- 2.2

countNodes :: BinTree a -> Int
countNodes = foldrBT (\_ n -> n + 1) 0

countNodes' :: BinTree' a -> Int
countNodes' = foldrBT' (\_ n -> n + 1) 0


heightBT :: BinTree a -> Int
heightBT = foldrBT (\_ hl hr -> 1 + max hl hr) 0
	where
		foldrBT :: (a -> b -> b -> b) -> b -> BinTree a -> b
		foldrBT _ z Empty = z
		foldrBT f z (Node x l r) = f x (foldrBT f z l) (foldrBT f z r)

heightBT' :: BinTree' a -> Int
heightBT' = foldrBT' (\hl hr -> 1 + max hl hr) 0
	where
		foldrBT' :: (b -> b -> b) -> b -> BinTree' a -> b
		foldrBT' _ z (Leaf _) = z
		foldrBT' f z (Node' l r) = f (foldrBT' f z l) (foldrBT' f z r)


maxImbalance :: BinTree a -> Int
maxImbalance = snd . foldrBT (\_ (hl, il) (hr, ir) -> (1 + max hl hr, max (abs (hl - hr)) (max il ir))) (0, 0)
  where
    foldrBT :: (a -> b -> b -> b) -> b -> BinTree a -> b
    foldrBT _ z Empty = z
    foldrBT f z (Node x l r) = f x (foldrBT f z l) (foldrBT f z r)

maxImbalance' :: BinTree' a -> Int
maxImbalance' = snd . foldrBT' (\(hl, il) (hr, ir) -> (1 + max hl hr, max (abs (hl - hr)) (max il ir))) (0, 0)
  where
    foldrBT' :: ((b, b) -> (b, b) -> (b, b)) -> (b, b) -> BinTree' a -> (b, b)
    foldrBT' _ z (Leaf _) = z
    foldrBT' f z (Node' l r) = f (foldrBT' f z l) (foldrBT' f z r)

-- 2.3

data Tree a = R a [Tree a] deriving Show

mapT :: (a -> b) -> Tree a -> Tree b
mapT f (R x ts) = R (f x) (map (mapT f) ts)

foldrT :: (a -> b -> b) -> b -> Tree a -> b
foldrT f z (R x ts) = f x (foldrT' f z ts)
	where
		foldrT' _ z [] = z
		foldrT' f z tss = foldrT' f (foldrT f z t) ts
			where
				t = last tss
				ts = init tss

foldlT :: (b -> a -> b) -> b -> Tree a -> b
foldlT f z (R x ts) = foldlT' (f z x) ts
	where
		foldlT' z [] = z
		foldlT' z (t:ts) = foldlT' (foldlT f z t) ts


-- ES 3: Nodi Equilibrati

{-
Questa funzione gira in O(n) in quanto ogni nodo
appartenete all'albero viene visitato una sola volta.
-}
nodiEquilibrati :: (Eq a, Num a) => BinTree a -> [a]
nodiEquilibrati = snd . nodiEquilibrati'
	where
		nodiEquilibrati' :: (Eq a, Num a) => BinTree a -> (a, [a])
		nodiEquilibrati' Empty = (0, [])
		nodiEquilibrati' (Node x l r) = (s, if x == sl + sr then x : nl ++ nr else nl ++ nr)
			where
				(sl, nl) = nodiEquilibrati' l
				(sr, nr) = nodiEquilibrati' r
				s = x + sl + sr


-- ES 4: Alberi Binari di Ricerca

listToABR :: Ord a => [a] -> BinTree a
listToABR [] = Empty
listToABR (x:xs) = insert x (listToABR xs)
	where
		insert :: Ord a => a -> BinTree a -> BinTree a
		insert y Empty = Node y Empty Empty
		insert y (Node z l r)
			| y < z = Node z (insert y l) r
			| otherwise = Node z l (insert y r)


-- ES 5: Derivazioni di programmi

originalScanr :: (a -> b -> b) -> b -> [a] -> [b]
originalScanr f e = map (foldr f e) . tails

myScanr :: (a -> b -> b) -> b -> [a] -> [b]
myScanr f z = foldl (\(y:ys) x -> f x y : y : ys) [z] . reverse
