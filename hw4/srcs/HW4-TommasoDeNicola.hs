{-# LANGUAGE UndecidableInstances #-}
module HW4 where
import qualified Data.Map.Strict as Map
import Control.Monad (replicateM , when)
import Data.Char (toLower)
import Data.List (nub)
import Control.Monad.State


-- ES 1: Input/Output

charCount :: IO()
charCount = do
	n <- readLn :: IO Int
	strings <- replicateM n getLine
	print $ countChars strings
		where
			countChars :: [String] -> Map.Map Char Int
			countChars strings = Map.fromListWith (+) [(c, 1) | s <- strings, c <- nub (map toLower s)]


-- ES 2: Nodi Equilibrati con Applicativi e Monadi

data BinTree a = Node a (BinTree a) (BinTree a) | Empty deriving Show

-- HW2 ES3 Nodi Equilibrati
nodiEquilibratiHW2 :: (Eq a, Num a) => BinTree a -> [a]
nodiEquilibratiHW2 = snd . nodiEquilibrati'
	where
		nodiEquilibrati' :: (Eq a, Num a) => BinTree a -> (a, [a])
		nodiEquilibrati' Empty = (0, [])
		nodiEquilibrati' (Node x l r) = (s, if x == sl + sr then x : nl ++ nr else nl ++ nr)
			where
				(sl, nl) = nodiEquilibrati' l
				(sr, nr) = nodiEquilibrati' r
				s = x + sl + sr


nodiEquilibrati :: (Eq a, Num a) => BinTree a -> [a]
nodiEquilibrati t = execState (nodiEquilibrati' t) []
	where
		nodiEquilibrati' :: (Eq a, Num a) => BinTree a -> State [a] a
		nodiEquilibrati' Empty = return 0
		nodiEquilibrati' (Node x l r) = do
			sl <- nodiEquilibrati' l
			sr <- nodiEquilibrati' r
			let s = x + sl + sr
			when (x == sl + sr) $ modify (x:)
			return s


-- ES 3: Monadi/Eccezioni

type NatBin = [Char]
data ArithException = DivisionByZero | NegativeNumber | Overflow deriving Show
type MaybeArith a = Either ArithException a


binAdd :: NatBin -> NatBin -> MaybeArith NatBin
binAdd x y = binAdd' (reverse x) (reverse y) '0' >>= return . reverse
	where
		binAdd' :: NatBin -> NatBin -> Char -> MaybeArith NatBin
		binAdd' [] [] '0' = Right []
		binAdd' [] [] '1' = Left Overflow
		binAdd' [] _ _ = Left Overflow
		binAdd' _ [] _ = Left Overflow
		binAdd' (x:xs) (y:ys) z = case (x, y, z) of

			('0', '0', '0') -> ('0':) <$> binAdd' xs ys '0'
			('1', '1', '0') -> ('0':) <$> binAdd' xs ys '1'
			('0', '1', '0') -> ('1':) <$> binAdd' xs ys '0'
			('1', '0', '0') -> ('1':) <$> binAdd' xs ys '0'

			('0', '0', '1') -> ('1':) <$> binAdd' xs ys '0'
			('1', '1', '1') -> ('1':) <$> binAdd' xs ys '1'
			('0', '1', '1') -> ('0':) <$> binAdd' xs ys '1'
			('1', '0', '1') -> ('0':) <$> binAdd' xs ys '1'


binSub :: NatBin -> NatBin -> MaybeArith NatBin
binSub x y = binSub' (reverse x) (reverse y) '0' >>= return . reverse
	where
		binSub' :: NatBin -> NatBin -> Char -> MaybeArith NatBin
		binSub' [] [] '0' = Right []
		binSub' [] [] '1' = Left NegativeNumber
		binSub' [] _ _ = Left Overflow
		binSub' _ [] _ = Left Overflow
		binSub' (x:xs) (y:ys) z = case (x, y, z) of

			('0', '0', '0') -> ('0':) <$> binSub' xs ys '0'
			('1', '1', '0') -> ('0':) <$> binSub' xs ys '0'
			('0', '1', '0') -> ('1':) <$> binSub' xs ys '1'
			('1', '0', '0') -> ('1':) <$> binSub' xs ys '0'

			('0', '0', '1') -> ('1':) <$> binSub' xs ys '1'
			('1', '1', '1') -> ('1':) <$> binSub' xs ys '1'
			('0', '1', '1') -> ('0':) <$> binSub' xs ys '1'
			('1', '0', '1') -> ('0':) <$> binSub' xs ys '0'


binMul :: NatBin -> NatBin -> MaybeArith NatBin
binMul x y = binMul' (reverse x) (reverse y) [] >>= binMul'' . map reverse . reverse . paddAll . reverse
	where
		zeros :: Int -> NatBin
		zeros 0 = []
		zeros n = '0' : zeros (n - 1)

		paddAll :: [NatBin] -> [NatBin]
		paddAll [] = []
		paddAll (x:xs) = x : paddAll (map ('0':) xs)

		binMul' :: NatBin -> NatBin -> [NatBin] -> MaybeArith [NatBin]
		binMul' [] _ p = Right p
		binMul' _ [] p = Right p
		binMul' y (x:xs) partials = case x of
				'0' -> binMul' y xs (zeros (length y) :partials)
				'1' -> binMul' y xs (y :partials)

		binMul'' :: [NatBin] -> MaybeArith NatBin
		binMul'' [] = Right []
		binMul'' [x] = Right x
		binMul'' (x:xs) = binMul'' xs >>= Right . ('0':) >>= binAdd x


binDivMod :: NatBin -> NatBin -> MaybeArith (NatBin, NatBin)
binDivMod x y = binDivMod' (drop (length x - bitLen x) x) (drop (length y - bitLen y) y) [] >>= return . (\(a, b) -> (zeros (length x - length a) ++ a, zeros (length x - length b) ++ b))
	where
		bitLen :: NatBin -> Int
		bitLen [] = 0
		bitLen (x:xs) = case x of
			'0' -> bitLen xs
			'1' -> 1 + length xs

		zeros :: Int -> NatBin
		zeros 0 = []
		zeros n = '0' : zeros (n - 1)

		binDivMod' :: NatBin -> NatBin -> NatBin -> MaybeArith (NatBin, NatBin)
		binDivMod' _ [] _ = Left DivisionByZero
		binDivMod' [] _ q = Right (q, [])
		binDivMod' x y q
			| length x < length y = Right (q, x)
			| part >= y && not (null x) = binSub part y >>= \pref -> binDivMod'' (drop (length pref - bitLen pref) pref ++ [x !! length pref]) (tail (drop (length pref) x)) (drop (length y - bitLen y) y) (q ++ "1")
			| part >= y = binSub part y >>= \pref -> binDivMod'' (drop (length pref - bitLen pref) pref) (drop (length pref) x) (drop (length y - bitLen y) y) (q ++ "1")
			| otherwise = binDivMod' x ('0':y) ('0':q)
				where
					part = take (length y) x

		binDivMod'' :: NatBin -> NatBin -> NatBin -> NatBin -> MaybeArith (NatBin, NatBin)
		binDivMod'' p x y q
			| length p > length y = binDivMod'' p x ('0':y) q
			| length p < length y = binDivMod'' ('0':p) x y q
			| p >= y && length x > 1 = binSub p y >>= \pref -> binDivMod'' (pref ++ [head x]) (tail x) (drop (length y - bitLen y) y) (q ++ "1")
			| p >= y && not (null x) = binSub p y >>= \pref -> binDivMod''' (pref ++ [head x]) (tail x) (drop (length y - bitLen y) y) (q ++ "1")
			| p >= y = binSub p y >>= \pref -> binDivMod'' pref x (drop (length y - bitLen y) y) (q ++ "1")
			| null x = Right (q, p)
			| otherwise = binDivMod'' (p ++ [head x]) (tail x) y (q ++ "0")

		binDivMod''' :: NatBin -> NatBin -> NatBin -> NatBin -> MaybeArith (NatBin, NatBin)
		binDivMod''' p x y q
			| length p > length y = binDivMod''' p x ('0':y) q
			| length p < length y = binDivMod''' ('0':p) x y q
			| p >= y = binSub p y >>= \pref -> Right (q ++ "1", pref)
			| otherwise = Right (q ++ "0", p)


binDiv :: NatBin -> NatBin -> MaybeArith NatBin
binDiv x y = binDivMod x y >>= return . fst


binMod :: NatBin -> NatBin -> MaybeArith NatBin
binMod x y = binDivMod x y >>= return . snd




add :: NatBin -> NatBin -> NatBin
add x y = case binAdd x y of
	Right z -> z
	Left e -> error (show e)

sub :: NatBin -> NatBin -> NatBin
sub x y = case binSub x y of
	Right z -> z
	Left e -> error (show e)

mul :: NatBin -> NatBin -> NatBin
mul x y = case binMul x y of
	Right z -> z
	Left e -> error (show e)

bindiv :: NatBin -> NatBin -> NatBin
bindiv x y = case binDiv x y of
	Right z -> z
	Left e -> error (show e)

divmod :: NatBin -> NatBin -> (NatBin, NatBin)
divmod x y = case binDivMod x y of
	Right (q, r) -> (q, r)
	Left e -> error (show e)

instance Num NatBin where
	(+) :: NatBin -> NatBin -> NatBin
	(+) = add

	(-) :: NatBin -> NatBin -> NatBin
	(-) = sub

	(*) :: NatBin -> NatBin -> NatBin
	(*) = mul

	abs :: NatBin -> NatBin
	abs = id

	signum :: NatBin -> NatBin
	signum = const 1

	fromInteger :: Integer -> NatBin
	fromInteger = show . fromInteger


instance Fractional NatBin where
	(/) :: NatBin -> NatBin -> NatBin
	(/) = bindiv

	fromRational :: Rational -> NatBin
	fromRational = undefined

instance (Enum NatBin, Real NatBin) => Integral NatBin where
	quotRem :: (Enum NatBin, Real NatBin) => NatBin -> NatBin -> (NatBin, NatBin)
	quotRem = divMod

	toInteger :: (Enum NatBin, Real NatBin) => NatBin -> Integer
	toInteger = undefined

