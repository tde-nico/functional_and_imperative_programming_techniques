module HW3 where

-- ES 1: Insomnia

insomnia :: [Char]
insomnia = concatMap (\x -> show x ++ " sheep ") [1..]


-- ES 2: Triangolo di Tartaglia

tartaglia :: [[Integer]]
tartaglia = iterate (\row -> zipWith (+) (0 : row) (row ++ [0])) [1]


-- ES 3: Numeri Fortunati

generator1 :: [Integer]
generator1 = [x | (x, i) <- zip [1..] [0..], even i]
generator2 :: [Integer]
generator2 = [x | (x, i) <- zip generator1 [0..], i `mod` 3 /= 2]
generator3 :: [Integer]
generator3 = [x | (x, i) <- zip generator2 [0..], i `mod` 7 /= 6]

fortunati :: [Integer]
fortunati = generator3

-- ES 1D: Iterazione, Ricorsione Primitiva, Church & Ackerman

-- 1
primRec :: (Int -> a -> a) -> a -> Int -> a
primRec h g 0 = g
primRec h g n = h n (primRec h g (n-1))

-- 2
for :: (Eq t, Num t) => (b -> b) -> t -> b -> b
for f 0 = id
for f n = f . for f (n-1)

primRec' :: (Int -> a -> a) -> a -> Int -> a
primRec' h g n = snd $ for (\(i, x) -> (i + 1, h i x)) (n+1) (0, g)

-- 3
church :: Int -> (a -> a) -> a -> a
church 0 _ x = x
church n f x = f (church (n-1) f x)

primRecChurch :: (a -> a) -> a -> Int -> a
primRecChurch f x n = church n f x

-- 4
ackermann :: Int -> Int -> Int
ackermann m n
    | m == 0 = n + 1
    | m > 0 && n == 0 = ackermann (m - 1) 1
    | m > 0 && n > 0 = ackermann (m - 1) (ackermann m (n - 1))
-- TODO



