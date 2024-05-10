import HW3

-- ES 1

test1 = do
	print $ take (8*10) insomnia
	-- "1 sheep 2 sheep 3 sheep 4 sheep 5 sheep 6 sheep 7 sheep 8 sheep 9 sheep 10 sheep"


-- ES 2

test2 = do
	print $ tartaglia !! 0
	-- [1]
	print $ tartaglia !! 1
	-- [1,1]
	print $ tartaglia !! 2
	-- [1,2,1]
	print $ tartaglia !! 3
	-- [1,3,3,1]
	print $ tartaglia !! 4
	-- [1,4,6,4,1]
	print $ tartaglia !! 5
	-- [1,5,10,10,5,1]
	
	print $ tartaglia !! 10 !! 5
	-- 252


-- ES 3

test3 = do
	print $ take 30 generator1
	-- [1,3,5,7,9,11,13,15,17,19,21,23,25,27,29,31,33,35,37,39,41,43,45,47,49,51,53,55,57,59]
	print $ take 30 generator2
	-- [1,3,7,9,13,15,19,21,25,27,31,33,37,39,43,45,49,51,55,57,61,63,67,69,73,75,79,81,85,87]
	print $ take 30 generator3
	-- [1,3,7,9,13,15,21,25,27,31,33,37,43,45,49,51,55,57,63,67,69,73,75,79,85,87,91,93,97,99]
	print $ take 30 fortunati
	-- [1,3,7,9,13,15,21,25,27,31,33,37,43,45,49,51,55,57,63,67,69,73,75,79,85,87,91,93,97,99]


-- ES 1D

test1D = do
	print $ primRec (+) 0 10
	-- 55
	print $ primRec' (+) 0 10
	-- 55
	print $ primRecq (+2) 0 10
	-- 20
	print $ ackermann 3 4
	-- 125

-- MAIN

main = do
	test1
	putStr "\n\n\n"
	test2
	putStr "\n\n\n"
	test3
	putStr "\n\n\n"
	test1D
	putStr "\n\n\n"

