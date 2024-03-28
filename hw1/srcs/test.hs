import HW1

-- ES 1

test1_1 = do
	let arr = [1,2,3,4,5,6,7,8,9,10,9,8,7,6,5,4,3,2,1]
	putStr "Test Case 1.1: "
	print arr
	putStr "myTakeWhile: "
	print $ myTakeWhile (<5) arr
	-- [1,2,3,4]
	putStr "myDropWhile: "
	print $ myDropWhile (<5) arr
	-- [5,6,7,8,9,10,9,8,7,6,5,4,3,2,1]

test1_2 = do
	let arr = [1,1,2,3,3,4,5,6,6,7,8,9,9,10,10]
	putStr "Test Case 1.2: "
	print arr
	putStr "myRemoveDupsOrd: "
	print $ myRemoveDupsOrd arr
	-- [1,2,3,4,5,6,7,8,9,10]

test1_3 = do
	let arr = [1,9,2,6,3,4,5,10,6,7,9,4,8,1,10,11]
	putStr "Test Case 1.3: "
	print arr
	putStr "myRemoveDups: "
	print $ myRemoveDups arr
	-- [1,9,2,6,3,4,5,10,7,8,11]
	let arr2 = [5,2,1,2,5,7,2,1,2,7]
	putStr "Test Case 1.3: "
	print arr2
	putStr "myRemoveDups: "
	print $ myRemoveDups arr2
	-- [5,2,1,7]

test1 = do
	test1_1
	putChar '\n'
	test1_2
	putChar '\n'
	test1_3


-- ES 2

test2_1 = do
	let arr1 = [1,2,3]
	let arr2 = [4,5,6]
	putStr "Test Case 2.1: "
	print (arr1, arr2)
	putStr "myZipWith: "
	print $ myZipWith (+) arr1 arr2
	-- [5,7,9]

test2_2 = do
	let arr1 = [1,2,3]
	let arr2 = [4,5,6]
	putStr "Test Case 2.2: "
	print (arr1, arr2)
	putStr "myZipWith2: "
	print $ myZipWith2 (+) arr1 arr2
	-- [5,7,9]

test2_3 = do
	let arr = [1,2,3,4,5,6,7,8,9,10]
	putStr "Test Case 2.3: "
	print arr
	putStr "myMap: "
	print $ myMap (* 2) arr
	-- [2,4,6,8,10,12,14,16,18,20]
	putStr "myMap2: "
	print $ myMap2 (* 2) arr
	-- [2,4,6,8,10,12,14,16,18,20]

test2 = do
	test2_1
	putChar '\n'
	test2_2
	putChar '\n'
	test2_3


-- ES 3

test3_1 = do
	let arr = [1,2,3]
	putStr "Test Case 3.1: "
	print arr
	putStr "prefissi: "
	print $ prefissi arr
	-- [[1],[1,2],[1,2,3]]

test3_2 = do
	let arr = [1,2,8,2,3,7,4,5,6,4,7,8,9,1,10]
	putStr "Test Case 3.2: "
	print arr
	putStr "segSommaS: "
	print $ segSommaS arr 10
	-- [[2,8],[8,2],[3,7],[6,4],[9,1],[10]]

test3_3 = do
	let arr = [1,2,3,4,5,6,7,8,9,10]
	putStr "Test Case 3.3: "
	print arr
	putStr "sublSommaS: "
	print $ sublSommaS arr 10
	-- [[10],[4,6],[3,7],[2,8],[2,3,5],[1,9],[1,4,5],[1,3,6],[1,2,7],[1,2,3,4]]

test3 = do
	test3_1
	putChar '\n'
	test3_2
	putChar '\n'
	test3_3


-- ES 4

test4_1 = do
	let count = 4
	putStr "Test Case 4.1: "
	print count
	putStr "part: "
	print $ part count
	-- 5

test4_2 = do
	let count = 4
	putStr "Test Case 4.2: "
	print count
	putStr "part1: "
	print $ part1 count
	-- 8

test4_3 = do
	let count = 4
	putStr "Test Case 4.3: "
	print count
	putStr "parts: "
	print $ parts count
	-- [[1,1,1,1],[1,1,2],[1,3],[2,2],[4]]

test4_4 = do
	let count = 4
	putStr "Test Case 4.4: "
	print count
	putStr "part2: "
	print $ part2 count
	-- 5

test4 = do
	test4_1
	putChar '\n'
	test4_2
	putChar '\n'
	test4_3
	putChar '\n'
	test4_4


-- MAIN

main = do
	test1
	putStr "\n\n\n"
	test2
	putStr "\n\n\n"
	test3
	putStr "\n\n\n"
	test4
