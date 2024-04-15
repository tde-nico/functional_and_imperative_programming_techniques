import HW2

-- ES 1

test1_1 = do
	let arr = [5,3,4,2,1]
	putStrLn $ "Test Case 1.1: " ++ show arr
	putStrLn $ "iterativeMergeSort: " ++ show (iterativeMergeSort arr)
	-- [1,2,3,4,5]

test1_2 = do
	let arr = [5,3,4,2,1]
	let arr2 = [1,2,3,4,5]
	putStrLn $ "Test Case 1.2: " ++ show arr
	putStrLn $ "iterativeMergeSort2: " ++ show (iterativeMergeSort2 arr)
	-- [1,2,3,4,5]
	putStrLn $ "Test Case 1.2: " ++ show arr2
	putStrLn $ "iterativeMergeSort2: " ++ show (iterativeMergeSort2 arr2)
	-- [1,2,3,4,5]

test1 = do
	test1_1
	putChar '\n'
	test1_2


-- ES 2

test2_1 = do
	let tree = Node 1 (Node 2 Empty Empty) (Node 3 (Node 4 Empty Empty) Empty)
	putStrLn $ "Test Case 2.1: " ++ show tree
	putStrLn $ "mapBT: " ++ show (mapBT (+ 1) tree)
	-- Node 2 (Node 3 Empty Empty) (Node 4 (Node 5 Empty Empty) Empty)
	putStrLn $ "foldrBT: " ++ show (foldrBT (:) [] tree)
	-- [1,2,3,4]
	putStrLn $ "foldlBT: " ++ show (foldlBT (flip (:)) [] tree)
	-- [4,3,2,1]

	let tree2 = Node' (Node' (Leaf 1) (Leaf 2)) (Node' (Node' (Leaf 3) (Leaf 4)) (Leaf 5))
	putStrLn $ "Test Case 2.1: " ++ show tree2
	putStrLn $ "mapBT': " ++ show (mapBT' (+ 1) tree2)
	-- Node' (Node' (Leaf 2) (Leaf 3)) (Node' (Node' (Leaf 4) (Leaf 5)) (Leaf 6))
	putStrLn $ "foldrBT': " ++ show (foldrBT' (:) [] tree2)
	-- [1,2,3,4,5]
	putStrLn $ "foldlBT': " ++ show (foldlBT' (flip (:)) [] tree2)
	-- [5,4,3,2,1]

test2_2 = do
	let tree = Node 1 (Node 2 Empty (Node 3 Empty Empty)) (Node 3 (Node 4 Empty Empty) Empty)
	putStrLn $ "Test Case 2.2: " ++ show tree
	putStrLn $ "countNodes: " ++ show (countNodes tree)
	-- 5
	putStrLn $ "heightBT: " ++ show (heightBT tree)
	-- 3
	putStrLn $ "maxImbalance: " ++ show (maxImbalance tree)
	-- 1
	let tree2 = Node 1 (Node 2 Empty (Node 3 (Node 4 Empty Empty) Empty)) (Node 5 Empty (Node 6 (Node 7 (Node 8 Empty (Node 9 Empty Empty)) Empty) Empty))
	putStrLn $ "Test Case 2.2: " ++ show tree2
	putStrLn $ "countNodes: " ++ show (countNodes tree2)
	-- 9
	putStrLn $ "heightBT: " ++ show (heightBT tree2)
	-- 6
	putStrLn $ "maxImbalance: " ++ show (maxImbalance tree2)
	-- 4

	let tree3 = Node' (Node' (Leaf 1) (Leaf 2)) (Node' (Node' (Leaf 3) (Leaf 4)) (Leaf 5))
	putStrLn $ "Test Case 2.2: " ++ show tree3
	putStrLn $ "countNodes': " ++ show (countNodes' tree3)
	-- 5
	putStrLn $ "heightBT': " ++ show (heightBT' tree3)
	-- 3
	putStrLn $ "maxImbalance': " ++ show (maxImbalance' tree3)
	-- 1
	let tree4 = Node' (Node' (Leaf 1) (Leaf 2)) (Node' (Leaf 3) (Node' (Leaf 4) (Node' (Leaf 5) (Node' (Leaf 6) (Node' (Leaf 7) (Leaf 8))))))
	putStrLn $ "Test Case 2.2: " ++ show tree4
	putStrLn $ "countNodes': " ++ show (countNodes' tree4)
	-- 8
	putStrLn $ "heightBT': " ++ show (heightBT' tree4)
	-- 6
	putStrLn $ "maxImbalance': " ++ show (maxImbalance' tree4)
	-- 4

test2_3 = do
	let tree = R 1 [R 2 [], R 3 [R 7 []], R 4 [], R 5 [R 8 [], R 9 []], R 6 [R 10 []]]
	putStrLn $ "Test Case 2.3: " ++ show tree
	putStrLn $ "mapT: " ++ show (mapT (+ 1) tree)
	-- R 2 [R 3 [],R 4 [R 8 []],R 5 [],R 6 [R 9 [],R 10 []],R 7 [R 11 []]]
	putStrLn $ "foldrT: " ++ show (foldrT (:) [] tree)
	-- [1,2,3,7,4,5,8,9,6,10]
	putStrLn $ "foldlT: " ++ show (foldlT (flip (:)) [] tree)
	-- [10,6,9,8,5,4,7,3,2,1]

test2 = do
	test2_1
	putChar '\n'
	test2_2
	putChar '\n'
	test2_3


-- ES 3

test3 = do
	let tree1 = Node 1 (Node 2 Empty Empty) (Node 3 Empty Empty)
	let tree2 = Node 10 (Node 5 Empty Empty) (Node 15 Empty Empty)
	let tree3 = Node 10 (Node 15 Empty Empty) (Node 5 Empty Empty)
	let tree4 = Node 0 (Node 0 Empty Empty) (Node 0 Empty Empty)
	let tree5 = Node 10 (Node 5 Empty Empty) (Node 5 Empty Empty)
	let tree6 = Node 40 (Node 10 (Node 5 Empty Empty) (Node 5 Empty Empty))
		(Node 0 (Node 10 Empty Empty) (Node 10 Empty Empty))

	putStrLn $ "Test Case 3: " ++ show tree1
	putStrLn $ "nodiEquilibrati: " ++ show (nodiEquilibrati tree1)
	-- []
	putStrLn $ "Test Case 3: " ++ show tree2
	putStrLn $ "nodiEquilibrati: " ++ show (nodiEquilibrati tree2)
	-- []
	putStrLn $ "Test Case 3: " ++ show tree3
	putStrLn $ "nodiEquilibrati: " ++ show (nodiEquilibrati tree3)
	-- []
	putStrLn $ "Test Case 3: " ++ show tree4
	putStrLn $ "nodiEquilibrati: " ++ show (nodiEquilibrati tree4)
	-- [0,0,0]
	putStrLn $ "Test Case 3: " ++ show tree5
	putStrLn $ "nodiEquilibrati: " ++ show (nodiEquilibrati tree5)
	-- [10]
	putStrLn $ "Test Case 3: " ++ show tree6
	putStrLn $ "nodiEquilibrati: " ++ show (nodiEquilibrati tree6)
	-- [40,10]


-- ES 4

test4 = do
	let arr1 = [1,2,3,4,5]
	let arr2 = [5,4,3,2,1]
	let arr3 = [1,2,3,4,5,4,3,2,1]
	let arr4 = [5,4,3,2,1,2,3,4,5]
	let arr5 = [2,9,7,1,6,4,3,8,5]

	putStrLn $ "Test Case 4: " ++ show arr1
	putStrLn $ "listToABR: " ++ show (listToABR arr1)
	-- Node 5 (Node 4 (Node 3 (Node 2 (Node 1 Empty Empty) Empty) Empty) Empty) Empty
	putStrLn $ "Test Case 4: " ++ show arr2
	putStrLn $ "listToABR: " ++ show (listToABR arr2)
	-- Node 1 Empty (Node 2 Empty (Node 3 Empty (Node 4 Empty (Node 5 Empty Empty))))
	putStrLn $ "Test Case 4: " ++ show arr3
	putStrLn $ "listToABR: " ++ show (listToABR arr3)
	--  Node 1 Empty (Node 2 (Node 1 Empty Empty) (Node 3 (Node 2 Empty Empty) (Node 4 (Node 3 Empty Empty) (Node 5 (Node 4 Empty Empty) Empty))))
	putStrLn $ "Test Case 4: " ++ show arr4
	putStrLn $ "listToABR: " ++ show (listToABR arr4)
	-- Node 5 (Node 4 (Node 3 (Node 2 (Node 1 Empty Empty) (Node 2 Empty Empty)) (Node 3 Empty Empty)) (Node 4 Empty Empty)) (Node 5 Empty Empty)
	putStrLn $ "Test Case 4: " ++ show arr5
	putStrLn $ "listToABR: " ++ show (listToABR arr5)
	-- Node 5 (Node 3 (Node 1 Empty (Node 2 Empty Empty)) (Node 4 Empty Empty)) (Node 8 (Node 6 Empty (Node 7 Empty Empty)) (Node 9 Empty Empty))


-- ES 5

test5 = do
	let arr1 = [1,2,3,4,5]
	putStrLn $ "Test Case 5: " ++ show arr1
	putStrLn $ "scanr: " ++ show (scanr (+) 0 arr1)
	putStrLn $ "originalScanr: " ++ show (originalScanr (+) 0 arr1)
	putStrLn $ "myScanr: " ++ show (myScanr (+) 0 arr1)
	-- [15,14,12,9,5,0]


-- MAIN

main = do
	test1
	putStr "\n\n\n"
	test2
	putStr "\n\n\n"
	test3
	putStr "\n\n\n"
	test4
	putStr "\n\n\n"
	test5

