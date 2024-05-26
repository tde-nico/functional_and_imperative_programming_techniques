import HW4

-- ES 1

test1 = do
	charCount

-- ES 2

test2 = do
	let tree1 = Node 1 (Node 2 Empty Empty) (Node 3 Empty Empty)
	let tree2 = Node 10 (Node 5 Empty Empty) (Node 15 Empty Empty)
	let tree3 = Node 10 (Node 15 Empty Empty) (Node 5 Empty Empty)
	let tree4 = Node 0 (Node 0 Empty Empty) (Node 0 Empty Empty)
	let tree5 = Node 10 (Node 5 Empty Empty) (Node 5 Empty Empty)
	let tree6 = Node 40 (Node 10 (Node 5 Empty Empty) (Node 5 Empty Empty))
		(Node 0 (Node 10 Empty Empty) (Node 10 Empty Empty))

	putStrLn $ "Test Case 3: " ++ show tree1
	print $ nodiEquilibrati tree1
	print $ nodiEquilibratiHW2 tree1
	-- []
	putStrLn $ "Test Case 3: " ++ show tree2
	print $ nodiEquilibrati tree2
	print $ nodiEquilibratiHW2 tree2
	-- []
	putStrLn $ "Test Case 3: " ++ show tree3
	print $ nodiEquilibrati tree3
	print $ nodiEquilibratiHW2 tree3
	-- []
	putStrLn $ "Test Case 3: " ++ show tree4
	print $ nodiEquilibrati tree4
	print $ nodiEquilibratiHW2 tree4
	-- [0,0,0]
	putStrLn $ "Test Case 3: " ++ show tree5
	print $ nodiEquilibrati tree5
	print $ nodiEquilibratiHW2 tree5
	-- [10]
	putStrLn $ "Test Case 3: " ++ show tree6
	print $ nodiEquilibrati tree6
	print $ nodiEquilibratiHW2 tree6
	-- [40,10]


-- ES 3

test3 = do
	print $ binAdd "0011100" "0011010"
	-- Right "0110110"
	print $ binAdd "10" "10"
	-- Left Overflow
	putChar '\n'


	print $ binSub "0011100" "0011010"
	-- Right "0000010"
	print $ binSub "1001" "0101"
	-- Right "0100"
	print $ binSub "100" "011"
	-- Right "001"
	print $ binSub "011" "100"
	-- Left NegativeNumber
	putChar '\n'


	print $ binMul "00000001" "00000000"
	-- Right "000000000000000"
	print $ binMul "00000000" "00000001"
	-- Right "000000000000000"
	print $ binMul "00000001" "00000001"
	-- Right "000000000000001"
	print $ binMul "00000011" "00000100"
	-- Right "000000000001100"
	print $ binMul "110" "110"
	-- Left Overflow
	print $ binMul "011101" "001001"
	-- Right "00100000101"
	putChar '\n'


	print $ binDivMod "00000001" "00000000"
	-- Left DivisionByZero
	print $ binDivMod "00000000" "00000001"
	-- Right ("","")
	print $ binDivMod "00011011" "00000011"
	-- Right ("1001","0000")
	print $ binDivMod "00101101" "00000110"
	-- Right ("0111","00011")
	print $ binDivMod "01011011" "00000101"
	-- Right ("10010","000001")
	putChar '\n'


	print $ binDiv "00000001" "00000000"
	-- Left DivisionByZero
	print $ binDiv "00000000" "00000001"
	-- Right ""
	print $ binDiv "00011011" "00000011"
	-- Right "1001"
	print $ binDiv "00101101" "00000110"
	-- Right "0111"
	print $ binDiv "01011011" "00000101"
	-- Right "10010"
	putChar '\n'


	print $ binMod "00000001" "00000000"
	-- Left DivisionByZero
	print $ binMod "00000000" "00000001"
	-- Right ""
	print $ binMod "00011011" "00000011"
	-- Right "0000"
	print $ binMod "00101101" "00000110"
	-- Right "00011"
	print $ binMod "01011011" "00000101"
	-- Right "000001"
	putChar '\n'


	print $ "0011100" + "0011010"
	-- Right "0110110"
	-- print $ "10" + "10"
	-- Left Overflow
	putChar '\n'


	print $ "0011100" - "0011010"
	-- Right "0000010"
	print $ "1001" - "0101"
	-- Right "0100"
	print $ "100" - "011"
	-- Right "001"
	-- print $ "011" - "100"
	-- Left NegativeNumber
	putChar '\n'


	print $ "00000001" * "00000000"
	-- Right "000000000000000"
	print $ "00000000" * "00000001"
	-- Right "000000000000000"
	print $ "00000001" * "00000001"
	-- Right "000000000000001"
	print $ "00000011" * "00000100"
	-- Right "000000000001100"
	-- print $ "110" * "110"
	-- Left Overflow
	print $ "011101" * "001001"
	-- Right "00100000101"
	putChar '\n'

	-- print $ "00000001" / "00000000"
	-- Left DivisionByZero
	print $ "00000000" / "00000001"
	-- Right ""
	print $ "00011011" / "00000011"
	-- Right "1001"
	print $ "00101101" / "00000110"
	-- Right "0111"
	print $ "01011011" / "00000101"
	-- Right "10010"
	putChar '\n'



-- MAIN

main = do
	--test1
	-- putStr "\n\n\n"
	-- test2
	-- putStr "\n\n\n"
	test3
	-- putStr "\n\n\n"
	-- test1D
	-- putStr "\n\n\n"

