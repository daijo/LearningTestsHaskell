-- ListTests.hs 
-- Test cases exploring lists.
-- © 2011 Daniel Hjort

import HUnit

testAppend = TestCase (assertEqual "" "ABC" ('A':"BC"))

testConcat = TestCase (assertEqual "" [1..4] ([1,2] ++ [3,4]))

testGetElement = TestCase (assertEqual "" 'P' ("Blue Pumpkin" !! 5))

testGetElement2 = TestCase (assertEqual "" 6 ([1..] !! 5))

testCompare = TestCase (assertBool "" ([1,2,3]<[3,2,1]))

testCompare2 = TestCase (assertBool "" ([1,2,3]==[1,2,3]))

testGetHead = TestCase (assertEqual "" 4 (head [4..20]))

testGetTail = TestCase (assertEqual "" [5..20]  (tail [4..20]) )

testGetLast = TestCase (assertEqual "" 25 (last [1..25]))

testGetInit = TestCase (assertEqual "" [1..24] (init [1..25]))

testGetLength = TestCase (assertEqual "" 10 (length [1..10]))

testReverse = TestCase (assertEqual "" [10,9..1]  (reverse [1..10]))

testNull = TestCase (assertBool "" (null []))

testTake = TestCase (assertEqual "" "Dan" (take 3 "Daniel"))

testDrop = TestCase (assertEqual "" "Truck" (drop 8 "Monster Truck"))

testMaximum = TestCase (assertEqual "" 6 (maximum [1,3,5,6,2]))

testMinimum = TestCase (assertEqual "" 'C' (minimum "Chenchen"))

testSum = TestCase (assertEqual "" 10 (sum [1,2,3,4]))

testProduct = TestCase (assertEqual "" 24 (product [1,2,3,4]))

testCycle = TestCase (assertEqual "" [1,2,3,1] (take 4 (cycle [1,2,3])))

testRepeat = TestCase (assertEqual "" 5 (sum (take 5 (repeat 1))))

allTests = TestList [TestLabel "testAppend" testAppend, TestLabel "testConcat" testConcat, TestLabel "testGetElement" testGetElement, TestLabel "testGetElement2" testGetElement2, TestLabel "testCompare" testCompare, TestLabel "testCompare2" testCompare2, TestLabel "testGetHead" testGetHead, TestLabel "testGetTail" testGetTail, TestLabel "testGetLast" testGetLast, TestLabel "testGetInit" testGetInit, TestLabel "testGetLength" testGetLength, TestLabel "testReverse" testReverse, TestLabel "testNull" testNull, TestLabel "testTake" testTake, TestLabel "testDrop" testDrop, TestLabel "testMaximum" testMaximum, TestLabel "testMinimum" testMinimum, TestLabel "testSum" testSum, TestLabel "testProduct" testProduct, TestLabel "testCycle" testCycle, TestLabel "testRepeat" testRepeat]

