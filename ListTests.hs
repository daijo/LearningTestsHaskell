-- ListTests.hs 
-- Test cases exploring lists.
-- © 2011 Daniel Hjort

import HUnit

testAppend = TestCase (assertEqual "" ('A':"BC") "ABC")

testConcat = TestCase (assertEqual "" ([1,2] ++ [3,4]) [1..4])

testGetElement = TestCase (assertEqual "" ("Blue Pumpkin" !! 5) 'P')

testGetElement2 = TestCase (assertEqual "" ([1..] !! 5) 6)

testCompare = TestCase (assertBool "" ([1,2,3]<[3,2,1]))

testCompare2 = TestCase (assertBool "" ([1,2,3]==[1,2,3]))

testGetHead = TestCase (assertEqual "" (head [4..20]) 4)

testGetTail = TestCase (assertEqual "" (tail [4..20]) [5..20])

testGetLast = TestCase (assertEqual "" (last [1..25]) 25)

testGetInit = TestCase (assertEqual "" (init [1..25]) [1..24])

testGetLength = TestCase (assertEqual "" (length [1..10]) 10)

testReverse = TestCase (assertEqual "" (reverse [1..10]) [10,9..1])

testNull = TestCase (assertBool "" (null []))

listTests = TestList [TestLabel "testAppend" testAppend, TestLabel "testConcat" testConcat, TestLabel "testGetElement" testGetElement, TestLabel "testGetElement2" testGetElement2, TestLabel "testCompare" testCompare, TestLabel "testCompare2" testCompare2, TestLabel "testGetHead" testGetHead, TestLabel "testGetTail" testGetTail, TestLabel "testGetLast" testGetLast, TestLabel "testGetInit" testGetInit, TestLabel "testGetLength" testGetLength, TestLabel "testReverse" testReverse, TestLabel "testNull" testNull]

