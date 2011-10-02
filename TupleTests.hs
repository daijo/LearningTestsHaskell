-- TupleTests.hs 
-- Test cases exploring tuples. 
-- © 2011 Daniel Hjort

import HUnit

testGetFirst = TestCase (assertEqual "" 4 (fst (4,2)))

testGetSecond = TestCase (assertEqual "" 2 (snd (4,2)))

testZipListsToPairs = TestCase (assertEqual "" [(1, "Ice cream"), (2, "Chocolatine")] (zip [1..] ["Ice cream", "Chocolatine"]))

testGetRightRectangleTriples = TestCase (assertEqual "" [(8,6,10)] ([(a,b,c) | c <- [1..10], a <- [1..c], b <- [1..a], a^2 + b^2 == c^2, a+b+c == 24]))

allTests = TestList [TestLabel "testGetFirst" testGetFirst, TestLabel "testGetSecond" testGetSecond, TestLabel "testZipListsToPairs" testZipListsToPairs, TestLabel "testGetRightRectangleTriples" testGetRightRectangleTriples]
