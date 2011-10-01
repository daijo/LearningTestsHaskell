-- ListComprehensionTests.hs 
-- Test cases exploring list comprehensions.
-- © 2011 Daniel Hjort

import HUnit

testFirst5EvenNumbers = TestCase (assertEqual "" [2,4,6,8,10] [2*x | x <- [1..5]])

testAllCombinations = TestCase (assertEqual "" [1,2,2,4,3,6] [x*y | x <- [1..3], y <- [1..2]])

testWithPredicate = TestCase (assertEqual "" "aniel" [c | c <- "Daniel", c `elem` ['a'..'z']])

listTests = TestList [TestLabel "testFirst5EvenNumbers" testFirst5EvenNumbers, TestLabel "testAllCombinations" testAllCombinations, TestLabel "testWithPredicate" testWithPredicate]
