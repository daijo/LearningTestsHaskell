-- FunctionSyntaxTests.hs 
-- Test cases exploring function syntax. 
-- © 2011 Daniel Hjort

import HUnit

-- Functions

matchingFunc :: Int -> String
matchingFunc 1 = "one"
matchingFunc 2 = "two"
matchingFunc x = "unknown value"

factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n - 1)

-- Test cases

testBasicMatching = TestCase (do assertEqual "" "one" (matchingFunc 1)
                                 assertEqual "" "two" (matchingFunc 2)
                                 assertEqual "" "unknown value" (matchingFunc 10))

testFactorial = TestCase (do assertEqual "" 1 (factorial 0)
                             assertEqual "" 120 (factorial 5)
                             assertEqual "" 720 (factorial 6))

allTests = TestList [TestLabel "testBasicMatching" testBasicMatching, TestLabel "testFactorial" testFactorial]
