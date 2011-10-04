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

scaleVector :: (Double, Double) -> Double -> (Double, Double)
scaleVector (x, y) s = (s * x, s * y)

secondInGenericTriple :: (a, b, c) -> b
secondInGenericTriple (_, x, _) = x

-- Test cases

testBasicMatching = TestCase (do assertEqual "" "one" (matchingFunc 1)
                                 assertEqual "" "two" (matchingFunc 2)
                                 assertEqual "" "unknown value" (matchingFunc 10))

testFactorial = TestCase (do assertEqual "" 1 (factorial 0)
                             assertEqual "" 120 (factorial 5)
                             assertEqual "" 720 (factorial 6))

testScaleVector = TestCase (assertEqual "" (1.5, 1.5) (scaleVector (1, 1) 1.5))

testGenericTriple = TestCase (do assertEqual "" 2 (secondInGenericTriple (1, 2, 3))
                                 assertEqual "" "er" (secondInGenericTriple ('i', "er", 3)))

allTests = TestList [TestLabel "testBasicMatching" testBasicMatching, TestLabel "testFactorial" testFactorial, TestLabel "testScaleVector" testScaleVector, TestLabel "testGenericTriple" testGenericTriple]
