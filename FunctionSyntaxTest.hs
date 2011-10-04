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

matchHead :: [a] -> a
matchHead [] = error "Empty list!"
matchHead (x:_) = x

matchTail :: [a] -> [a]
matchTail [] = error "Empty list!"
matchTail (_:xs) = xs

secondLetterOf :: String -> String
secondLetterOf "" = "Empty String!"
secondLetterOf (x:[]) = "One letter String!"
secondLetterOf all@(x:y:xs) = "Second letter of " ++ all ++ " is " ++ [y]

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

testMatchHead = TestCase (assertEqual "" 1 (matchHead [1, 2, 3]))

testMatchTail = TestCase (assertEqual "" [2, 3] (matchTail [1, 2, 3]))

testSecondLetterOf = TestCase (do assertEqual "" "Empty String!" (secondLetterOf "")
                                  assertEqual "" "One letter String!" (secondLetterOf "D")
                                  assertEqual "" "Second letter of Daniel is a" (secondLetterOf "Daniel"))


allTests = TestList [TestLabel "testBasicMatching" testBasicMatching, TestLabel "testFactorial" testFactorial, TestLabel "testScaleVector" testScaleVector, TestLabel "testGenericTriple" testGenericTriple, TestLabel "testMatchHead" testMatchHead, TestLabel "testMatchTail" testMatchTail, TestLabel "testSecondLetterOf" testSecondLetterOf]
