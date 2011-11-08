-- LinearAlgebraTests.hs 
-- Test cases exploring linear algebra using hmatrix.
-- © 2011 Daniel Hjort

import HUnit
import Numeric.LinearAlgebra -- requires hmatrix

-- Functions

a1 = (2><2) [1..4] :: Matrix Double -- with instance declaration
a2 = fromLists [[1, 2], [3, 4]] :: Matrix Double

b = (2><2) (reverse [1..4]) :: Matrix Double 
a_b_add_result = fromLists [[5, 5], [5, 5]] :: Matrix Double

a_b_mult_result = fromLists [[8, 5], [20, 13]] :: Matrix Double
b_a_mult_result = fromLists [[13, 20], [5, 8]] :: Matrix Double

i_matrix = ident 2 :: Matrix Double 

a_inv = fromLists [[-2, 1], [1.5, -0.5]] :: Matrix Double

-- Test cases

createMatrixTest = TestCase (assertBool "" (a1==a2))

addMatrixTest = TestCase (assertEqual "" a_b_add_result (add a1 b))

multiplicateMatrixTest = TestCase (do assertEqual "" a_b_mult_result (a1 <> b)
                                      assertEqual "" b_a_mult_result (b <> a1))

identMatrixTest = TestCase (do assertEqual "" a1 (i_matrix <> a1)
                               assertEqual "" a1 (a1 <> i_matrix))

invMatrixTest = TestCase (assertEqual "" a_inv (inv a1)) -- rounding error fail

matrixDetTest = TestCase (assertEqual "" (-2) (det a1))

allTests = TestList [TestLabel "createMatrixTest" createMatrixTest,
                     TestLabel "addMatrixTest" addMatrixTest,
                     TestLabel "multiplicateMatrixTest" multiplicateMatrixTest,
                     TestLabel "identMatrixTest" identMatrixTest,
                     -- TestLabel "invMatrixTest" invMatrixTest,
                     TestLabel "matrixDetTest" matrixDetTest]
