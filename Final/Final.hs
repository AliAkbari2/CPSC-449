{- 
    CPSC 449 Final Haskell Part
    Tutorial #3
    Fall 2020 
    Ali Akbari 30010402

    Refrences : 
    Previous Assignments.

-}


{- Question #1)
    Two binary relations passed in a list, uses type BinRel defined.
    Returns BinRel type back
-}
--BinRel type defined, a list of Integer Tuple
type BinRel = [(Integer, Integer)]
bin_rel_comp :: BinRel -> BinRel -> BinRel
bin_rel_comp relation1 relation2 = [(x,z)| (x,y)<-relation1, (q,z)<-relation2, y == q]

{-
    Question #2
    4-bit binary representation of bools and addition of two bitVecs passed
    in.

-}
--Bit vector is a list of bools
type BitVec = [Bool]
--Based on 2-bit(empty list case) and 3 bit carry truth tables
--Helper function to do the calculation of adding the bools
addbvHelperCarry :: Bool -> BitVec -> BitVec -> BitVec
addbvHelperCarry False [] [] = []
addbvHelperCarry True [] [] = [True]
--When first list is empty with or with out carry
addbvHelperCarry True [] (True:ys) = False : addbvHelperCarry True [] ys
addbvHelperCarry True [] (False:ys) = True : addbvHelperCarry False [] ys
addbvHelperCarry False [] (False:ys) = False : addbvHelperCarry False [] ys
addbvHelperCarry False [] (True:ys) = False : addbvHelperCarry True [] ys
--When second list is empty with or with out carry
addbvHelperCarry True (True:ys) [] = False : addbvHelperCarry True ys []
addbvHelperCarry True (False:ys) [] = True : addbvHelperCarry False ys []
addbvHelperCarry False  (False:ys) [] = False : addbvHelperCarry False ys []
addbvHelperCarry False  (True:ys) [] = False : addbvHelperCarry True ys []
--When carry is true 
addbvHelperCarry True (True:xs) (True:ys) = True : addbvHelperCarry True xs ys
addbvHelperCarry True (True:xs) (False:ys) = False : addbvHelperCarry False xs ys
addbvHelperCarry True (False:xs) (True:ys) = False : addbvHelperCarry False xs ys
addbvHelperCarry True (False:xs) (False:ys) = True : addbvHelperCarry False xs ys
--When carry is False
addbvHelperCarry False (False:xs) (False:ys) = False : addbvHelperCarry False xs ys
addbvHelperCarry False (False:xs) (True:ys) = True : addbvHelperCarry False xs ys
addbvHelperCarry False (True:xs) (False:ys) = True : addbvHelperCarry False xs ys
addbvHelperCarry False (True:xs) (True:ys) = False : addbvHelperCarry True xs ys

--Takes in two bit vectors and returns a bitvector
--Uses helper function to do the calculations
addbv :: BitVec -> BitVec -> BitVec
addbv [] [] = []
addbv (x:xs)[] = (x:xs)
addbv [](y:ys) = (y:ys)
addbv (x:xs)(y:ys) = addbvHelperCarry False (x:xs) (y:ys)


{-
    Given sample code 
-}
type VarName = Char

data Expr = Lit Integer
          | Var VarName
          | Add Expr Expr deriving Show

type Binding = (VarName, Expr)
type Substitution = [Binding]

ex :: Expr
ex = Add (Add (Add (Var 'x') (Lit 3)) (Var 'y')) (Var 'z')

sub :: Substitution
sub = [('x', Lit 7), ('z', Lit 0)]


showExpr :: Expr -> String
showExpr (Lit n) = "(Lit " ++ (show n) ++ ")"
showExpr (Var name) = "(Var " ++ (show name) ++ ")"
showExpr (Add e1 e2) = "(Add " ++ (showExpr e1) ++ " " ++ (showExpr e2) ++ ")"

showSub :: Substitution -> String
showSub [] = "[]"
showSub ((ch, e):ss) =
    "[(" ++ (show ch) ++ ", " ++ (showExpr e) ++ ")" ++
    (concat [", (" ++ (show ch2) ++ ", " ++ (showExpr e2) ++ ")" |
             (ch2, e2) <- ss]) ++
    "]"
{-
    Question #4
-}
--Helper function that uses the substitute on the passed expression returns the updates expression
substitutionhelper :: Substitution -> Expr -> Expr
substitutionhelper par1 (Var v) 
    | length([z | (x, y) <- par1, z <- [snd (x,y)], v == fst(x,y)]) == 0 = Var v
    | otherwise = changedExpr 
    where
        changedExpr = head([z | (x, y) <- par1, z <- [snd (x,y)], v == fst(x,y)])
--Substitue function that calls the helper function
substitute :: Substitution -> Expr -> Expr 
substitute [] express = express
substitute par1 (Lit x) = Lit x
substitute par1 (Add exp1 exp2) = Add (substitute par1 exp1) (substitute par1 exp2)
substitute par1  v = substitutionhelper par1 v



-- Question #6
--A) The sum of the evens squared from a list, returns sum.
-- Map squares all values of the evens after filter only chooses the evens of the list
-- foldr uses (+) to add every element
sum_sq_even :: [Integer] -> Integer
sum_sq_even xs = foldr(+) 0 (map(^2) (filter even xs))

--B) bubble_up takes in a list of integer and returns a list of Integer
-- Uses foldr1 to get the last element of the list which is then appended to the front
bubble_up :: [Integer] -> [Integer]
bubble_up [] = []
bubble_up aList = foldr1 (\_ r -> r) aList: init aList

--C)Takes in a function, figure out if a given an integer is less than the function output
boost_all :: [Integer -> Integer] -> (Integer -> Integer)
boost_all aList = foldr1 (.) (filter (\x -> x(1000) > 1000) aList)

--Question #7
-- eleganthelper function takes a list of list bools 
eleganthelper :: [[Bool]] -> [[Bool]]
eleganthelper(head: tail) = (True:head ++ [True]):(False:head ++ [False]):(eleganthelper tail)
eleganthelper [] = []
--Makes infinite list of odd pelindromes list of bool
elegant :: [[Bool]]
elegant = [True]:[False]:(eleganthelper elegant)