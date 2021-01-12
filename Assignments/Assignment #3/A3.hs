{- 
    CPSC 449 Assignment #3
    Tutorial #3
    Fall 2020 
    Ali Akbari 30010402

    Refrences : 

    Haskell Webpages: 
        
    Stack OverFlow: 
        
    Text Book : Thompson S. Haskell The Craft of Functional Programmming
        page: 15 for double
        page: 222 for foldr1
    
    Comments use the followin format:
    (a) the purpose of the function
    (b) the role of each formal parameter
    (c) any preconditions or assumptions that the function relies on
    (d) the return value of the function.
-}


{- Question #1)a.

    (a) The purpose of the data type is to define an alegrabic formula of wff
    (b) Data type Formula has a String variable, Negate (negation) attribute, AND/OR attribute
    (c) Assumes that formula is a variables that is string type and NegateAND/OR take Formulas
    (d) No return, data type
-}
data Formula = Var String       
            | Negate Formula      
            | And Formula Formula 
            | Or  Formula Formula  

{- Question #1)b.

    (a) The purpose of the function is to define a wff ~(~p AND ~q) an instance of Formula
    (b) Takes in no parameter
    (c) Assumes Formula is defined
    (d) No return type
-}
expression :: Formula
expression = Negate(And (Negate( Var "p")) (Negate( Var "q")))

--Temporary Formula for testing
temp :: Formula
temp = Negate(Or (Var "p") (Var "q"))

{- Question #1)c.

    (a) The purpose of the function is to show a Formula in String format
    (b) Takes in a Formula type 
    (c) Assumes Formula is defined 
    (d) The return value of the function is of String type, representing the formula in a string
-}
showFormula :: Formula -> String
showFormula (Var v) = v
showFormula (Negate p) = "~" ++ showFormula p
showFormula (And p q) = "(" ++ showFormula p ++ " & " ++ showFormula q ++ ")"
showFormula (Or p q) = "(" ++ showFormula p ++ " | " ++ showFormula q ++ ")"

{- Question #1)d.

    (a) The purpose of the function is to rewrite a Formula, while using DeMorgan's Law (NNF)
    (b) Takes in a Formula 
    (c) Assumes Formula is define 
    (d) The return value of the function is a Formula where Demorgan's Law is applied to the applicable Formula
-}
rewrite :: Formula -> Formula
rewrite (Var p) = Var p
rewrite (Negate (Negate p)) = (rewrite p)
rewrite (And p q) = And (rewrite p) (rewrite q)
rewrite (Or p q) = Or (rewrite p) (rewrite q)
rewrite (Negate (And p q)) = Or (rewrite(Negate(p))) (rewrite (Negate(q)))
rewrite (Negate (Or p q)) = And (rewrite(Negate(p))) (rewrite(Negate(q)))
rewrite (Negate p) = Negate (rewrite(p))

-- Q2 in the A3.pdf file

{- Question #3)a.

    (a) The purpose of the function is to use foldr1 to get last element in a list
    (b) Takes in a list with general types
    (c) Assumes list order and sorting does not matter
    (d) The return value of the function is last element of the list
-}
lastElm :: [a] -> a
lastElm = foldr1(\_ a -> a)

{- Question #3)b.

    (a) The purpose of the function is to apply a list of boolean predicates on a general entity 
    (b) Takes in a predicate list and a entity of general type
    (c) Assumes that predicate list has a boolean returned after filtered
    (d) The return value of the function is a boolean
-}
unanimous :: [a -> Bool] -> a -> Bool
unanimous [] a = True;
unanimous (x:xs) entity = aBool
    where
        aBool = length (filter x [entity]) == 1 &&  unanimous xs entity


{- Question #3)c.

    (a) The purpose of the function is to test every element and changes the elements that satisfy the predicate
    (b) Takes in a predicate, a transforming function, a list of general types 
    (c) Assumes predicate can be applied on list to check
    (d) The return value of the function is a changed list of general type b
-}
selectiveMap :: (a -> Bool) -> (a -> b) -> [a] -> [b]
selectiveMap pred trans aList = map trans bList
    where
        bList = filter pred aList

{- Question #4)a.

    (a) The purpose of the function is to iterate a transformative function on general type t
    (b) Takes in a general type transformative function and general type entity
    (c) Assumes transformative function can be applied on type t
    (d) The return value of the function is the resulting type t after transformative function is applied
-}
iter :: Integer -> (t -> t) -> t -> t
iter 0 function input = input
iter nTimes function input = iter (nTimes - 1) function ( function (input))

--Helper function double, doubles an input Integer and returns a Integer
double :: Integer -> Integer
double value = 2 * value

{- Question #4)b.

    (a) The purpose of the function is to use iter with double function to find two to a power
    (b) Takes in an Integer as the exponent to be applied to 2
    (c) Assumes Integer type is passed in
    (d) The return value of the function is the Integer value after double is used for iter
-}
powerOfTwo :: Integer -> Integer
powerOfTwo n = iter n double 1