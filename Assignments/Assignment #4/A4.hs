{- 
    CPSC 449 Assignment #4 Question #1, #4
    Tutorial #3
    Fall 2020 
    Ali Akbari 30010402

    Refrences : 
    Haskell Webpages: 
    Stack OverFlow: 
    Text Book : 
    
    Comments use the followin format:
    (a) the purpose of the function
    (b) the role of each formal parameter
    (c) any preconditions or assumptions that the function relies on
    (d) the return value of the function.
-}

--Data type polynomial that
data Poly =    PConst Int |
               PVar |
               PAdd Poly Poly |
               PMul Poly Poly

{-
    (a) The purpose of this function is to compile a polynomial with premitive recursion, adds/multiplies the coeffecients
    (b) Takes in a data type of poly
    (c) Assumes poly is of type defined above
    (d) The return value of the function is a integer
-}
compilePoly :: Poly -> (Int -> Int)
compilePoly (PConst x) = (\_ -> x)
compilePoly (PVar) = id
compilePoly (PAdd poly1 poly2) = (\x -> (compilePoly poly1 x) + (compilePoly poly2 x))
compilePoly (PMul poly1 poly2) = (\x -> (compilePoly poly1 x) * (compilePoly poly2 x))


{-
    (a) The purpose of the function is to add the number in a list in sequence
    (b) Takes in a integer list 
    (c) Assumes list is of integer type
    (d) The return value of the function is a integer list
-}
runningSums :: [Int] -> [Int]
runningSums aList = result
            where
                result = 0 : zipWith (+) aList result