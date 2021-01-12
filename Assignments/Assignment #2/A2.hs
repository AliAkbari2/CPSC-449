{- 
    CPSC 449 Assignment #2 
    Tutorial #3
    Fall 2020 
    Ali Akbari 30010402

    Refrences : 

    Haskell Webpages: https://wiki.haskell.org/Add_polynomials
        For addpoly
    Stack OverFlow: https://stackoverflow.com/questions/3938438/merging-two-lists-in-haskell
        For Merge
    Text Book : Thompson S. Haskell The Craft of Functional Programmming
        page: 370 for MergeSort
    
    Comments use the followin format:
    (a) the purpose of the function
    (b) the role of each formal parameter
    (c) any preconditions or assumptions that the function relies on
    (d) the return value of the function.
-}

--Polynomial is defined as a type of Integer list containing the coefficients 
type Poly = [Integer]
{-
    (a) The purpose of the function is to add two polynomial types, to add the coefficients
    (b) Takes in two polynomial lists paramenters for the addition to be computed on.
    (c) Assumes that polynomials are type of integer lists
    (d) The return value of the function is the resulting polynomial after the addition
-}
addpoly :: Poly -> Poly -> Poly
addpoly ab [] = ab
addpoly [] cd = cd
addpoly (x:xs) (y:ys)  = head : restOfList
    where
        head = (x + y) 
        restOfList = (addpoly xs ys)

{-
    (a) The purpose of this helper function is to multiply a polynomial by a value using prelude map
    (b) Takes in two paramenters an integer(multiplier) and a polynomial for the multiplication to be computed on.
    (c) Assumes that polynomials are type of integer lists and multiplier is integer type
    (d) The return value of the function is the resulting polynomial after the map of multiplication
-}
multiply :: Integer -> Poly -> Poly
multiply multiplier = map (*multiplier)

{-
    (a) The purpose of this function is to multiply a polynomial by another polynomial using the above helper function and addpoly
    (b) Takes in two paramenters, two polynomials for the multiplication to be computed on.
    (c) Assumes that polynomials are type of integer lists 
    (d) The return value of the function is the resulting polynomial after the multiplication
-}
multpoly :: Poly -> Poly -> Poly
multpoly _ [] = []
multpoly [] _ = []
-- Multiplies each coefficients from one list to the other list then add the values in a new list 
-- Similar to how multiplication is done by hand
multpoly (x:xs) ys = addpoly (multiply x ys) ([0] ++ (multpoly xs ys))

{-
    (a) The purpose of this helper function is to merge two lists of integers into one list of integers
    (b) Takes in two paramenters, two integer list for  the merge to be computed on.
    (c) Assumes that lists are integer lists 
    (d) The return value of the function is the resulting integer list after the merge
-}
mergeLists :: [Integer] -> [Integer] -> [Integer]
mergeLists intList [] = intList
mergeLists [] intListb = intListb
mergeLists (headFirst : restFirst) (headSecond : restSecond) 
    | headFirst <= headSecond = headFirst : mergeLists restFirst (headSecond : restSecond)
    | otherwise = headSecond : mergeLists (headFirst : restFirst) restSecond

{-
    (a) The purpose of this helper function is to split an integer list into a tuple of two integer list, 
        first of the tuple list contains the even indices elements of the passed in list,
        second of the tuple list contains the odd indices elements of the passed in list.
    (b) Takes in one paramenters an integer list split function is computed on that
    (c) Assumes that the list is an integer lists 
    (d) The return value of the function is the resulting tuple list after splitting.
-}
splitList :: [Integer] -> ([Integer] , [Integer])
splitList [] = ([],[])
splitList [head] = ([head],[])
splitList (head:second:tailofRest) = (head:tailfirst, second:tailsecond)
    where
        (tailfirst, tailsecond) = splitList tailofRest

{-
    (a) The purpose of this function is sort a given integer list in accending order using the above split and merge functions
    (b) Takes in one paramenter an integer list for the merge sort to be computed on.
    (c) Assumes that parameter list is of integer type.
    (d) The return value of the function is the resulting sorted integer list
-}
mSort :: [Integer] -> [Integer] 
mSort [] = []
mSort [head]= [head]
--Recusive call of each half of the list
--Mergelists is called and each half is sent to to be joint merged back together
mSort wholeList = mergeLists (mSort firstHalf) (mSort secondHalf)
    where
--Using split to make two lists
        (firstHalf, secondHalf) = splitList wholeList


{-  Main function used mainly for testing purposes for most questions
-}
main :: IO ()
main = do
    {-
    Printing the values for the result of adding two polynomials 
    -}
    putStrLn "The added poly is "
    print (addpoly [] [])
    print (addpoly [1,2] [3,4])
    print (addpoly [1,1,1,1] [-1,1])
    print (addpoly [] [1, 3])
    {-
    Printing the values for the result of multiplying two polynomials
    -}
    putStrLn "The mult poly is "
    print (multpoly [1, 1] [1, 1])
    print (multpoly [1, 1, 1, 1] [-1, 1])
    print (multpoly [2,1] [1,3])
    print (multpoly [1,1] (multpoly [1,1] [1,1]))
    print (multpoly [1,2,1] [])
    {-
    Printing the values for the result of merginging two polynomials/list
    -}
    putStrLn "The merge poly is "
    print (mergeLists [1, 3, 5] [2, 4, 6])
    {-
    Printing the values for the result of spliting one polynomial/list
    -}
    putStrLn "The split poly is "
    print (splitList [4, 6, 3, 1, 2, 10])
    print (splitList [7, 1, 4])
    print (splitList [4])
    print (splitList [])
    {-
    Printing the values for the result of mergesorting one polynomial/list
    -}
    putStrLn "The m sorted poly is "
    print (mSort [3, 4, 1, 3, 6])
    print (mSort [])
    print (mSort [1,2,3,4])
    print (mSort [-5, 5, 4])
    print (mSort [1, 0, -1 , 3, 2])

