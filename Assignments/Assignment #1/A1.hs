{-  CPSC 449 Assignment #1 
    Tutorial #3
    Fall 2020 
    Ali Akbari 30010402

    Refrences : 
    - Tutorial slides, 5 & 6
    - Lecture video 5 for Question 2 borrowers and borrowed
    - For Question #1
        - https://en.wikibooks.org/wiki/Haskell/Solutions/Recursion
    - Stack overflow for general syntax
    - Haskell Textbook

    Import of Packages, i.e predefined functions in prelude
-}

import System.IO

{-  Question #1
    Takes two integers for base of the log and the number (x) the log is computed on
    Returns the integer of exponent value/ the calculated value of logb(x)
    Assumes both parameters are postive where base > 2
-}
myLog :: Integer -> Integer -> Integer
myLog  base x
    |  base > x = 0
    |  otherwise = 1 + myLog base (div x base)

{-  
    Helper Function for Question 2:
    Concatenator takes a list of list and returns a list of just the elements
    i.e is the predefine concat function in haskell prelude
-}
concatenator :: [[Book]] -> [Book]
concatenator listOflists = [aBook | listOfBooks <- listOflists, aBook <- listOfBooks]


{-  Question #2
    Classifying the types
-}
type Book = String
type Person = String

--type of the 2-D list dataBase containing tuples is defined
type Database = [(Person, [Book])]

--Example library of the database for testing purposes
library_db :: Database
library_db = [ ("John", ["umm", "d"]), ("Harry", ["Harry", "Potter", "Part2"]), ("Cldye",  ["n", "c"]), ("Rick", ["PercyJa"]), ("al", ["yo"])]

{-  
    books function takes in a database and a person's name and returns the
    list of books loaned by them, uses list comprehension
-}
books :: Database -> Person -> [Book]
books databaseBook findPerson = concatenator [listOfBooks | (currentPerson, listOfBooks) <- databaseBook, currentPerson == findPerson]

{-  
    borrowers function takes in the database and the book name and lists out the people
    who have currently loaned the same book
-}
borrowers :: Database -> Book -> [Person]
borrowers databaseBook findBook = [people | (people, listOfBooks) <- databaseBook, book <- listOfBooks, elem findBook [book] == True ]

{-  
    borrowed function takes in a database and book name and returns True or
    False if the book is borrowed or not
-}
borrowed :: Database -> Book -> Bool
borrowed databaseBook findBook = (borrowers databaseBook findBook) /=[]

{-  
    numBorrowed takes in a database and a person's name
    returns the number of books loan by the person in int
-}
numBorrowed :: Database -> Person -> Int
numBorrowed databaseBook findPerson  = length (books databaseBook findPerson)

{-  
    makeLoan takes in a database and a person's name and a book
    name then adds it to the database with list comprehension
-}
makeLoan :: Database -> Person -> Book -> Database
makeLoan databaseBook findPerson findBook
    {-  Cases are made so no duplicates are not made in database and that lists are updated
        when new books are added
    -}
    |  elem findBook (books databaseBook findPerson) == True = databaseBook 
    |  otherwise = listWithout ++ listOfUpdatedBooks
        where 
        {-  
            Two list are made, one takes out the person and their list of books (listWithout) and the other one
            makes new list of the person with the new book added, then above the two lists are added
        -}
        listWithout = [(currentPerson, aBook) | (currentPerson, aBook) <- databaseBook, findPerson /= currentPerson] 
        listOfUpdatedBooks =[(findPerson,  books databaseBook findPerson ++ [findBook])]

{-  returnLoan takes in a database and a person's name with a certain book name and removes the book of the list of books connected to the person.
    If the book doesnt exist in the database return same database. If the person only has one book thats being remove, remove person from database.
-}
returnLoan :: Database -> Person -> Book -> Database
returnLoan databaseBook findPerson findBook 
    -- Case one if book does not exist in database
    |  elem findBook (books databaseBook findPerson) == False = databaseBook
    -- Case two if person only has one book
    |  numBorrowed databaseBook findPerson < 2 = [(person, abook) | (person, abook) <- databaseBook, person /= findPerson]
    -- Otherwise case, just remove book from persons book list
    |  otherwise = listWithout ++ listOfUpdatedBooks
        where
    -- Makes two lists one without person (listWithout) and one
        listWithout = [(person, abook) | (person, abook) <- databaseBook, person /= findPerson] 
        listOfUpdatedBooks = [(findPerson, [book | book <- (books databaseBook findPerson), book /= findBook])]


--  Helper Function for Question 3
{-  Copyunit takes a list and copies the elements the number of times the parameter for Int is sent
    i.e is the replicate function in haskell prelude
-}
copyUnitMultipleTimes :: [x] -> Int -> [x]
copyUnitMultipleTimes aList numberOfCopies = [unit | unit  <- aList, numberOfCopies <- [1 .. numberOfCopies]]

{-  Question #3

    type of picture is define as list with list of chars
-}
type Picture = [[Char]]

{-  scale takes in a picture type and an int for scale values
    and returns a picture
-}
scale :: Picture -> Int -> Picture
scale pictureList scaleValue
    -- If the int, scalevalue is less than 1 scale value is empty list of empty list
    |  scaleValue < 1 = [[]]
    -- Otherwise use the helper function, and copy the units, the unit in the list are sent
    -- to the helper function and so is the whole list
    |  otherwise = copyUnitMultipleTimes [copyUnitMultipleTimes innerList scaleValue | innerList <- pictureList] scaleValue


{-  Question #4
    type of Graph made with a list of int tuples
-}
type Graph = [(Int, Int)]

{-  An example graph used for testing purposes
-}
exampleGraph :: Graph
exampleGraph = [(1, 3), (4, 2), (4, 1), (2, 3)]

{-  ListCommonFriends takes in the graph and makes a list of the friends
    based of the friend integer passed in
-}
listCommonFriends :: Graph -> Int -> [Int]
listCommonFriends friendGraph friend = firstInList ++ secondInList
    where
        firstInList = [z | (x,y) <- friendGraph,  z <- [fst (x,y)], friend == snd(x,y)]
        secondInList = [z | (x,y) <- friendGraph, z <- [snd (x,y)], friend == fst(x,y)]

{-  commonFriends takes in the Graph and two friends represented as int
    and returns a list of common friends, i.e list of same adjacent tuple
    values 
-}
commonFriends :: Graph -> Int -> Int -> [Int]
--Using list comprehension to make list of the returned listcommonfriends for both friendone 
--and friend two, including element that match from both using elem
commonFriends friendGraph friendOne friendTwo = [friend | friend <- (listCommonFriends friendGraph friendOne), elem friend (listCommonFriends friendGraph friendTwo) == True]

{-  Main function used mainly for testing purposes for Question 1
-}
main :: IO ()
main = do
    putStrLn "The log is"
    print (myLog 10 100)
    