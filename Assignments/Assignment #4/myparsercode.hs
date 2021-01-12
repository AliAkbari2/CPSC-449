{- 
    CPSC 449 Assignment #4 Question #3
    Tutorial #3
    Fall 2020 
    Ali Akbari 30010402

    Refrences : 

    Haskell Webpages: 
        https://hackage.haskell.org/package/Craft3e-0.1.1.0/src/Calculator/CalcParse.hs
        
    Stack OverFlow: 
        
    Text Book : Thompson S. Haskell The Craft of Functional Programmming
        page: 434-435 for infixr 
        page: 438 for makeExpr
    
    Comments use the followin format:
    (a) the purpose of the function
    (b) the role of each formal parameter
    (c) any preconditions or assumptions that the function relies on
    (d) the return value of the function.
-}


{-
    (a) The purpose of the function is to add the number in a list in sequence
    (b) Takes in a integer list 
    (c) Assumes list is of integer type
    (d) The return value of the function is a integer list
-}

{-
    (a) Helper function. The purpose of this infix function parses a b to a (b,c).
    (b) Takes in parameters of polynomial type
    (c) Assumes poly is of type defined above
    (d) The return value of the function is a integer
-}
infixr 5 >*>
(>*>) :: Parse a b -> Parse a c -> Parse a (b,c)
(>*>) p1 p2 inp = [((y,z),rem2)|(y,rem1) <- p1 inp, (z,rem2) <- p2 rem1]

{-
    (a) The purpose of this function is to check char are of type operators
    (b) Takes in parameters of char element
    (c) Assumes all applicable operators are defined below.
    (d) The return value of the function is bool, true if operator else false
-}
isOp :: Char -> Bool
isOp x = elem x "+-*/%"

{-
    (a) The purpose of this function using case by case method to change the char operator to Ops data type
    (b) Takes in parameters of char element
    (c) Assumes all applicable operators are defined below.
    (d) The return value of the function is a ops element data type
-}
charToOp :: Char -> Ops
charToOp op =
     case op of
          '+' -> Add
          '-' -> Sub
          '*' -> Mul
          '/' -> Div
          '%' -> Mod
          otherwise -> error ("Error, not char of Op.")

{-
    (a) The purpose of this function is to recognizes objects optionally, input parser may recognizr it or succeed 
    (b) Takes in parameters of parse type a,b
    (d) The return value of the function parse type a and b lis
-}
optional :: Parse a b -> Parse a [b]
optional pVal = alt (succeed []) (build pVal (:[]))

{-
    (a) The purpose of this function is to recognize a non-empty list of objects, which the input parser recongizes
    (b) Takes in parameters of parse type a,b 
    (d) The return value of the function parse type a and b list
-}
neList :: Parse a b -> Parse a [b]
neList pVal = (pVal `build` (:[])) `alt` ((pVal >*> neList pVal) `build` (uncurry (:)))

{-
    (a) The purpose of this function is to  
    (b) Takes in parameters of char element
    (c) Assumes all applicable operators are defined below.
    (d) The return value of the function is a ops element data type
-}
makeExpr (_,(e1,(bop,(e2,_)))) = Op (charToOp bop) e1 e2

{-
    (a) The purpose of this function is to transforms a char list into expression  
    (b) Takes in a list as a parameter of char  element
    (c) Assumes char list can be transformed into expression
    (d) The return value of the function is an expression 
-}
stringToExpr :: [Char] -> Expr
stringToExpr ('~':charList) = Lit (negate (read charList::Int))
stringToExpr charList = Lit (read charList::Int)
