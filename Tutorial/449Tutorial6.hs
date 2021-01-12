module Week6 where

    type Color = String
    type Brand = String
    type Database = [(Brand, [Color])]

    car_db :: Database
    car_db = [("Ferrari", ["Red", "Yellow"]), ("BMW", ["Red", "Black", "Blue"]), ("Lambo", ["Yellow", "Black"]), ("VW", ["Red"])]

    which_brand :: Database -> Color -> [Brand]
    which_brand db findColor = [brand | (brand, colors) <- db, elem findColor colors]

    get_brand :: Database -> Brand -> Database
    get_brand db findBrand = [(brand, colors) | (brand, colors) <- db, brand == findBrand]

    remove_brand :: Database -> Brand -> Database
    remove_brand db findBrand = [info | info <- db, [info] /= (get_brand db findBrand)]

    update_brand :: Database -> Brand -> [Color] -> Database
    update_brand db findBrand colors
        |(get_brand db findBrand) == [] = db ++ [(findBrand, colors)]
        |otherwise = update_brand (remove_brand db findBrand) findBrand colors

    continue :: [a] -> [a]
    continue [] = []
    continue (x:xs) = x:(continue xs)
    -- or continue (x:xs) = [x] ++ (continue xs)
    -- x:y:z:t:[] == [x,y,z,t]

    my_length :: [a] -> Integer
    my_length [] = 0
    my_length (x:xs) = 1 + my_length xs

    my_product :: [Integer] -> Integer
    my_product [] = 1
    my_product (x:xs) = x * my_product xs

    my_reverse :: [a] -> [a]
    my_reverse [] = []
    my_reverse (x:xs) = my_reverse xs ++ [x]
