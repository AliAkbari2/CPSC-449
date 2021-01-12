data BinTree = BinEmpty | BinNode Int BinTree BinTree

foldBinTree :: (Int -> a -> a -> a) -> a -> BinTree -> a
foldBinTree function cumulator BinEmpty = cumulator
foldBinTree function cumulator (BinNode value left right) = function value (foldBinTree function cumulator left)
                                                                           (foldBinTree function cumulator right)
