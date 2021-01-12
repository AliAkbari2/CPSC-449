concat' :: [[element]] -> [element]
concat' [] = []
concat' (headEl:restEls) = headEl ++ concat' restEls

reverse' :: [element] -> [element]
reverse' [] = []
reverse' (headEl:restEls) = reverse' restEls ++ [headEl]

