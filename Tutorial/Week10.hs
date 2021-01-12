nTimes :: Integer -> Parse a b -> Parse a [b]
nTimes 0 _ = succeed []
nTime n p = build (sqn p (nTimes (n-1) p)) (uncurry (:))