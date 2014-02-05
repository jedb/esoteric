


isInt :: (RealFrac a) => a -> Bool
isInt x =
    x == fromInteger (round x)



modulo :: Int -> Int -> Int
modulo x y =
    x - (x `div` y) * y



primeFactors :: Int -> [Int]
primeFactors x =
    let p = (\x e c -> if (x == 1)
                       then (reverse c)
                       else if (x `modulo` (head e) == 0)
                            then p (x `div` (head e)) e ((head e) : c)
                            else p x (tail e) c)
    in p x euler []



euler :: [Int]
euler =
    let f = (\list -> (head list) : (f (filter (\x -> x `modulo` (head list) /= 0) list)))
    in f [2..]



isPowerOf :: Int -> Int -> Bool
isPowerOf x y =
    case (compare x y) of
        LT -> False
        EQ -> True
        GT -> if (x `modulo` y == 0) then isPowerOf (x `div` y) y else False



fractran :: [(Int,Int)] -> Int -> [Int]
fractran program value =
    let prog = map (\(x,y) -> (fromIntegral x, fromIntegral y)) program
        f = (\p v -> if (p == [])
                     then []
                     else let (curX, curY) = head p
                              newV = v * curX / curY
                          in if (isInt newV)
                             then newV : (f prog newV)
                             else f (tail p) v)
        result = map round (f prog (fromIntegral value))
    in value : result



