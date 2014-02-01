


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



-- some simple fractran programs

-- input: 2^a * 3^b
-- output: 3^(a+b)
addition :: [(Int,Int)]
addition = [(3,2)]

-- input: 2^a * 3^b
-- output: 5^ab
multiply :: [(Int,Int)]
multiply = [(13,21), (385,13), (1,7), (3,11), (7,2), (1,3)]

-- input: 2
-- output: a sequence containing all prime powers of 2
prime2 :: [(Int,Int)]
prime2 = [(17,91), (78,85), (19,51), (23,38), (29,33), (77,29), (95,23), (77,19), (1,17), (11,13), (13,11), (15,14), (15,2), (55,1)]

-- input: 10
-- output: a sequence containing all prime powers of 10
prime10short :: [(Int,Int)]
prime10short = [(3,11), (847,45), (143,6), (7,3), (10,91), (3,7), (36,325), (1,2), (36,5)]

prime10 :: [(Int,Int)]
prime10 = [(7,3), (99,98), (13,49), (39,35), (36,91), (10,143), (49,13), (7,11), (1,2), (91,1)]



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



