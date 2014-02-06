module Interpreter (
    fractran
	) where


import Parser




fractran :: FractranProgram -> [Int]
fractran program =
    let prog = map (\(x,y) -> (fromIntegral x, fromIntegral y)) (fractions program)
        f = (\p v -> if (p == [])
                     then []
                     else let (curX, curY) = head p
                              newV = v * curX / curY
                          in if (isInt newV)
                             then newV : (f prog newV)
                             else f (tail p) v)
        result = map round (f prog (fromIntegral (initialValue program)))
    in value : result

