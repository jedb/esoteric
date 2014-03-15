module Unlambda.Builtins (
	k,
	s,
	i,
	dot,
	r,
	d
	) where




k :: a -> b -> a
k x y = x



s :: (a -> b -> c) -> (a -> b) -> a -> c
s x y z = (x z) (y z)



i :: a -> a
i = id



dot :: Char -> a -> IO a
dot ch f = putChar ch >> return f



r :: a -> IO a
r f = putChar '\n' >> return f



-- may not work as per unlambda lazy semantics
d :: (a -> b) -> (a -> b)
d x = (\y -> x y)

