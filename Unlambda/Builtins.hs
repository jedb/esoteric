module Unlambda.Builtins (
	k,
	s,
	i,
	dot,
	r,
	d,
	c,
	e
	) where


import Control.Exception( Exception(..), throw )



data MyException = MyException { func :: a -> b }
    deriving (Show, Eq)

instance Exception MyException



k :: a -> b -> a
k x y = x



s :: (a -> b -> c) -> (a -> b) -> a -> c
s x y z = (x z) (y z)



i :: a -> a
i = id



class Void a where
	v :: a -> r

instance Void v => Void (a -> r) where
	v x = v



dot :: Char -> a -> IO a
dot ch f = putChar ch >> return f



r :: a -> IO a
r f = putChar '\n' >> return f



-- may not work as per unlambda lazy semantics
d :: (a -> b) -> (a -> b)
d x = (\y -> x y)



c :: (a -> b) -> (a -> b)
c x = (`runCont` id) (callCC $ \cont -> x cont)



e :: a -> b
e x = throw (MyException x)

