--!!! simple test of 0-method classes
--

class (Num a, Integral a) => Foo a

main _ = [AppendChan stdout (shows (f ((fromInteger 21)::Foo a => a)
				      ((fromInteger 37)::Foo a => a)) "\n")]

f :: Foo a => a -> a -> Integer

f a b = toInteger (a + b)
