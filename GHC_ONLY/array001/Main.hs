main _ = [AppendChan stdout (shows a "\n")]
    where
	a :: Array Integer Integer
	a = array (1,100) ((1 := 1) : [i := i * a!(i-1) | i <- [2..100]])
