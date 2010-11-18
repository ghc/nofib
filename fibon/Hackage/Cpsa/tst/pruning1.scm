(defprotocol prune basic
  (defrole init (vars (a b name) (n text))
    (trace 
	(send (enc n (pubk a)))
      (recv (enc n (pubk b) (pubk a)))
      (recv (enc n (privk b) (pubk a)))
	)
    (uniq-orig n)
    (non-orig (privk a))
  )
  (defrole trans (vars (a name) (n text) (k akey))
     (trace
        (recv (enc n (pubk a)))
	(recv k )
        (send (enc n k (pubk a)))
     )
  )
  (comment "Shows a failure with generalization"
	   "Run this with a step count of 4"))

(defskeleton prune
  (vars (a name) (n text) (k akey))
  (defstrand init 3))

(defskeleton prune
  (vars (n text) (a b name))
  (defstrand init 3 (n n) (a a) (b b))
  (defstrand trans 3 (n n) (a a) (k (pubk b)))
  (defstrand trans 3 (n n) (a a) (k (privk b)))
  (precedes ((0 0) (1 0)) ((0 0) (2 0)) ((1 2) (0 1)) ((2 2) (0 2)))
)

