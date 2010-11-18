;;; Target terms experiment

(defprotocol tt6 basic
  (defrole init (vars (a name) (n text) (m mesg))
    (trace
	(send (enc n (pubk a)))
      (recv (cat (enc n (enc n (enc n (pubk a)) (pubk a)) (pubk a))
         (enc n (enc n (pubk a)) (pubk a)) ))
    )
    (uniq-orig n)
    (non-orig (privk a))
)
  (defrole trans (vars (a name) (n text) (m mesg))
    (trace
     (recv (enc n (pubk a)))
     (recv m)
     (send (enc n m (pubk a)))
    ))
)

(defskeleton tt6
  (vars (a name) (n text))
  (defstrand init 2 (a a) (n n))
)

(defskeleton tt6
  (vars (n text) (a name))
  (defstrand init 2 (n n) (a a))
  (defstrand trans 3 (m (enc n (pubk a))) (n n) (a a))
  (defstrand trans 3 (m (enc n (enc n (pubk a)) (pubk a))) (n n) (a a))
  (precedes ((0 0) (1 0)) ((0 0) (2 0)) ((1 2) (2 1)) ((2 2) (0 1)))
)
