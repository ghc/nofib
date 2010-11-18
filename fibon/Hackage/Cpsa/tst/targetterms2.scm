;;; Target terms experiment

(defprotocol tt2 basic
  (defrole init (vars (a name) (n text))
    (trace
	(send (enc n (pubk a)))
      (recv (enc n (enc n (enc n (pubk a)) (pubk a)) (pubk a)))
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

(defskeleton tt2
  (vars (a name) (n text))
  (defstrand init 2 (a a) (n n))
)
