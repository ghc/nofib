(defprotocol dh_unifywith1 diffie-hellman
  (defrole resp (vars (n text) (x base) (a b name))
    (trace
     (recv (enc n (pubk b)))
     (send (enc x n (pubk a)))
    )
    (uniq-orig x)
    (non-orig (privk a))
  )
  (defrole init (vars (n text) (a b name))
    (trace
     (send (enc n (pubk b)))
     (recv (enc (gen) n (pubk a)))
    )
    (non-orig (privk b))
    (uniq-orig n)
  )
  (comment "A test: will CPSA unify x with g?")
)

(defskeleton dh_unifywith1
   (vars (n text) (a b name))
   (defstrand init 2 (n n) (a a) (b b))
   (comment "Initiator full point of view")
)
