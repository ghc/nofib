(comment "CPSA 2.1.0")
(comment "All input read")

(defprotocol dh_unifywith1 diffie-hellman
  (defrole resp
    (vars (n text) (x base) (a b name))
    (trace (recv (enc n (pubk b))) (send (enc x n (pubk a))))
    (non-orig (privk a))
    (uniq-orig x))
  (defrole init
    (vars (n text) (a b name))
    (trace (send (enc n (pubk b))) (recv (enc (gen) n (pubk a))))
    (non-orig (privk b))
    (uniq-orig n))
  (comment "A test: will CPSA unify x with g?"))

(defskeleton dh_unifywith1
  (vars (n text) (a b name))
  (defstrand init 2 (n n) (a a) (b b))
  (non-orig (privk b))
  (uniq-orig n)
  (comment "Initiator full point of view")
  (traces ((send (enc n (pubk b))) (recv (enc (gen) n (pubk a)))))
  (label 0)
  (unrealized (0 1))
  (comment "1 in cohort - 1 not yet seen"))

(defskeleton dh_unifywith1
  (vars (n text) (a b a-0 name) (x base))
  (defstrand init 2 (n n) (a a) (b b))
  (defstrand resp 2 (n n) (a a-0) (b b) (x x))
  (precedes ((0 0) (1 0)) ((1 1) (0 1)))
  (non-orig (privk b) (privk a-0))
  (uniq-orig n x)
  (operation nonce-test (added-strand resp 2) n (0 1) (enc n (pubk b)))
  (traces ((send (enc n (pubk b))) (recv (enc (gen) n (pubk a))))
    ((recv (enc n (pubk b))) (send (enc x n (pubk a-0)))))
  (label 1)
  (parent 0)
  (unrealized (0 1))
  (comment "1 in cohort - 1 not yet seen"))

(defskeleton dh_unifywith1
  (vars (n text) (b a name))
  (defstrand init 2 (n n) (a a) (b b))
  (defstrand resp 2 (n n) (a a) (b b) (x (gen)))
  (precedes ((0 0) (1 0)) ((1 1) (0 1)))
  (non-orig (privk b) (privk a))
  (uniq-orig n (gen))
  (operation nonce-test (contracted (a-0 a) (x (gen))) n (0 1)
    (enc n (pubk b)) (enc (gen) n (pubk a)))
  (traces ((send (enc n (pubk b))) (recv (enc (gen) n (pubk a))))
    ((recv (enc n (pubk b))) (send (enc (gen) n (pubk a)))))
  (label 2)
  (parent 1)
  (unrealized)
  (shape))

(comment "Nothing left to do")
