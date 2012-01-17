;; Diffie-Hellman Key Exchange

;; The Diffie-Hellman problem is given (exp (gen) m), and (exp (gen) n),
;; compute the value of (exp (gen) (mul m n)).

(defprotocol dhke diffie-hellman
  (defrole init (vars (a b name) (g base) (m expn))
    (trace
     (send (cat a (enc "i" (exp (gen) m) (privk a))))
     (recv (cat b (enc g (privk b)) (enc "r" a b (exp g m))))
     (send (enc "i" a b (exp g m))))
    (uniq-orig (exp (gen) m)))
  (defrole resp (vars (a b name) (h base) (n expn))
    (trace
     (recv (cat a (enc "i" h (privk a))))
     (send (cat b (enc (exp (gen) n) (privk b)) (enc "r" a b (exp h n))))
     (recv (enc "i" a b (exp h n))))
    (uniq-orig (exp (gen) n))))

(defskeleton dhke (vars (a b name) (h base) (n expn))
  (defstrand resp 3 (a a) (b b) (h h) (n n))
  (non-orig (privk a) (privk b) (exp h n)))

(defskeleton dhke (vars (a b name))
  (defstrand resp 3 (a a) (b b))
  (non-orig (privk a) (privk b)))
