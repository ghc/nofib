(defprotocol missing-contraction basic
  (defrole sender
    (vars (m n text) (a b name))
    (trace
     (send (enc a m (pubk a)))
     (send (enc a n (pubk b)))))
  (defrole receiver
    (vars (m text) (a b name))
    (trace
     (recv (enc a m (pubk b))))))

(defskeleton missing-contraction
  (vars (m text) (a c name))
  (defstrand sender 2 (m m) (a a))
  (defstrand receiver 1 (m m) (a a) (b c))
  (precedes ((0 1) (1 0)))
  (uniq-orig m)
  (non-orig (privk a)))

(defskeleton missing-contraction
  (vars (m text) (a c name))
  (defstrand sender 1 (m m) (a a))
  (deflistener (enc a m (pubk c)))
  (uniq-orig m)
  (non-orig (privk a)))
 

(defskeleton missing-contraction
  (vars (m text) (a b c name))
  (defstrand sender 2 (m m) (a a) (b b))
  (defstrand receiver 1 (m m) (a a) (b c))
  (precedes ((0 1) (1 0)))
  (uniq-orig m)
  (non-orig (privk a) (privk b)))