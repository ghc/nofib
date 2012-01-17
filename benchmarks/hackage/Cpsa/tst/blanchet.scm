(defprotocol blanchet basic
  (defrole init
    (vars (a b name) (s skey) (d text))
    (trace
     (send (enc (enc s (privk a)) (pubk b)))
     (recv (enc d s))))
  (defrole resp
    (vars (a b name) (s skey) (d text))
    (trace
     (recv (enc (enc s (privk a)) (pubk b)))
     (send (enc d s)))))

(defskeleton blanchet
  (vars (a b name) (s skey) (d text))
  (defstrand resp 2 (a a) (b b) (s s) (d d))
  (non-orig (privk a) (privk b))
  (uniq-orig s))

(defskeleton blanchet
  (vars (a b name) (s skey) (d text))
  (defstrand resp 2 (a a) (b b) (s s) (d d))
  (deflistener s)
  (non-orig (privk a) (privk b))
  (uniq-orig s))

(defskeleton blanchet
  (vars (a b name) (s skey) (d text))
  (defstrand init 2 (a a) (b b) (s s) (d d))
  (deflistener s)
  (non-orig (privk a) (privk b))
  (uniq-orig s))

(defskeleton blanchet
  (vars (a b name) (s skey) (d text))
  (defstrand init 2 (a a) (b b) (s s) (d d))
  (deflistener s)
  (non-orig (privk a))
  (uniq-orig s))

(defskeleton blanchet
  (vars (a b name) (s skey) (d text))
  (defstrand init 2 (a a) (b b) (s s) (d d))
  (deflistener s)
  (non-orig (privk a))
  (uniq-orig s d))
