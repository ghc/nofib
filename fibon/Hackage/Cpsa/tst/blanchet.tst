(comment "CPSA 2.1.0")
(comment "All input read")

(defprotocol blanchet basic
  (defrole init
    (vars (a b name) (s skey) (d text))
    (trace (send (enc (enc s (privk a)) (pubk b))) (recv (enc d s))))
  (defrole resp
    (vars (a b name) (s skey) (d text))
    (trace (recv (enc (enc s (privk a)) (pubk b))) (send (enc d s)))))

(defskeleton blanchet
  (vars (d text) (a b name) (s skey))
  (defstrand resp 2 (d d) (a a) (b b) (s s))
  (non-orig (privk a) (privk b))
  (uniq-orig s)
  (traces ((recv (enc (enc s (privk a)) (pubk b))) (send (enc d s))))
  (label 0)
  (unrealized (0 0))
  (comment "1 in cohort - 1 not yet seen"))

(defskeleton blanchet
  (vars (d text) (a b b-0 name) (s skey))
  (defstrand resp 2 (d d) (a a) (b b) (s s))
  (defstrand init 1 (a a) (b b-0) (s s))
  (precedes ((1 0) (0 0)))
  (non-orig (privk a) (privk b))
  (uniq-orig s)
  (operation encryption-test (added-strand init 1) (enc s (privk a))
    (0 0))
  (traces ((recv (enc (enc s (privk a)) (pubk b))) (send (enc d s)))
    ((send (enc (enc s (privk a)) (pubk b-0)))))
  (label 1)
  (parent 0)
  (unrealized)
  (shape))

(comment "Nothing left to do")

(defprotocol blanchet basic
  (defrole init
    (vars (a b name) (s skey) (d text))
    (trace (send (enc (enc s (privk a)) (pubk b))) (recv (enc d s))))
  (defrole resp
    (vars (a b name) (s skey) (d text))
    (trace (recv (enc (enc s (privk a)) (pubk b))) (send (enc d s)))))

(defskeleton blanchet
  (vars (d text) (a b name) (s skey))
  (defstrand resp 2 (d d) (a a) (b b) (s s))
  (deflistener s)
  (non-orig (privk a) (privk b))
  (uniq-orig s)
  (traces ((recv (enc (enc s (privk a)) (pubk b))) (send (enc d s)))
    ((recv s) (send s)))
  (label 2)
  (unrealized (0 0))
  (comment "1 in cohort - 1 not yet seen"))

(defskeleton blanchet
  (vars (d text) (a b b-0 name) (s skey))
  (defstrand resp 2 (d d) (a a) (b b) (s s))
  (deflistener s)
  (defstrand init 1 (a a) (b b-0) (s s))
  (precedes ((2 0) (0 0)) ((2 0) (1 0)))
  (non-orig (privk a) (privk b))
  (uniq-orig s)
  (operation encryption-test (added-strand init 1) (enc s (privk a))
    (0 0))
  (traces ((recv (enc (enc s (privk a)) (pubk b))) (send (enc d s)))
    ((recv s) (send s)) ((send (enc (enc s (privk a)) (pubk b-0)))))
  (label 3)
  (parent 2)
  (unrealized)
  (shape))

(comment "Nothing left to do")

(defprotocol blanchet basic
  (defrole init
    (vars (a b name) (s skey) (d text))
    (trace (send (enc (enc s (privk a)) (pubk b))) (recv (enc d s))))
  (defrole resp
    (vars (a b name) (s skey) (d text))
    (trace (recv (enc (enc s (privk a)) (pubk b))) (send (enc d s)))))

(defskeleton blanchet
  (vars (d text) (a b name) (s skey))
  (defstrand init 2 (d d) (a a) (b b) (s s))
  (deflistener s)
  (non-orig (privk a) (privk b))
  (uniq-orig s)
  (traces ((send (enc (enc s (privk a)) (pubk b))) (recv (enc d s)))
    ((recv s) (send s)))
  (label 4)
  (unrealized (0 1) (1 0)))

(defskeleton blanchet
  (vars (d text) (a b name) (s skey))
  (defstrand init 2 (d d) (a a) (b b) (s s))
  (deflistener s)
  (precedes ((0 0) (1 0)))
  (non-orig (privk a) (privk b))
  (uniq-orig s)
  (traces ((send (enc (enc s (privk a)) (pubk b))) (recv (enc d s)))
    ((recv s) (send s)))
  (label 5)
  (parent 4)
  (unrealized (0 1) (1 0))
  (comment "empty cohort"))

(comment "Nothing left to do")

(defprotocol blanchet basic
  (defrole init
    (vars (a b name) (s skey) (d text))
    (trace (send (enc (enc s (privk a)) (pubk b))) (recv (enc d s))))
  (defrole resp
    (vars (a b name) (s skey) (d text))
    (trace (recv (enc (enc s (privk a)) (pubk b))) (send (enc d s)))))

(defskeleton blanchet
  (vars (d text) (a b name) (s skey))
  (defstrand init 2 (d d) (a a) (b b) (s s))
  (deflistener s)
  (non-orig (privk a))
  (uniq-orig s)
  (traces ((send (enc (enc s (privk a)) (pubk b))) (recv (enc d s)))
    ((recv s) (send s)))
  (label 6)
  (unrealized (1 0)))

(defskeleton blanchet
  (vars (d text) (a b name) (s skey))
  (defstrand init 2 (d d) (a a) (b b) (s s))
  (deflistener s)
  (precedes ((0 0) (1 0)))
  (non-orig (privk a))
  (uniq-orig s)
  (traces ((send (enc (enc s (privk a)) (pubk b))) (recv (enc d s)))
    ((recv s) (send s)))
  (label 7)
  (parent 6)
  (unrealized)
  (shape))

(comment "Nothing left to do")

(defprotocol blanchet basic
  (defrole init
    (vars (a b name) (s skey) (d text))
    (trace (send (enc (enc s (privk a)) (pubk b))) (recv (enc d s))))
  (defrole resp
    (vars (a b name) (s skey) (d text))
    (trace (recv (enc (enc s (privk a)) (pubk b))) (send (enc d s)))))

(defskeleton blanchet
  (vars (d text) (a b name) (s skey))
  (defstrand init 2 (d d) (a a) (b b) (s s))
  (deflistener s)
  (non-orig (privk a))
  (uniq-orig d s)
  (traces ((send (enc (enc s (privk a)) (pubk b))) (recv (enc d s)))
    ((recv s) (send s)))
  (label 8)
  (unrealized (1 0)))

(defskeleton blanchet
  (vars (d text) (a b name) (s skey))
  (defstrand init 2 (d d) (a a) (b b) (s s))
  (deflistener s)
  (precedes ((0 0) (1 0)))
  (non-orig (privk a))
  (uniq-orig d s)
  (traces ((send (enc (enc s (privk a)) (pubk b))) (recv (enc d s)))
    ((recv s) (send s)))
  (label 9)
  (parent 8)
  (unrealized)
  (shape))

(comment "Nothing left to do")
