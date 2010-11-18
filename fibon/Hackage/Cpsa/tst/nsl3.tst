(comment "CPSA 2.1.0")
(comment "All input read")

(defprotocol nsl3 basic
  (defrole init
    (vars (a b c name) (na0 na1 nb0 nb1 nc0 nc1 text))
    (trace (send (enc na0 a c (enc na1 a b (pubk c)) (pubk b)))
      (recv
        (enc na1 nc1 c b (enc na0 nb0 b c (pubk a))
          (enc nb1 nc0 c a (pubk b)) (pubk a)))
      (send
        (enc nb0 (enc nb1 nc0 c a (pubk b)) (enc nc1 (pubk c))
          (pubk b))))
    (uniq-orig na0 na1))
  (defrole mid
    (vars (a b c name) (na0 na1 nb0 nb1 nc0 nc1 text))
    (trace (recv (enc na0 a c (enc na1 a b (pubk c)) (pubk b)))
      (send
        (enc nb1 b a (enc na1 a b (pubk c)) (enc na0 nb0 b c (pubk a))
          (pubk c)))
      (recv
        (enc nb0 (enc nb1 nc0 c a (pubk b)) (enc nc1 (pubk c))
          (pubk b))) (send (enc nc0 (enc nc1 (pubk c)) (pubk c))))
    (uniq-orig nb0 nb1))
  (defrole resp
    (vars (a b c name) (na0 na1 nb0 nb1 nc0 nc1 text))
    (trace
      (recv
        (enc nb1 b a (enc na1 a b (pubk c)) (enc na0 nb0 b c (pubk a))
          (pubk c)))
      (send
        (enc na1 nc1 c b (enc na0 nb0 b c (pubk a))
          (enc nb1 nc0 c a (pubk b)) (pubk a)))
      (recv (enc nc0 (enc nc1 (pubk c)) (pubk c))))
    (uniq-orig nc0 nc1)))

(defskeleton nsl3
  (vars (na0 na1 nb0 nb1 nc0 nc1 text) (a b c name))
  (defstrand init 3 (na0 na0) (na1 na1) (nb0 nb0) (nb1 nb1) (nc0 nc0)
    (nc1 nc1) (a a) (b b) (c c))
  (non-orig (privk a) (privk b) (privk c))
  (uniq-orig na0 na1)
  (traces
    ((send (enc na0 a c (enc na1 a b (pubk c)) (pubk b)))
      (recv
        (enc na1 nc1 c b (enc na0 nb0 b c (pubk a))
          (enc nb1 nc0 c a (pubk b)) (pubk a)))
      (send
        (enc nb0 (enc nb1 nc0 c a (pubk b)) (enc nc1 (pubk c))
          (pubk b)))))
  (label 0)
  (unrealized (0 1))
  (comment "1 in cohort - 1 not yet seen"))

(defskeleton nsl3
  (vars (na0 na1 nb0 nb1 nc0 nc1 nb0-0 nb1-0 text) (a b c name))
  (defstrand init 3 (na0 na0) (na1 na1) (nb0 nb0) (nb1 nb1) (nc0 nc0)
    (nc1 nc1) (a a) (b b) (c c))
  (defstrand mid 2 (na0 na0) (na1 na1) (nb0 nb0-0) (nb1 nb1-0) (a a)
    (b b) (c c))
  (precedes ((0 0) (1 0)) ((1 1) (0 1)))
  (non-orig (privk a) (privk b) (privk c))
  (uniq-orig na0 na1 nb0-0 nb1-0)
  (operation nonce-test (added-strand mid 2) na0 (0 1)
    (enc na0 a c (enc na1 a b (pubk c)) (pubk b)))
  (traces
    ((send (enc na0 a c (enc na1 a b (pubk c)) (pubk b)))
      (recv
        (enc na1 nc1 c b (enc na0 nb0 b c (pubk a))
          (enc nb1 nc0 c a (pubk b)) (pubk a)))
      (send
        (enc nb0 (enc nb1 nc0 c a (pubk b)) (enc nc1 (pubk c))
          (pubk b))))
    ((recv (enc na0 a c (enc na1 a b (pubk c)) (pubk b)))
      (send
        (enc nb1-0 b a (enc na1 a b (pubk c))
          (enc na0 nb0-0 b c (pubk a)) (pubk c)))))
  (label 1)
  (parent 0)
  (unrealized (0 1))
  (comment "1 in cohort - 1 not yet seen"))

(defskeleton nsl3
  (vars (na0 na1 nb0 nb1 nc0 nc1 nb0-0 nb1-0 nc0-0 nc1-0 text)
    (a b c name))
  (defstrand init 3 (na0 na0) (na1 na1) (nb0 nb0) (nb1 nb1) (nc0 nc0)
    (nc1 nc1) (a a) (b b) (c c))
  (defstrand mid 2 (na0 na0) (na1 na1) (nb0 nb0-0) (nb1 nb1-0) (a a)
    (b b) (c c))
  (defstrand resp 2 (na0 na0) (na1 na1) (nb0 nb0-0) (nb1 nb1-0)
    (nc0 nc0-0) (nc1 nc1-0) (a a) (b b) (c c))
  (precedes ((0 0) (1 0)) ((1 1) (2 0)) ((2 1) (0 1)))
  (non-orig (privk a) (privk b) (privk c))
  (uniq-orig na0 na1 nb0-0 nb1-0 nc0-0 nc1-0)
  (operation nonce-test (added-strand resp 2) na0 (0 1)
    (enc na0 a c (enc na1 a b (pubk c)) (pubk b))
    (enc nb1-0 b a (enc na1 a b (pubk c)) (enc na0 nb0-0 b c (pubk a))
      (pubk c)))
  (traces
    ((send (enc na0 a c (enc na1 a b (pubk c)) (pubk b)))
      (recv
        (enc na1 nc1 c b (enc na0 nb0 b c (pubk a))
          (enc nb1 nc0 c a (pubk b)) (pubk a)))
      (send
        (enc nb0 (enc nb1 nc0 c a (pubk b)) (enc nc1 (pubk c))
          (pubk b))))
    ((recv (enc na0 a c (enc na1 a b (pubk c)) (pubk b)))
      (send
        (enc nb1-0 b a (enc na1 a b (pubk c))
          (enc na0 nb0-0 b c (pubk a)) (pubk c))))
    ((recv
       (enc nb1-0 b a (enc na1 a b (pubk c))
         (enc na0 nb0-0 b c (pubk a)) (pubk c)))
      (send
        (enc na1 nc1-0 c b (enc na0 nb0-0 b c (pubk a))
          (enc nb1-0 nc0-0 c a (pubk b)) (pubk a)))))
  (label 2)
  (parent 1)
  (unrealized (0 1))
  (comment "1 in cohort - 1 not yet seen"))

(defskeleton nsl3
  (vars (na0 na1 nb0 nb1 nc0 nc1 text) (a b c name))
  (defstrand init 3 (na0 na0) (na1 na1) (nb0 nb0) (nb1 nb1) (nc0 nc0)
    (nc1 nc1) (a a) (b b) (c c))
  (defstrand mid 2 (na0 na0) (na1 na1) (nb0 nb0) (nb1 nb1) (a a) (b b)
    (c c))
  (defstrand resp 2 (na0 na0) (na1 na1) (nb0 nb0) (nb1 nb1) (nc0 nc0)
    (nc1 nc1) (a a) (b b) (c c))
  (precedes ((0 0) (1 0)) ((1 1) (2 0)) ((2 1) (0 1)))
  (non-orig (privk a) (privk b) (privk c))
  (uniq-orig na0 na1 nb0 nb1 nc0 nc1)
  (operation nonce-test
    (contracted (nb0-0 nb0) (nb1-0 nb1) (nc0-0 nc0) (nc1-0 nc1)) na0
    (0 1) (enc na0 a c (enc na1 a b (pubk c)) (pubk b))
    (enc na1 nc1 c b (enc na0 nb0 b c (pubk a))
      (enc nb1 nc0 c a (pubk b)) (pubk a))
    (enc nb1 b a (enc na1 a b (pubk c)) (enc na0 nb0 b c (pubk a))
      (pubk c)))
  (traces
    ((send (enc na0 a c (enc na1 a b (pubk c)) (pubk b)))
      (recv
        (enc na1 nc1 c b (enc na0 nb0 b c (pubk a))
          (enc nb1 nc0 c a (pubk b)) (pubk a)))
      (send
        (enc nb0 (enc nb1 nc0 c a (pubk b)) (enc nc1 (pubk c))
          (pubk b))))
    ((recv (enc na0 a c (enc na1 a b (pubk c)) (pubk b)))
      (send
        (enc nb1 b a (enc na1 a b (pubk c)) (enc na0 nb0 b c (pubk a))
          (pubk c))))
    ((recv
       (enc nb1 b a (enc na1 a b (pubk c)) (enc na0 nb0 b c (pubk a))
         (pubk c)))
      (send
        (enc na1 nc1 c b (enc na0 nb0 b c (pubk a))
          (enc nb1 nc0 c a (pubk b)) (pubk a)))))
  (label 3)
  (parent 2)
  (unrealized)
  (shape))

(comment "Nothing left to do")

(defprotocol nsl3 basic
  (defrole init
    (vars (a b c name) (na0 na1 nb0 nb1 nc0 nc1 text))
    (trace (send (enc na0 a c (enc na1 a b (pubk c)) (pubk b)))
      (recv
        (enc na1 nc1 c b (enc na0 nb0 b c (pubk a))
          (enc nb1 nc0 c a (pubk b)) (pubk a)))
      (send
        (enc nb0 (enc nb1 nc0 c a (pubk b)) (enc nc1 (pubk c))
          (pubk b))))
    (uniq-orig na0 na1))
  (defrole mid
    (vars (a b c name) (na0 na1 nb0 nb1 nc0 nc1 text))
    (trace (recv (enc na0 a c (enc na1 a b (pubk c)) (pubk b)))
      (send
        (enc nb1 b a (enc na1 a b (pubk c)) (enc na0 nb0 b c (pubk a))
          (pubk c)))
      (recv
        (enc nb0 (enc nb1 nc0 c a (pubk b)) (enc nc1 (pubk c))
          (pubk b))) (send (enc nc0 (enc nc1 (pubk c)) (pubk c))))
    (uniq-orig nb0 nb1))
  (defrole resp
    (vars (a b c name) (na0 na1 nb0 nb1 nc0 nc1 text))
    (trace
      (recv
        (enc nb1 b a (enc na1 a b (pubk c)) (enc na0 nb0 b c (pubk a))
          (pubk c)))
      (send
        (enc na1 nc1 c b (enc na0 nb0 b c (pubk a))
          (enc nb1 nc0 c a (pubk b)) (pubk a)))
      (recv (enc nc0 (enc nc1 (pubk c)) (pubk c))))
    (uniq-orig nc0 nc1)))

(defskeleton nsl3
  (vars (na0 na1 nb0 nb1 nc0 nc1 text) (a b c name))
  (defstrand mid 4 (na0 na0) (na1 na1) (nb0 nb0) (nb1 nb1) (nc0 nc0)
    (nc1 nc1) (a a) (b b) (c c))
  (non-orig (privk a) (privk b) (privk c))
  (uniq-orig nb0 nb1)
  (traces
    ((recv (enc na0 a c (enc na1 a b (pubk c)) (pubk b)))
      (send
        (enc nb1 b a (enc na1 a b (pubk c)) (enc na0 nb0 b c (pubk a))
          (pubk c)))
      (recv
        (enc nb0 (enc nb1 nc0 c a (pubk b)) (enc nc1 (pubk c))
          (pubk b))) (send (enc nc0 (enc nc1 (pubk c)) (pubk c)))))
  (label 4)
  (unrealized (0 2))
  (comment "1 in cohort - 1 not yet seen"))

(defskeleton nsl3
  (vars (na0 na1 nb0 nb1 nc0 nc1 nc0-0 nc1-0 text) (a b c name))
  (defstrand mid 4 (na0 na0) (na1 na1) (nb0 nb0) (nb1 nb1) (nc0 nc0)
    (nc1 nc1) (a a) (b b) (c c))
  (defstrand resp 2 (na0 na0) (na1 na1) (nb0 nb0) (nb1 nb1) (nc0 nc0-0)
    (nc1 nc1-0) (a a) (b b) (c c))
  (precedes ((0 1) (1 0)) ((1 1) (0 2)))
  (non-orig (privk a) (privk b) (privk c))
  (uniq-orig nb0 nb1 nc0-0 nc1-0)
  (operation nonce-test (added-strand resp 2) nb0 (0 2)
    (enc nb1 b a (enc na1 a b (pubk c)) (enc na0 nb0 b c (pubk a))
      (pubk c)))
  (traces
    ((recv (enc na0 a c (enc na1 a b (pubk c)) (pubk b)))
      (send
        (enc nb1 b a (enc na1 a b (pubk c)) (enc na0 nb0 b c (pubk a))
          (pubk c)))
      (recv
        (enc nb0 (enc nb1 nc0 c a (pubk b)) (enc nc1 (pubk c))
          (pubk b))) (send (enc nc0 (enc nc1 (pubk c)) (pubk c))))
    ((recv
       (enc nb1 b a (enc na1 a b (pubk c)) (enc na0 nb0 b c (pubk a))
         (pubk c)))
      (send
        (enc na1 nc1-0 c b (enc na0 nb0 b c (pubk a))
          (enc nb1 nc0-0 c a (pubk b)) (pubk a)))))
  (label 5)
  (parent 4)
  (unrealized (0 2))
  (comment "1 in cohort - 1 not yet seen"))

(defskeleton nsl3
  (vars (na0 na1 nb0 nb1 nc0 nc1 nc0-0 nc1-0 text) (a b c name))
  (defstrand mid 4 (na0 na0) (na1 na1) (nb0 nb0) (nb1 nb1) (nc0 nc0)
    (nc1 nc1) (a a) (b b) (c c))
  (defstrand resp 2 (na0 na0) (na1 na1) (nb0 nb0) (nb1 nb1) (nc0 nc0-0)
    (nc1 nc1-0) (a a) (b b) (c c))
  (defstrand init 3 (na0 na0) (na1 na1) (nb0 nb0) (nb1 nb1) (nc0 nc0-0)
    (nc1 nc1-0) (a a) (b b) (c c))
  (precedes ((0 1) (1 0)) ((1 1) (2 1)) ((2 0) (0 0)) ((2 2) (0 2)))
  (non-orig (privk a) (privk b) (privk c))
  (uniq-orig na0 na1 nb0 nb1 nc0-0 nc1-0)
  (operation nonce-test (added-strand init 3) nb0 (0 2)
    (enc na1 nc1-0 c b (enc na0 nb0 b c (pubk a))
      (enc nb1 nc0-0 c a (pubk b)) (pubk a))
    (enc nb1 b a (enc na1 a b (pubk c)) (enc na0 nb0 b c (pubk a))
      (pubk c)))
  (traces
    ((recv (enc na0 a c (enc na1 a b (pubk c)) (pubk b)))
      (send
        (enc nb1 b a (enc na1 a b (pubk c)) (enc na0 nb0 b c (pubk a))
          (pubk c)))
      (recv
        (enc nb0 (enc nb1 nc0 c a (pubk b)) (enc nc1 (pubk c))
          (pubk b))) (send (enc nc0 (enc nc1 (pubk c)) (pubk c))))
    ((recv
       (enc nb1 b a (enc na1 a b (pubk c)) (enc na0 nb0 b c (pubk a))
         (pubk c)))
      (send
        (enc na1 nc1-0 c b (enc na0 nb0 b c (pubk a))
          (enc nb1 nc0-0 c a (pubk b)) (pubk a))))
    ((send (enc na0 a c (enc na1 a b (pubk c)) (pubk b)))
      (recv
        (enc na1 nc1-0 c b (enc na0 nb0 b c (pubk a))
          (enc nb1 nc0-0 c a (pubk b)) (pubk a)))
      (send
        (enc nb0 (enc nb1 nc0-0 c a (pubk b)) (enc nc1-0 (pubk c))
          (pubk b)))))
  (label 6)
  (parent 5)
  (unrealized (0 2))
  (comment "1 in cohort - 1 not yet seen"))

(defskeleton nsl3
  (vars (na0 na1 nb0 nb1 nc0 nc1 text) (a b c name))
  (defstrand mid 4 (na0 na0) (na1 na1) (nb0 nb0) (nb1 nb1) (nc0 nc0)
    (nc1 nc1) (a a) (b b) (c c))
  (defstrand resp 2 (na0 na0) (na1 na1) (nb0 nb0) (nb1 nb1) (nc0 nc0)
    (nc1 nc1) (a a) (b b) (c c))
  (defstrand init 3 (na0 na0) (na1 na1) (nb0 nb0) (nb1 nb1) (nc0 nc0)
    (nc1 nc1) (a a) (b b) (c c))
  (precedes ((0 1) (1 0)) ((1 1) (2 1)) ((2 0) (0 0)) ((2 2) (0 2)))
  (non-orig (privk a) (privk b) (privk c))
  (uniq-orig na0 na1 nb0 nb1 nc0 nc1)
  (operation nonce-test (contracted (nc0-0 nc0) (nc1-0 nc1)) nb0 (0 2)
    (enc na1 nc1 c b (enc na0 nb0 b c (pubk a))
      (enc nb1 nc0 c a (pubk b)) (pubk a))
    (enc nb0 (enc nb1 nc0 c a (pubk b)) (enc nc1 (pubk c)) (pubk b))
    (enc nb1 b a (enc na1 a b (pubk c)) (enc na0 nb0 b c (pubk a))
      (pubk c)))
  (traces
    ((recv (enc na0 a c (enc na1 a b (pubk c)) (pubk b)))
      (send
        (enc nb1 b a (enc na1 a b (pubk c)) (enc na0 nb0 b c (pubk a))
          (pubk c)))
      (recv
        (enc nb0 (enc nb1 nc0 c a (pubk b)) (enc nc1 (pubk c))
          (pubk b))) (send (enc nc0 (enc nc1 (pubk c)) (pubk c))))
    ((recv
       (enc nb1 b a (enc na1 a b (pubk c)) (enc na0 nb0 b c (pubk a))
         (pubk c)))
      (send
        (enc na1 nc1 c b (enc na0 nb0 b c (pubk a))
          (enc nb1 nc0 c a (pubk b)) (pubk a))))
    ((send (enc na0 a c (enc na1 a b (pubk c)) (pubk b)))
      (recv
        (enc na1 nc1 c b (enc na0 nb0 b c (pubk a))
          (enc nb1 nc0 c a (pubk b)) (pubk a)))
      (send
        (enc nb0 (enc nb1 nc0 c a (pubk b)) (enc nc1 (pubk c))
          (pubk b)))))
  (label 7)
  (parent 6)
  (unrealized)
  (shape))

(comment "Nothing left to do")

(defprotocol nsl3 basic
  (defrole init
    (vars (a b c name) (na0 na1 nb0 nb1 nc0 nc1 text))
    (trace (send (enc na0 a c (enc na1 a b (pubk c)) (pubk b)))
      (recv
        (enc na1 nc1 c b (enc na0 nb0 b c (pubk a))
          (enc nb1 nc0 c a (pubk b)) (pubk a)))
      (send
        (enc nb0 (enc nb1 nc0 c a (pubk b)) (enc nc1 (pubk c))
          (pubk b))))
    (uniq-orig na0 na1))
  (defrole mid
    (vars (a b c name) (na0 na1 nb0 nb1 nc0 nc1 text))
    (trace (recv (enc na0 a c (enc na1 a b (pubk c)) (pubk b)))
      (send
        (enc nb1 b a (enc na1 a b (pubk c)) (enc na0 nb0 b c (pubk a))
          (pubk c)))
      (recv
        (enc nb0 (enc nb1 nc0 c a (pubk b)) (enc nc1 (pubk c))
          (pubk b))) (send (enc nc0 (enc nc1 (pubk c)) (pubk c))))
    (uniq-orig nb0 nb1))
  (defrole resp
    (vars (a b c name) (na0 na1 nb0 nb1 nc0 nc1 text))
    (trace
      (recv
        (enc nb1 b a (enc na1 a b (pubk c)) (enc na0 nb0 b c (pubk a))
          (pubk c)))
      (send
        (enc na1 nc1 c b (enc na0 nb0 b c (pubk a))
          (enc nb1 nc0 c a (pubk b)) (pubk a)))
      (recv (enc nc0 (enc nc1 (pubk c)) (pubk c))))
    (uniq-orig nc0 nc1)))

(defskeleton nsl3
  (vars (na0 na1 nb0 nb1 nc0 nc1 text) (a b c name))
  (defstrand resp 3 (na0 na0) (na1 na1) (nb0 nb0) (nb1 nb1) (nc0 nc0)
    (nc1 nc1) (a a) (b b) (c c))
  (non-orig (privk a) (privk b) (privk c))
  (uniq-orig nc0 nc1)
  (traces
    ((recv
       (enc nb1 b a (enc na1 a b (pubk c)) (enc na0 nb0 b c (pubk a))
         (pubk c)))
      (send
        (enc na1 nc1 c b (enc na0 nb0 b c (pubk a))
          (enc nb1 nc0 c a (pubk b)) (pubk a)))
      (recv (enc nc0 (enc nc1 (pubk c)) (pubk c)))))
  (label 8)
  (unrealized (0 2))
  (comment "1 in cohort - 1 not yet seen"))

(defskeleton nsl3
  (vars (na0 na1 nb0 nb1 nc0 nc1 text) (a b c name))
  (defstrand resp 3 (na0 na0) (na1 na1) (nb0 nb0) (nb1 nb1) (nc0 nc0)
    (nc1 nc1) (a a) (b b) (c c))
  (defstrand init 3 (na0 na0) (na1 na1) (nb0 nb0) (nb1 nb1) (nc0 nc0)
    (nc1 nc1) (a a) (b b) (c c))
  (precedes ((0 1) (1 1)) ((1 0) (0 0)) ((1 2) (0 2)))
  (non-orig (privk a) (privk b) (privk c))
  (uniq-orig na0 na1 nc0 nc1)
  (operation nonce-test (added-strand init 3) nc0 (0 2)
    (enc na1 nc1 c b (enc na0 nb0 b c (pubk a))
      (enc nb1 nc0 c a (pubk b)) (pubk a)))
  (traces
    ((recv
       (enc nb1 b a (enc na1 a b (pubk c)) (enc na0 nb0 b c (pubk a))
         (pubk c)))
      (send
        (enc na1 nc1 c b (enc na0 nb0 b c (pubk a))
          (enc nb1 nc0 c a (pubk b)) (pubk a)))
      (recv (enc nc0 (enc nc1 (pubk c)) (pubk c))))
    ((send (enc na0 a c (enc na1 a b (pubk c)) (pubk b)))
      (recv
        (enc na1 nc1 c b (enc na0 nb0 b c (pubk a))
          (enc nb1 nc0 c a (pubk b)) (pubk a)))
      (send
        (enc nb0 (enc nb1 nc0 c a (pubk b)) (enc nc1 (pubk c))
          (pubk b)))))
  (label 9)
  (parent 8)
  (unrealized (0 0) (0 2))
  (comment "1 in cohort - 1 not yet seen"))

(defskeleton nsl3
  (vars (na0 na1 nb0 nb1 nc0 nc1 nb0-0 nb1-0 text) (a b c name))
  (defstrand resp 3 (na0 na0) (na1 na1) (nb0 nb0) (nb1 nb1) (nc0 nc0)
    (nc1 nc1) (a a) (b b) (c c))
  (defstrand init 3 (na0 na0) (na1 na1) (nb0 nb0) (nb1 nb1) (nc0 nc0)
    (nc1 nc1) (a a) (b b) (c c))
  (defstrand mid 2 (na0 na0) (na1 na1) (nb0 nb0-0) (nb1 nb1-0) (a a)
    (b b) (c c))
  (precedes ((0 1) (1 1)) ((1 0) (2 0)) ((1 2) (0 2)) ((2 1) (0 0)))
  (non-orig (privk a) (privk b) (privk c))
  (uniq-orig na0 na1 nc0 nc1 nb0-0 nb1-0)
  (operation nonce-test (added-strand mid 2) na1 (0 0)
    (enc na0 a c (enc na1 a b (pubk c)) (pubk b)))
  (traces
    ((recv
       (enc nb1 b a (enc na1 a b (pubk c)) (enc na0 nb0 b c (pubk a))
         (pubk c)))
      (send
        (enc na1 nc1 c b (enc na0 nb0 b c (pubk a))
          (enc nb1 nc0 c a (pubk b)) (pubk a)))
      (recv (enc nc0 (enc nc1 (pubk c)) (pubk c))))
    ((send (enc na0 a c (enc na1 a b (pubk c)) (pubk b)))
      (recv
        (enc na1 nc1 c b (enc na0 nb0 b c (pubk a))
          (enc nb1 nc0 c a (pubk b)) (pubk a)))
      (send
        (enc nb0 (enc nb1 nc0 c a (pubk b)) (enc nc1 (pubk c))
          (pubk b))))
    ((recv (enc na0 a c (enc na1 a b (pubk c)) (pubk b)))
      (send
        (enc nb1-0 b a (enc na1 a b (pubk c))
          (enc na0 nb0-0 b c (pubk a)) (pubk c)))))
  (label 10)
  (parent 9)
  (unrealized (0 0) (0 2))
  (comment "2 in cohort - 2 not yet seen"))

(defskeleton nsl3
  (vars (na0 na1 nc0 nc1 nb0 nb1 text) (a b c name))
  (defstrand resp 3 (na0 na0) (na1 na1) (nb0 nb0) (nb1 nb1) (nc0 nc0)
    (nc1 nc1) (a a) (b b) (c c))
  (defstrand init 3 (na0 na0) (na1 na1) (nb0 nb0) (nb1 nb1) (nc0 nc0)
    (nc1 nc1) (a a) (b b) (c c))
  (defstrand mid 2 (na0 na0) (na1 na1) (nb0 nb0) (nb1 nb1) (a a) (b b)
    (c c))
  (precedes ((0 1) (1 1)) ((1 0) (2 0)) ((1 2) (0 2)) ((2 1) (0 0)))
  (non-orig (privk a) (privk b) (privk c))
  (uniq-orig na0 na1 nc0 nc1 nb0 nb1)
  (operation nonce-test (contracted (nb0-0 nb0) (nb1-0 nb1)) na1 (0 0)
    (enc na0 a c (enc na1 a b (pubk c)) (pubk b))
    (enc nb1 b a (enc na1 a b (pubk c)) (enc na0 nb0 b c (pubk a))
      (pubk c)))
  (traces
    ((recv
       (enc nb1 b a (enc na1 a b (pubk c)) (enc na0 nb0 b c (pubk a))
         (pubk c)))
      (send
        (enc na1 nc1 c b (enc na0 nb0 b c (pubk a))
          (enc nb1 nc0 c a (pubk b)) (pubk a)))
      (recv (enc nc0 (enc nc1 (pubk c)) (pubk c))))
    ((send (enc na0 a c (enc na1 a b (pubk c)) (pubk b)))
      (recv
        (enc na1 nc1 c b (enc na0 nb0 b c (pubk a))
          (enc nb1 nc0 c a (pubk b)) (pubk a)))
      (send
        (enc nb0 (enc nb1 nc0 c a (pubk b)) (enc nc1 (pubk c))
          (pubk b))))
    ((recv (enc na0 a c (enc na1 a b (pubk c)) (pubk b)))
      (send
        (enc nb1 b a (enc na1 a b (pubk c)) (enc na0 nb0 b c (pubk a))
          (pubk c)))))
  (label 11)
  (parent 10)
  (unrealized (0 2))
  (comment "1 in cohort - 1 not yet seen"))

(defskeleton nsl3
  (vars (na0 na1 nb0 nb1 nc0 nc1 nb0-0 nb1-0 nc0-0 nc1-0 text)
    (a b c name))
  (defstrand resp 3 (na0 na0) (na1 na1) (nb0 nb0) (nb1 nb1) (nc0 nc0)
    (nc1 nc1) (a a) (b b) (c c))
  (defstrand init 3 (na0 na0) (na1 na1) (nb0 nb0) (nb1 nb1) (nc0 nc0)
    (nc1 nc1) (a a) (b b) (c c))
  (defstrand mid 2 (na0 na0) (na1 na1) (nb0 nb0-0) (nb1 nb1-0) (a a)
    (b b) (c c))
  (defstrand resp 2 (na0 na0) (na1 na1) (nb0 nb0-0) (nb1 nb1-0)
    (nc0 nc0-0) (nc1 nc1-0) (a a) (b b) (c c))
  (precedes ((0 1) (1 1)) ((1 0) (2 0)) ((1 2) (0 2)) ((2 1) (3 0))
    ((3 1) (0 0)))
  (non-orig (privk a) (privk b) (privk c))
  (uniq-orig na0 na1 nc0 nc1 nb0-0 nb1-0 nc0-0 nc1-0)
  (operation nonce-test (added-strand resp 2) na1 (0 0)
    (enc na0 a c (enc na1 a b (pubk c)) (pubk b))
    (enc nb1-0 b a (enc na1 a b (pubk c)) (enc na0 nb0-0 b c (pubk a))
      (pubk c)))
  (traces
    ((recv
       (enc nb1 b a (enc na1 a b (pubk c)) (enc na0 nb0 b c (pubk a))
         (pubk c)))
      (send
        (enc na1 nc1 c b (enc na0 nb0 b c (pubk a))
          (enc nb1 nc0 c a (pubk b)) (pubk a)))
      (recv (enc nc0 (enc nc1 (pubk c)) (pubk c))))
    ((send (enc na0 a c (enc na1 a b (pubk c)) (pubk b)))
      (recv
        (enc na1 nc1 c b (enc na0 nb0 b c (pubk a))
          (enc nb1 nc0 c a (pubk b)) (pubk a)))
      (send
        (enc nb0 (enc nb1 nc0 c a (pubk b)) (enc nc1 (pubk c))
          (pubk b))))
    ((recv (enc na0 a c (enc na1 a b (pubk c)) (pubk b)))
      (send
        (enc nb1-0 b a (enc na1 a b (pubk c))
          (enc na0 nb0-0 b c (pubk a)) (pubk c))))
    ((recv
       (enc nb1-0 b a (enc na1 a b (pubk c))
         (enc na0 nb0-0 b c (pubk a)) (pubk c)))
      (send
        (enc na1 nc1-0 c b (enc na0 nb0-0 b c (pubk a))
          (enc nb1-0 nc0-0 c a (pubk b)) (pubk a)))))
  (label 12)
  (parent 10)
  (unrealized (0 0) (0 2))
  (comment "1 in cohort - 1 not yet seen"))

(defskeleton nsl3
  (vars (nc0 nc1 nb0 nb1 na0 na1 text) (a b c name))
  (defstrand resp 3 (na0 na0) (na1 na1) (nb0 nb0) (nb1 nb1) (nc0 nc0)
    (nc1 nc1) (a a) (b b) (c c))
  (defstrand init 3 (na0 na0) (na1 na1) (nb0 nb0) (nb1 nb1) (nc0 nc0)
    (nc1 nc1) (a a) (b b) (c c))
  (defstrand mid 4 (na0 na0) (na1 na1) (nb0 nb0) (nb1 nb1) (nc0 nc0)
    (nc1 nc1) (a a) (b b) (c c))
  (precedes ((0 1) (1 1)) ((0 1) (2 2)) ((1 0) (2 0)) ((1 2) (0 2))
    ((2 1) (0 0)) ((2 3) (0 2)))
  (non-orig (privk a) (privk b) (privk c))
  (uniq-orig nc0 nc1 nb0 nb1 na0 na1)
  (operation nonce-test (added-strand mid 4) nc0 (0 2)
    (enc nb0 (enc nb1 nc0 c a (pubk b)) (enc nc1 (pubk c)) (pubk b))
    (enc na1 nc1 c b (enc na0 nb0 b c (pubk a))
      (enc nb1 nc0 c a (pubk b)) (pubk a)))
  (traces
    ((recv
       (enc nb1 b a (enc na1 a b (pubk c)) (enc na0 nb0 b c (pubk a))
         (pubk c)))
      (send
        (enc na1 nc1 c b (enc na0 nb0 b c (pubk a))
          (enc nb1 nc0 c a (pubk b)) (pubk a)))
      (recv (enc nc0 (enc nc1 (pubk c)) (pubk c))))
    ((send (enc na0 a c (enc na1 a b (pubk c)) (pubk b)))
      (recv
        (enc na1 nc1 c b (enc na0 nb0 b c (pubk a))
          (enc nb1 nc0 c a (pubk b)) (pubk a)))
      (send
        (enc nb0 (enc nb1 nc0 c a (pubk b)) (enc nc1 (pubk c))
          (pubk b))))
    ((recv (enc na0 a c (enc na1 a b (pubk c)) (pubk b)))
      (send
        (enc nb1 b a (enc na1 a b (pubk c)) (enc na0 nb0 b c (pubk a))
          (pubk c)))
      (recv
        (enc nb0 (enc nb1 nc0 c a (pubk b)) (enc nc1 (pubk c))
          (pubk b))) (send (enc nc0 (enc nc1 (pubk c)) (pubk c)))))
  (label 13)
  (parent 11)
  (unrealized (2 2))
  (comment "1 in cohort - 1 not yet seen"))

(defskeleton nsl3
  (vars (na0 na1 nc0 nc1 nb0 nb1 nc0-0 nc1-0 text) (a b c name))
  (defstrand resp 3 (na0 na0) (na1 na1) (nb0 nb0) (nb1 nb1) (nc0 nc0)
    (nc1 nc1) (a a) (b b) (c c))
  (defstrand init 3 (na0 na0) (na1 na1) (nb0 nb0) (nb1 nb1) (nc0 nc0)
    (nc1 nc1) (a a) (b b) (c c))
  (defstrand mid 2 (na0 na0) (na1 na1) (nb0 nb0) (nb1 nb1) (a a) (b b)
    (c c))
  (defstrand resp 2 (na0 na0) (na1 na1) (nb0 nb0) (nb1 nb1) (nc0 nc0-0)
    (nc1 nc1-0) (a a) (b b) (c c))
  (precedes ((0 1) (1 1)) ((1 0) (2 0)) ((1 2) (0 2)) ((2 1) (3 0))
    ((3 1) (0 0)))
  (non-orig (privk a) (privk b) (privk c))
  (uniq-orig na0 na1 nc0 nc1 nb0 nb1 nc0-0 nc1-0)
  (operation nonce-test (contracted (nb0-0 nb0) (nb1-0 nb1)) na1 (0 0)
    (enc na0 a c (enc na1 a b (pubk c)) (pubk b))
    (enc na1 nc1-0 c b (enc na0 nb0 b c (pubk a))
      (enc nb1 nc0-0 c a (pubk b)) (pubk a))
    (enc nb1 b a (enc na1 a b (pubk c)) (enc na0 nb0 b c (pubk a))
      (pubk c)))
  (traces
    ((recv
       (enc nb1 b a (enc na1 a b (pubk c)) (enc na0 nb0 b c (pubk a))
         (pubk c)))
      (send
        (enc na1 nc1 c b (enc na0 nb0 b c (pubk a))
          (enc nb1 nc0 c a (pubk b)) (pubk a)))
      (recv (enc nc0 (enc nc1 (pubk c)) (pubk c))))
    ((send (enc na0 a c (enc na1 a b (pubk c)) (pubk b)))
      (recv
        (enc na1 nc1 c b (enc na0 nb0 b c (pubk a))
          (enc nb1 nc0 c a (pubk b)) (pubk a)))
      (send
        (enc nb0 (enc nb1 nc0 c a (pubk b)) (enc nc1 (pubk c))
          (pubk b))))
    ((recv (enc na0 a c (enc na1 a b (pubk c)) (pubk b)))
      (send
        (enc nb1 b a (enc na1 a b (pubk c)) (enc na0 nb0 b c (pubk a))
          (pubk c))))
    ((recv
       (enc nb1 b a (enc na1 a b (pubk c)) (enc na0 nb0 b c (pubk a))
         (pubk c)))
      (send
        (enc na1 nc1-0 c b (enc na0 nb0 b c (pubk a))
          (enc nb1 nc0-0 c a (pubk b)) (pubk a)))))
  (label 14)
  (parent 12)
  (unrealized (0 2))
  (comment "1 in cohort - 1 not yet seen"))

(defskeleton nsl3
  (vars (nc0 nc1 nb0 nb1 na0 na1 text) (a b c name))
  (defstrand resp 3 (na0 na0) (na1 na1) (nb0 nb0) (nb1 nb1) (nc0 nc0)
    (nc1 nc1) (a a) (b b) (c c))
  (defstrand init 3 (na0 na0) (na1 na1) (nb0 nb0) (nb1 nb1) (nc0 nc0)
    (nc1 nc1) (a a) (b b) (c c))
  (defstrand mid 4 (na0 na0) (na1 na1) (nb0 nb0) (nb1 nb1) (nc0 nc0)
    (nc1 nc1) (a a) (b b) (c c))
  (precedes ((0 1) (1 1)) ((1 0) (2 0)) ((1 2) (2 2)) ((2 1) (0 0))
    ((2 3) (0 2)))
  (non-orig (privk a) (privk b) (privk c))
  (uniq-orig nc0 nc1 nb0 nb1 na0 na1)
  (operation nonce-test (added-strand init 3) nc0 (2 2)
    (enc na1 nc1 c b (enc na0 nb0 b c (pubk a))
      (enc nb1 nc0 c a (pubk b)) (pubk a)))
  (traces
    ((recv
       (enc nb1 b a (enc na1 a b (pubk c)) (enc na0 nb0 b c (pubk a))
         (pubk c)))
      (send
        (enc na1 nc1 c b (enc na0 nb0 b c (pubk a))
          (enc nb1 nc0 c a (pubk b)) (pubk a)))
      (recv (enc nc0 (enc nc1 (pubk c)) (pubk c))))
    ((send (enc na0 a c (enc na1 a b (pubk c)) (pubk b)))
      (recv
        (enc na1 nc1 c b (enc na0 nb0 b c (pubk a))
          (enc nb1 nc0 c a (pubk b)) (pubk a)))
      (send
        (enc nb0 (enc nb1 nc0 c a (pubk b)) (enc nc1 (pubk c))
          (pubk b))))
    ((recv (enc na0 a c (enc na1 a b (pubk c)) (pubk b)))
      (send
        (enc nb1 b a (enc na1 a b (pubk c)) (enc na0 nb0 b c (pubk a))
          (pubk c)))
      (recv
        (enc nb0 (enc nb1 nc0 c a (pubk b)) (enc nc1 (pubk c))
          (pubk b))) (send (enc nc0 (enc nc1 (pubk c)) (pubk c)))))
  (label 15)
  (parent 13)
  (unrealized)
  (shape))

(defskeleton nsl3
  (vars (nc0 nc1 nb0 nb1 nc0-0 nc1-0 na0 na1 text) (a b c name))
  (defstrand resp 3 (na0 na0) (na1 na1) (nb0 nb0) (nb1 nb1) (nc0 nc0)
    (nc1 nc1) (a a) (b b) (c c))
  (defstrand init 3 (na0 na0) (na1 na1) (nb0 nb0) (nb1 nb1) (nc0 nc0)
    (nc1 nc1) (a a) (b b) (c c))
  (defstrand resp 2 (na0 na0) (na1 na1) (nb0 nb0) (nb1 nb1) (nc0 nc0-0)
    (nc1 nc1-0) (a a) (b b) (c c))
  (defstrand mid 4 (na0 na0) (na1 na1) (nb0 nb0) (nb1 nb1) (nc0 nc0)
    (nc1 nc1) (a a) (b b) (c c))
  (precedes ((0 1) (1 1)) ((0 1) (3 2)) ((1 0) (3 0)) ((1 2) (0 2))
    ((2 1) (0 0)) ((3 1) (2 0)) ((3 3) (0 2)))
  (non-orig (privk a) (privk b) (privk c))
  (uniq-orig nc0 nc1 nb0 nb1 nc0-0 nc1-0 na0 na1)
  (operation nonce-test (added-strand mid 4) nc0 (0 2)
    (enc nb0 (enc nb1 nc0 c a (pubk b)) (enc nc1 (pubk c)) (pubk b))
    (enc na1 nc1 c b (enc na0 nb0 b c (pubk a))
      (enc nb1 nc0 c a (pubk b)) (pubk a)))
  (traces
    ((recv
       (enc nb1 b a (enc na1 a b (pubk c)) (enc na0 nb0 b c (pubk a))
         (pubk c)))
      (send
        (enc na1 nc1 c b (enc na0 nb0 b c (pubk a))
          (enc nb1 nc0 c a (pubk b)) (pubk a)))
      (recv (enc nc0 (enc nc1 (pubk c)) (pubk c))))
    ((send (enc na0 a c (enc na1 a b (pubk c)) (pubk b)))
      (recv
        (enc na1 nc1 c b (enc na0 nb0 b c (pubk a))
          (enc nb1 nc0 c a (pubk b)) (pubk a)))
      (send
        (enc nb0 (enc nb1 nc0 c a (pubk b)) (enc nc1 (pubk c))
          (pubk b))))
    ((recv
       (enc nb1 b a (enc na1 a b (pubk c)) (enc na0 nb0 b c (pubk a))
         (pubk c)))
      (send
        (enc na1 nc1-0 c b (enc na0 nb0 b c (pubk a))
          (enc nb1 nc0-0 c a (pubk b)) (pubk a))))
    ((recv (enc na0 a c (enc na1 a b (pubk c)) (pubk b)))
      (send
        (enc nb1 b a (enc na1 a b (pubk c)) (enc na0 nb0 b c (pubk a))
          (pubk c)))
      (recv
        (enc nb0 (enc nb1 nc0 c a (pubk b)) (enc nc1 (pubk c))
          (pubk b))) (send (enc nc0 (enc nc1 (pubk c)) (pubk c)))))
  (label 16)
  (parent 14)
  (unrealized (3 2))
  (comment "1 in cohort - 1 not yet seen"))

(defskeleton nsl3
  (vars (nc0 nc1 nb0 nb1 nc0-0 nc1-0 na0 na1 text) (a b c name))
  (defstrand resp 3 (na0 na0) (na1 na1) (nb0 nb0) (nb1 nb1) (nc0 nc0)
    (nc1 nc1) (a a) (b b) (c c))
  (defstrand init 3 (na0 na0) (na1 na1) (nb0 nb0) (nb1 nb1) (nc0 nc0)
    (nc1 nc1) (a a) (b b) (c c))
  (defstrand resp 2 (na0 na0) (na1 na1) (nb0 nb0) (nb1 nb1) (nc0 nc0-0)
    (nc1 nc1-0) (a a) (b b) (c c))
  (defstrand mid 4 (na0 na0) (na1 na1) (nb0 nb0) (nb1 nb1) (nc0 nc0)
    (nc1 nc1) (a a) (b b) (c c))
  (precedes ((0 1) (1 1)) ((1 0) (3 0)) ((1 2) (3 2)) ((2 1) (0 0))
    ((3 1) (2 0)) ((3 3) (0 2)))
  (non-orig (privk a) (privk b) (privk c))
  (uniq-orig nc0 nc1 nb0 nb1 nc0-0 nc1-0 na0 na1)
  (operation nonce-test (added-strand init 3) nc0 (3 2)
    (enc na1 nc1 c b (enc na0 nb0 b c (pubk a))
      (enc nb1 nc0 c a (pubk b)) (pubk a)))
  (traces
    ((recv
       (enc nb1 b a (enc na1 a b (pubk c)) (enc na0 nb0 b c (pubk a))
         (pubk c)))
      (send
        (enc na1 nc1 c b (enc na0 nb0 b c (pubk a))
          (enc nb1 nc0 c a (pubk b)) (pubk a)))
      (recv (enc nc0 (enc nc1 (pubk c)) (pubk c))))
    ((send (enc na0 a c (enc na1 a b (pubk c)) (pubk b)))
      (recv
        (enc na1 nc1 c b (enc na0 nb0 b c (pubk a))
          (enc nb1 nc0 c a (pubk b)) (pubk a)))
      (send
        (enc nb0 (enc nb1 nc0 c a (pubk b)) (enc nc1 (pubk c))
          (pubk b))))
    ((recv
       (enc nb1 b a (enc na1 a b (pubk c)) (enc na0 nb0 b c (pubk a))
         (pubk c)))
      (send
        (enc na1 nc1-0 c b (enc na0 nb0 b c (pubk a))
          (enc nb1 nc0-0 c a (pubk b)) (pubk a))))
    ((recv (enc na0 a c (enc na1 a b (pubk c)) (pubk b)))
      (send
        (enc nb1 b a (enc na1 a b (pubk c)) (enc na0 nb0 b c (pubk a))
          (pubk c)))
      (recv
        (enc nb0 (enc nb1 nc0 c a (pubk b)) (enc nc1 (pubk c))
          (pubk b))) (send (enc nc0 (enc nc1 (pubk c)) (pubk c)))))
  (label 17)
  (parent 16)
  (seen 15)
  (unrealized)
  (comment "1 in cohort - 0 not yet seen"))

(comment "Nothing left to do")
