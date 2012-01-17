(comment "CPSA 2.1.0")
(comment "All input read")

(defprotocol tt6 basic
  (defrole init
    (vars (a name) (n text))
    (trace (send (enc n (pubk a)))
      (recv
        (cat (enc n (enc n (enc n (pubk a)) (pubk a)) (pubk a))
          (enc n (enc n (pubk a)) (pubk a)))))
    (non-orig (privk a))
    (uniq-orig n))
  (defrole trans
    (vars (a name) (n text) (m mesg))
    (trace (recv (enc n (pubk a))) (recv m) (send (enc n m (pubk a))))))

(defskeleton tt6
  (vars (n text) (a name))
  (defstrand init 2 (n n) (a a))
  (non-orig (privk a))
  (uniq-orig n)
  (traces
    ((send (enc n (pubk a)))
      (recv
        (cat (enc n (enc n (enc n (pubk a)) (pubk a)) (pubk a))
          (enc n (enc n (pubk a)) (pubk a))))))
  (label 0)
  (unrealized (0 1))
  (comment "1 in cohort - 1 not yet seen"))

(defskeleton tt6
  (vars (m mesg) (n text) (a name))
  (defstrand init 2 (n n) (a a))
  (defstrand trans 3 (m m) (n n) (a a))
  (precedes ((0 0) (1 0)) ((1 2) (0 1)))
  (non-orig (privk a))
  (uniq-orig n)
  (operation nonce-test (added-strand trans 3) n (0 1) (enc n (pubk a)))
  (traces
    ((send (enc n (pubk a)))
      (recv
        (cat (enc n (enc n (enc n (pubk a)) (pubk a)) (pubk a))
          (enc n (enc n (pubk a)) (pubk a)))))
    ((recv (enc n (pubk a))) (recv m) (send (enc n m (pubk a)))))
  (label 1)
  (parent 0)
  (unrealized (0 1))
  (comment "1 in cohort - 1 not yet seen"))

(defskeleton tt6
  (vars (n text) (a name))
  (defstrand init 2 (n n) (a a))
  (defstrand trans 3 (m (enc n (pubk a))) (n n) (a a))
  (precedes ((0 0) (1 0)) ((1 2) (0 1)))
  (non-orig (privk a))
  (uniq-orig n)
  (operation nonce-test (contracted (m (enc n (pubk a)))) n (0 1)
    (enc n (pubk a)) (enc n (enc n (pubk a)) (pubk a)))
  (traces
    ((send (enc n (pubk a)))
      (recv
        (cat (enc n (enc n (enc n (pubk a)) (pubk a)) (pubk a))
          (enc n (enc n (pubk a)) (pubk a)))))
    ((recv (enc n (pubk a))) (recv (enc n (pubk a)))
      (send (enc n (enc n (pubk a)) (pubk a)))))
  (label 2)
  (parent 1)
  (unrealized (0 1))
  (comment "1 in cohort - 1 not yet seen"))

(defskeleton tt6
  (vars (m mesg) (n text) (a name))
  (defstrand init 2 (n n) (a a))
  (defstrand trans 3 (m (enc n (pubk a))) (n n) (a a))
  (defstrand trans 3 (m m) (n n) (a a))
  (precedes ((0 0) (1 0)) ((0 0) (2 0)) ((1 2) (0 1)) ((2 2) (0 1)))
  (non-orig (privk a))
  (uniq-orig n)
  (operation nonce-test (added-strand trans 3) n (0 1) (enc n (pubk a))
    (enc n (enc n (pubk a)) (pubk a)))
  (traces
    ((send (enc n (pubk a)))
      (recv
        (cat (enc n (enc n (enc n (pubk a)) (pubk a)) (pubk a))
          (enc n (enc n (pubk a)) (pubk a)))))
    ((recv (enc n (pubk a))) (recv (enc n (pubk a)))
      (send (enc n (enc n (pubk a)) (pubk a))))
    ((recv (enc n (pubk a))) (recv m) (send (enc n m (pubk a)))))
  (label 3)
  (parent 2)
  (unrealized (0 1))
  (comment "1 in cohort - 1 not yet seen"))

(defskeleton tt6
  (vars (n text) (a name))
  (defstrand init 2 (n n) (a a))
  (defstrand trans 3 (m (enc n (pubk a))) (n n) (a a))
  (defstrand trans 3 (m (enc n (enc n (pubk a)) (pubk a))) (n n) (a a))
  (precedes ((0 0) (1 0)) ((0 0) (2 0)) ((1 2) (0 1)) ((2 2) (0 1)))
  (non-orig (privk a))
  (uniq-orig n)
  (operation nonce-test
    (contracted (m (enc n (enc n (pubk a)) (pubk a)))) n (0 1)
    (enc n (pubk a)) (enc n (enc n (pubk a)) (pubk a))
    (enc n (enc n (enc n (pubk a)) (pubk a)) (pubk a)))
  (traces
    ((send (enc n (pubk a)))
      (recv
        (cat (enc n (enc n (enc n (pubk a)) (pubk a)) (pubk a))
          (enc n (enc n (pubk a)) (pubk a)))))
    ((recv (enc n (pubk a))) (recv (enc n (pubk a)))
      (send (enc n (enc n (pubk a)) (pubk a))))
    ((recv (enc n (pubk a))) (recv (enc n (enc n (pubk a)) (pubk a)))
      (send (enc n (enc n (enc n (pubk a)) (pubk a)) (pubk a)))))
  (label 4)
  (parent 3)
  (unrealized (2 1))
  (comment "1 in cohort - 1 not yet seen"))

(defskeleton tt6
  (vars (m mesg) (n text) (a name))
  (defstrand init 2 (n n) (a a))
  (defstrand trans 3 (m (enc n (pubk a))) (n n) (a a))
  (defstrand trans 3 (m (enc n (enc n (pubk a)) (pubk a))) (n n) (a a))
  (defstrand trans 3 (m m) (n n) (a a))
  (precedes ((0 0) (1 0)) ((0 0) (2 0)) ((0 0) (3 0)) ((1 2) (0 1))
    ((2 2) (0 1)) ((3 2) (2 1)))
  (non-orig (privk a))
  (uniq-orig n)
  (operation nonce-test (added-strand trans 3) n (2 1) (enc n (pubk a)))
  (traces
    ((send (enc n (pubk a)))
      (recv
        (cat (enc n (enc n (enc n (pubk a)) (pubk a)) (pubk a))
          (enc n (enc n (pubk a)) (pubk a)))))
    ((recv (enc n (pubk a))) (recv (enc n (pubk a)))
      (send (enc n (enc n (pubk a)) (pubk a))))
    ((recv (enc n (pubk a))) (recv (enc n (enc n (pubk a)) (pubk a)))
      (send (enc n (enc n (enc n (pubk a)) (pubk a)) (pubk a))))
    ((recv (enc n (pubk a))) (recv m) (send (enc n m (pubk a)))))
  (label 5)
  (parent 4)
  (unrealized (2 1))
  (comment "1 in cohort - 1 not yet seen"))

(defskeleton tt6
  (vars (n text) (a name))
  (defstrand init 2 (n n) (a a))
  (defstrand trans 3 (m (enc n (enc n (pubk a)) (pubk a))) (n n) (a a))
  (defstrand trans 3 (m (enc n (pubk a))) (n n) (a a))
  (precedes ((0 0) (1 0)) ((0 0) (2 0)) ((1 2) (0 1)) ((2 2) (1 1)))
  (non-orig (privk a))
  (uniq-orig n)
  (operation nonce-test (contracted (m (enc n (pubk a)))) n (2 1)
    (enc n (pubk a)) (enc n (enc n (pubk a)) (pubk a)))
  (traces
    ((send (enc n (pubk a)))
      (recv
        (cat (enc n (enc n (enc n (pubk a)) (pubk a)) (pubk a))
          (enc n (enc n (pubk a)) (pubk a)))))
    ((recv (enc n (pubk a))) (recv (enc n (enc n (pubk a)) (pubk a)))
      (send (enc n (enc n (enc n (pubk a)) (pubk a)) (pubk a))))
    ((recv (enc n (pubk a))) (recv (enc n (pubk a)))
      (send (enc n (enc n (pubk a)) (pubk a)))))
  (label 6)
  (parent 5)
  (unrealized)
  (shape))

(comment "Nothing left to do")

(defprotocol tt6 basic
  (defrole init
    (vars (a name) (n text))
    (trace (send (enc n (pubk a)))
      (recv
        (cat (enc n (enc n (enc n (pubk a)) (pubk a)) (pubk a))
          (enc n (enc n (pubk a)) (pubk a)))))
    (non-orig (privk a))
    (uniq-orig n))
  (defrole trans
    (vars (a name) (n text) (m mesg))
    (trace (recv (enc n (pubk a))) (recv m) (send (enc n m (pubk a))))))

(defskeleton tt6
  (vars (n text) (a name))
  (defstrand init 2 (n n) (a a))
  (defstrand trans 3 (m (enc n (pubk a))) (n n) (a a))
  (defstrand trans 3 (m (enc n (enc n (pubk a)) (pubk a))) (n n) (a a))
  (precedes ((0 0) (1 0)) ((0 0) (2 0)) ((1 2) (2 1)) ((2 2) (0 1)))
  (non-orig (privk a))
  (uniq-orig n)
  (traces
    ((send (enc n (pubk a)))
      (recv
        (cat (enc n (enc n (enc n (pubk a)) (pubk a)) (pubk a))
          (enc n (enc n (pubk a)) (pubk a)))))
    ((recv (enc n (pubk a))) (recv (enc n (pubk a)))
      (send (enc n (enc n (pubk a)) (pubk a))))
    ((recv (enc n (pubk a))) (recv (enc n (enc n (pubk a)) (pubk a)))
      (send (enc n (enc n (enc n (pubk a)) (pubk a)) (pubk a)))))
  (label 7)
  (unrealized)
  (shape))

(comment "Nothing left to do")
