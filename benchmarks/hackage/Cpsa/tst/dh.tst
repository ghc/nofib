(comment "CPSA 2.1.0")
(comment "All input read")

(defprotocol dhke diffie-hellman
  (defrole init
    (vars (a b name) (g base) (m expn))
    (trace (send (cat a (enc "i" (exp (gen) m) (privk a))))
      (recv (cat b (enc g (privk b)) (enc "r" a b (exp g m))))
      (send (enc "i" a b (exp g m))))
    (uniq-orig (exp (gen) m)))
  (defrole resp
    (vars (a b name) (h base) (n expn))
    (trace (recv (cat a (enc "i" h (privk a))))
      (send
        (cat b (enc (exp (gen) n) (privk b)) (enc "r" a b (exp h n))))
      (recv (enc "i" a b (exp h n))))
    (uniq-orig (exp (gen) n))))

(defskeleton dhke
  (vars (a b name) (h base) (n expn))
  (defstrand resp 3 (a a) (b b) (h h) (n n))
  (non-orig (exp h n) (privk a) (privk b))
  (uniq-orig (exp (gen) n))
  (traces
    ((recv (cat a (enc "i" h (privk a))))
      (send
        (cat b (enc (exp (gen) n) (privk b)) (enc "r" a b (exp h n))))
      (recv (enc "i" a b (exp h n)))))
  (label 0)
  (unrealized (0 0) (0 2))
  (comment "1 in cohort - 1 not yet seen"))

(defskeleton dhke
  (vars (a b name) (n m expn))
  (defstrand resp 3 (a a) (b b) (h (exp (gen) m)) (n n))
  (defstrand init 1 (a a) (m m))
  (precedes ((1 0) (0 0)))
  (non-orig (exp (gen) (mul n m)) (privk a) (privk b))
  (uniq-orig (exp (gen) n) (exp (gen) m))
  (operation encryption-test (added-strand init 1)
    (enc "i" (exp (gen) m) (privk a)) (0 0))
  (traces
    ((recv (cat a (enc "i" (exp (gen) m) (privk a))))
      (send
        (cat b (enc (exp (gen) n) (privk b))
          (enc "r" a b (exp (gen) (mul n m)))))
      (recv (enc "i" a b (exp (gen) (mul n m)))))
    ((send (cat a (enc "i" (exp (gen) m) (privk a))))))
  (label 1)
  (parent 0)
  (unrealized (0 2))
  (comment "1 in cohort - 1 not yet seen"))

(defskeleton dhke
  (vars (a b name) (n m m-0 expn))
  (defstrand resp 3 (a a) (b b) (h (exp (gen) m)) (n n))
  (defstrand init 1 (a a) (m m))
  (defstrand init 3 (a a) (b b) (g (exp (gen) (mul n m (rec m-0))))
    (m m-0))
  (precedes ((1 0) (0 0)) ((2 2) (0 2)))
  (non-orig (exp (gen) (mul n m)) (privk a) (privk b))
  (uniq-orig (exp (gen) n) (exp (gen) m) (exp (gen) m-0))
  (operation encryption-test (added-strand init 3)
    (enc "i" a b (exp (gen) (mul n m))) (0 2))
  (traces
    ((recv (cat a (enc "i" (exp (gen) m) (privk a))))
      (send
        (cat b (enc (exp (gen) n) (privk b))
          (enc "r" a b (exp (gen) (mul n m)))))
      (recv (enc "i" a b (exp (gen) (mul n m)))))
    ((send (cat a (enc "i" (exp (gen) m) (privk a)))))
    ((send (cat a (enc "i" (exp (gen) m-0) (privk a))))
      (recv
        (cat b (enc (exp (gen) (mul n m (rec m-0))) (privk b))
          (enc "r" a b (exp (gen) (mul n m)))))
      (send (enc "i" a b (exp (gen) (mul n m))))))
  (label 2)
  (parent 1)
  (unrealized (2 1))
  (comment "1 in cohort - 1 not yet seen"))

(defskeleton dhke
  (vars (a b a-0 name) (h base) (n m m-0 expn))
  (defstrand resp 3 (a a) (b b) (h (exp (gen) m-0))
    (n (mul n m (rec m-0))))
  (defstrand init 1 (a a) (m m-0))
  (defstrand init 3 (a a) (b b) (g (exp (gen) n)) (m m))
  (defstrand resp 2 (a a-0) (b b) (h h) (n n))
  (precedes ((1 0) (0 0)) ((2 2) (0 2)) ((3 1) (2 1)))
  (non-orig (exp (gen) (mul n m)) (privk a) (privk b))
  (uniq-orig (exp (gen) n) (exp (gen) (mul n m (rec m-0))) (exp (gen) m)
    (exp (gen) m-0))
  (operation encryption-test (added-strand resp 2)
    (enc (exp (gen) n) (privk b)) (2 1))
  (traces
    ((recv (cat a (enc "i" (exp (gen) m-0) (privk a))))
      (send
        (cat b (enc (exp (gen) (mul n m (rec m-0))) (privk b))
          (enc "r" a b (exp (gen) (mul n m)))))
      (recv (enc "i" a b (exp (gen) (mul n m)))))
    ((send (cat a (enc "i" (exp (gen) m-0) (privk a)))))
    ((send (cat a (enc "i" (exp (gen) m) (privk a))))
      (recv
        (cat b (enc (exp (gen) n) (privk b))
          (enc "r" a b (exp (gen) (mul n m)))))
      (send (enc "i" a b (exp (gen) (mul n m)))))
    ((recv (cat a-0 (enc "i" h (privk a-0))))
      (send
        (cat b (enc (exp (gen) n) (privk b))
          (enc "r" a-0 b (exp h n))))))
  (label 3)
  (parent 2)
  (unrealized (2 1))
  (comment "1 in cohort - 1 not yet seen"))

(defskeleton dhke
  (vars (a b a-0 name) (h base) (n m m-0 n-0 expn))
  (defstrand resp 3 (a a) (b b) (h (exp (gen) m-0))
    (n (mul n m (rec m-0))))
  (defstrand init 1 (a a) (m m-0))
  (defstrand init 3 (a a) (b b) (g (exp (gen) n)) (m m))
  (defstrand resp 2 (a a-0) (b b) (h h) (n n))
  (defstrand resp 2 (a a) (b b) (h (exp (gen) (mul n m (rec n-0))))
    (n n-0))
  (precedes ((1 0) (0 0)) ((2 2) (0 2)) ((3 1) (2 1)) ((4 1) (2 1)))
  (non-orig (exp (gen) (mul n m)) (privk a) (privk b))
  (uniq-orig (exp (gen) n) (exp (gen) (mul n m (rec m-0))) (exp (gen) m)
    (exp (gen) m-0) (exp (gen) n-0))
  (operation encryption-test (added-strand resp 2)
    (enc "r" a b (exp (gen) (mul n m))) (2 1))
  (traces
    ((recv (cat a (enc "i" (exp (gen) m-0) (privk a))))
      (send
        (cat b (enc (exp (gen) (mul n m (rec m-0))) (privk b))
          (enc "r" a b (exp (gen) (mul n m)))))
      (recv (enc "i" a b (exp (gen) (mul n m)))))
    ((send (cat a (enc "i" (exp (gen) m-0) (privk a)))))
    ((send (cat a (enc "i" (exp (gen) m) (privk a))))
      (recv
        (cat b (enc (exp (gen) n) (privk b))
          (enc "r" a b (exp (gen) (mul n m)))))
      (send (enc "i" a b (exp (gen) (mul n m)))))
    ((recv (cat a-0 (enc "i" h (privk a-0))))
      (send
        (cat b (enc (exp (gen) n) (privk b))
          (enc "r" a-0 b (exp h n)))))
    ((recv (cat a (enc "i" (exp (gen) (mul n m (rec n-0))) (privk a))))
      (send
        (cat b (enc (exp (gen) n-0) (privk b))
          (enc "r" a b (exp (gen) (mul n m)))))))
  (label 4)
  (parent 3)
  (unrealized (4 0))
  (comment "1 in cohort - 1 not yet seen"))

(defskeleton dhke
  (vars (a b a-0 name) (h base) (m n m-0 n-0 expn))
  (defstrand resp 3 (a a) (b b) (h (exp (gen) m))
    (n (mul (rec m) n m-0)))
  (defstrand init 1 (a a) (m m))
  (defstrand init 3 (a a) (b b) (g (exp (gen) (mul n m-0 (rec n-0))))
    (m n-0))
  (defstrand resp 2 (a a-0) (b b) (h h) (n (mul n m-0 (rec n-0))))
  (defstrand resp 2 (a a) (b b) (h (exp (gen) n)) (n m-0))
  (defstrand init 1 (a a) (m n))
  (precedes ((1 0) (0 0)) ((2 2) (0 2)) ((3 1) (2 1)) ((4 1) (2 1))
    ((5 0) (4 0)))
  (non-orig (exp (gen) (mul n m-0)) (privk a) (privk b))
  (uniq-orig (exp (gen) (mul (rec m) n m-0)) (exp (gen) m) (exp (gen) n)
    (exp (gen) (mul n m-0 (rec n-0))) (exp (gen) m-0) (exp (gen) n-0))
  (operation encryption-test (added-strand init 1)
    (enc "i" (exp (gen) n) (privk a)) (4 0))
  (traces
    ((recv (cat a (enc "i" (exp (gen) m) (privk a))))
      (send
        (cat b (enc (exp (gen) (mul (rec m) n m-0)) (privk b))
          (enc "r" a b (exp (gen) (mul n m-0)))))
      (recv (enc "i" a b (exp (gen) (mul n m-0)))))
    ((send (cat a (enc "i" (exp (gen) m) (privk a)))))
    ((send (cat a (enc "i" (exp (gen) n-0) (privk a))))
      (recv
        (cat b (enc (exp (gen) (mul n m-0 (rec n-0))) (privk b))
          (enc "r" a b (exp (gen) (mul n m-0)))))
      (send (enc "i" a b (exp (gen) (mul n m-0)))))
    ((recv (cat a-0 (enc "i" h (privk a-0))))
      (send
        (cat b (enc (exp (gen) (mul n m-0 (rec n-0))) (privk b))
          (enc "r" a-0 b (exp h (mul n m-0 (rec n-0)))))))
    ((recv (cat a (enc "i" (exp (gen) n) (privk a))))
      (send
        (cat b (enc (exp (gen) m-0) (privk b))
          (enc "r" a b (exp (gen) (mul n m-0))))))
    ((send (cat a (enc "i" (exp (gen) n) (privk a))))))
  (label 5)
  (parent 4)
  (seen 6 7 8)
  (unrealized)
  (shape)
  (comment "6 in cohort - 3 not yet seen"))

(defskeleton dhke
  (vars (a b name) (n m m-0 expn))
  (defstrand resp 3 (a a) (b b) (h (exp (gen) m-0))
    (n (mul n m (rec m-0))))
  (defstrand init 3 (a a) (b b) (g (exp (gen) (mul n m (rec m-0))))
    (m m-0))
  (defstrand resp 2 (a a) (b b) (h (exp (gen) n)) (n m))
  (defstrand init 1 (a a) (m n))
  (precedes ((0 1) (1 1)) ((1 0) (0 0)) ((1 2) (0 2)) ((2 1) (1 1))
    ((3 0) (2 0)))
  (non-orig (exp (gen) (mul n m)) (privk a) (privk b))
  (uniq-orig (exp (gen) n) (exp (gen) (mul n m (rec m-0))) (exp (gen) m)
    (exp (gen) m-0))
  (operation collapsed 1 2)
  (traces
    ((recv (cat a (enc "i" (exp (gen) m-0) (privk a))))
      (send
        (cat b (enc (exp (gen) (mul n m (rec m-0))) (privk b))
          (enc "r" a b (exp (gen) (mul n m)))))
      (recv (enc "i" a b (exp (gen) (mul n m)))))
    ((send (cat a (enc "i" (exp (gen) m-0) (privk a))))
      (recv
        (cat b (enc (exp (gen) (mul n m (rec m-0))) (privk b))
          (enc "r" a b (exp (gen) (mul n m)))))
      (send (enc "i" a b (exp (gen) (mul n m)))))
    ((recv (cat a (enc "i" (exp (gen) n) (privk a))))
      (send
        (cat b (enc (exp (gen) m) (privk b))
          (enc "r" a b (exp (gen) (mul n m))))))
    ((send (cat a (enc "i" (exp (gen) n) (privk a))))))
  (label 6)
  (parent 5)
  (seen 9)
  (unrealized)
  (shape)
  (comment "2 in cohort - 1 not yet seen"))

(defskeleton dhke
  (vars (a b a-0 name) (h base) (m n m-0 expn))
  (defstrand resp 3 (a a) (b b) (h (exp (gen) m-0)) (n m))
  (defstrand init 1 (a a) (m m-0))
  (defstrand init 3 (a a) (b b) (g (exp (gen) (mul m (rec n) m-0)))
    (m n))
  (defstrand resp 2 (a a-0) (b b) (h h) (n (mul m (rec n) m-0)))
  (precedes ((0 1) (2 1)) ((1 0) (0 0)) ((2 2) (0 2)) ((3 1) (2 1)))
  (non-orig (exp (gen) (mul m m-0)) (privk a) (privk b))
  (uniq-orig (exp (gen) m) (exp (gen) (mul m (rec n) m-0)) (exp (gen) n)
    (exp (gen) m-0))
  (operation collapsed 4 0)
  (traces
    ((recv (cat a (enc "i" (exp (gen) m-0) (privk a))))
      (send
        (cat b (enc (exp (gen) m) (privk b))
          (enc "r" a b (exp (gen) (mul m m-0)))))
      (recv (enc "i" a b (exp (gen) (mul m m-0)))))
    ((send (cat a (enc "i" (exp (gen) m-0) (privk a)))))
    ((send (cat a (enc "i" (exp (gen) n) (privk a))))
      (recv
        (cat b (enc (exp (gen) (mul m (rec n) m-0)) (privk b))
          (enc "r" a b (exp (gen) (mul m m-0)))))
      (send (enc "i" a b (exp (gen) (mul m m-0)))))
    ((recv (cat a-0 (enc "i" h (privk a-0))))
      (send
        (cat b (enc (exp (gen) (mul m (rec n) m-0)) (privk b))
          (enc "r" a-0 b (exp h (mul m (rec n) m-0)))))))
  (label 7)
  (parent 5)
  (seen 9)
  (unrealized)
  (shape)
  (comment "2 in cohort - 0 not yet seen"))

(defskeleton dhke
  (vars (b a name) (m m-0 n expn))
  (defstrand resp 3 (a a) (b b) (h (exp (gen) m))
    (n (mul (rec m) m-0 n)))
  (defstrand init 1 (a a) (m m))
  (defstrand init 3 (a a) (b b) (g (exp (gen) m-0)) (m n))
  (defstrand resp 2 (a a) (b b) (h (exp (gen) n)) (n m-0))
  (precedes ((1 0) (0 0)) ((2 0) (3 0)) ((2 2) (0 2)) ((3 1) (2 1)))
  (non-orig (exp (gen) (mul m-0 n)) (privk b) (privk a))
  (uniq-orig (exp (gen) (mul (rec m) m-0 n)) (exp (gen) m)
    (exp (gen) m-0) (exp (gen) n))
  (operation collapsed 4 3)
  (traces
    ((recv (cat a (enc "i" (exp (gen) m) (privk a))))
      (send
        (cat b (enc (exp (gen) (mul (rec m) m-0 n)) (privk b))
          (enc "r" a b (exp (gen) (mul m-0 n)))))
      (recv (enc "i" a b (exp (gen) (mul m-0 n)))))
    ((send (cat a (enc "i" (exp (gen) m) (privk a)))))
    ((send (cat a (enc "i" (exp (gen) n) (privk a))))
      (recv
        (cat b (enc (exp (gen) m-0) (privk b))
          (enc "r" a b (exp (gen) (mul m-0 n)))))
      (send (enc "i" a b (exp (gen) (mul m-0 n)))))
    ((recv (cat a (enc "i" (exp (gen) n) (privk a))))
      (send
        (cat b (enc (exp (gen) m-0) (privk b))
          (enc "r" a b (exp (gen) (mul m-0 n)))))))
  (label 8)
  (parent 5)
  (seen 9)
  (unrealized)
  (shape)
  (comment "2 in cohort - 0 not yet seen"))

(defskeleton dhke
  (vars (a b name) (m n expn))
  (defstrand resp 3 (a a) (b b) (h (exp (gen) n)) (n m))
  (defstrand init 3 (a a) (b b) (g (exp (gen) m)) (m n))
  (precedes ((0 1) (1 1)) ((1 0) (0 0)) ((1 2) (0 2)))
  (non-orig (exp (gen) (mul m n)) (privk a) (privk b))
  (uniq-orig (exp (gen) m) (exp (gen) n))
  (operation collapsed 2 0)
  (traces
    ((recv (cat a (enc "i" (exp (gen) n) (privk a))))
      (send
        (cat b (enc (exp (gen) m) (privk b))
          (enc "r" a b (exp (gen) (mul m n)))))
      (recv (enc "i" a b (exp (gen) (mul m n)))))
    ((send (cat a (enc "i" (exp (gen) n) (privk a))))
      (recv
        (cat b (enc (exp (gen) m) (privk b))
          (enc "r" a b (exp (gen) (mul m n)))))
      (send (enc "i" a b (exp (gen) (mul m n))))))
  (label 9)
  (parent 6)
  (unrealized)
  (shape))

(comment "Nothing left to do")

(defprotocol dhke diffie-hellman
  (defrole init
    (vars (a b name) (g base) (m expn))
    (trace (send (cat a (enc "i" (exp (gen) m) (privk a))))
      (recv (cat b (enc g (privk b)) (enc "r" a b (exp g m))))
      (send (enc "i" a b (exp g m))))
    (uniq-orig (exp (gen) m)))
  (defrole resp
    (vars (a b name) (h base) (n expn))
    (trace (recv (cat a (enc "i" h (privk a))))
      (send
        (cat b (enc (exp (gen) n) (privk b)) (enc "r" a b (exp h n))))
      (recv (enc "i" a b (exp h n))))
    (uniq-orig (exp (gen) n))))

(defskeleton dhke
  (vars (a b name) (h base) (n expn))
  (defstrand resp 3 (a a) (b b) (h h) (n n))
  (non-orig (privk a) (privk b))
  (uniq-orig (exp (gen) n))
  (traces
    ((recv (cat a (enc "i" h (privk a))))
      (send
        (cat b (enc (exp (gen) n) (privk b)) (enc "r" a b (exp h n))))
      (recv (enc "i" a b (exp h n)))))
  (label 10)
  (unrealized (0 0))
  (comment "1 in cohort - 1 not yet seen"))

(defskeleton dhke
  (vars (a b name) (n m expn))
  (defstrand resp 3 (a a) (b b) (h (exp (gen) m)) (n n))
  (defstrand init 1 (a a) (m m))
  (precedes ((1 0) (0 0)))
  (non-orig (privk a) (privk b))
  (uniq-orig (exp (gen) n) (exp (gen) m))
  (operation encryption-test (added-strand init 1)
    (enc "i" (exp (gen) m) (privk a)) (0 0))
  (traces
    ((recv (cat a (enc "i" (exp (gen) m) (privk a))))
      (send
        (cat b (enc (exp (gen) n) (privk b))
          (enc "r" a b (exp (gen) (mul n m)))))
      (recv (enc "i" a b (exp (gen) (mul n m)))))
    ((send (cat a (enc "i" (exp (gen) m) (privk a))))))
  (label 11)
  (parent 10)
  (unrealized)
  (shape))

(comment "Nothing left to do")
