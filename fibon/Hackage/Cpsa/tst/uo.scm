(defprotocol uniq-orig basic
  (defrole init
    (vars (n text))
    (trace (send n))
    (uniq-orig n))
  (defrole resp
    (vars (m n text))
    (trace
     (send (enc m n))
     (recv n))))

(defskeleton uniq-orig
  (vars (n text))
  (defstrand init 1 (n n))
  (defstrand resp 2 (n n)))
