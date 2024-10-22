(include-book "arithmetic-3/top" :dir :system)

(defun prime-factors-r (n i)
  (declare (xargs :mode :program))
  (cond ((or (zp n) (zp (- n i)) (zp i) (< i 2) (< n 2))
         (list n))
        ((= (mod n i) 0)
         (cons i (prime-factors-r (floor n i) 2)))
        (t (prime-factors-r n (+ 1 i)))))

(defun prime-factors (n)
  (declare (xargs :mode :program))
  (prime-factors-r n 2))
