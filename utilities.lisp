;;; -*- Mode: Lisp -*-

(in-package :blitz.math.project-euler)

(declaim (inline dividesp))

(defun dividesp (number divisor)
  "Tests whether `divisor' divides `number'."
  (zerop (mod number divisor)))

(defun factorize (n &optional (found nil))
  "Returns a list of all prime factors of `n', e.g. (factorize 20)
=> (5 2 2). This function uses a very primitive form of trial
division, so don't expect it to scale."
  (assert (> n 0))
  (cond
    ((= 1 n) found)
    (t (loop
	  for trial upfrom 2
	  until (dividesp n trial)
	  finally (return
		    (factorize (truncate n trial)
			       (cons trial found)))))))

(defun map-primes (fn)
  "Call `fn' with all prime numbers (starting with 2). It uses a Sieve
of Eratostenes-like algorithm."
  (funcall fn 2)
  (iter (with primes = (list 2))
	(with tail = primes)
	(for candidate upfrom 3 by 2)
	(declare (fixnum candidate))
	(when (iter (for trial in primes)
		    (declare (fixnum trial))
		    (never (dividesp candidate trial)))
	  (funcall fn candidate)
	  (let ((new-tail (cons candidate nil)))
	    (setf (cdr tail) new-tail
		  tail new-tail)))))

(defun map-permutations (set fn)
  "Call `fn' with all permutations of `set' (a list). `fn' will be
called with a function taking no arguments and returning the
permutation when called."
  (iter (for val in set)
	(let ((reduced (remove val set)))
	  (if (endp reduced)
	      (funcall fn (lambda () set))
	      (map-permutations reduced
				(lambda (perm)
				  (funcall fn
					   (lambda ()
					     (cons val (funcall perm))))))))))

(defun map-fibonacci (fn)
  "Call `fn' with all Fibonacci numbers in ascending order."
  (let ((a 1)
	(b 1))
    (loop (funcall fn a)
       (psetf a b
	      b (+ a b)))))

(defun palindromp (str)
  "Tests whether `str' (a string) is a palindrom."
  (declare (optimize speed)
           (type string str))
  (iter (with length = (length str))
	(for i from 0 to (truncate length 2))
	(always (char= (char str i)
		       (char str (- length i 1))))))

(defun exptmod (b e m)
  "Calculates b^e mod m in a efficient way."
  (if (zerop e) 
      1
      (mod (if (evenp e)
	       (expt (exptmod b (/ e 2) m) 2)
	       (* b (exptmod b (1- e) m)))
	   m)))


;;; EOF
