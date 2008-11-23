;;; -*- Mode: Lisp -*-

(in-package :blitz.math.project-euler)

;;; Problem 3


(defun problem-3 ()
  (let ((factors (factorize 317584931803)))
    (first (sort factors #'>=))))

;;; Problem 4

(defun 3-digit-reverse (n)
  (declare (type (integer 0 999) n))
  (+ (truncate n 100)
     (* 10 (mod (truncate n 10) 10))
     (* 100 (mod n 10))))

(defun 3-digit-palindrom-p (n)
  (declare (fixnum n))
  (multiple-value-bind (part1 part2)
      (truncate n 1000)
    (= part1 (3-digit-reverse part2))))

(defun problem-4 ()
  (loop
     for i from 99 to 999
     nconcing (loop
		 for j from i to 999
		 when (3-digit-palindrom-p (* i j))
		 collect (* i j))))



;;; Problem 5

(defun problem-5 ()
  (reduce #'lcm (loop for i from 2 to 20
		   collect i)))

;;; Problem 6

(defun problem-6 ()
  (iter (for i from 1 to 100)
	(sum i into normal-sum)
	(sum (* i i) into squared-sum)
	(finally (return (- (expt normal-sum 2) squared-sum)))))

;;; Problem 7

(defun nth-prime (n)
  (assert (> n 0))
  (iter (with primes = nil)
	(with found = 0)
	(for candidate upfrom 2)
	(when (notany (lambda (p)
			(dividesp candidate p))
		      primes)
	  (when (= (incf found) n)
	    (return candidate))
	  (push candidate primes)
	  (unless (= candidate 2)
	    (incf candidate)))))

(defun problem-7 ()
  (nth-prime 10001))

;;; Problem 8

(defparameter *problem-8-digits*
  "73167176531330624919225119674426574742355349194934
   96983520312774506326239578318016984801869478851843
   85861560789112949495459501737958331952853208805511
   12540698747158523863050715693290963295227443043557
   66896648950445244523161731856403098711121722383113
   62229893423380308135336276614282806444486645238749
   30358907296290491560440772390713810515859307960866
   70172427121883998797908792274921901699720888093776
   65727333001053367881220235421809751254540594752243
   52584907711670556013604839586446706324415722155397
   53697817977846174064955149290862569321978468622482
   83972241375657056057490261407972968652414535100474
   82166370484403199890008895243450658541227588666881
   16427171479924442928230863465674813919123162824586
   17866458359124566529476545682848912883142607690042
   24219022671055626321111109370544217506941658960408
   07198403850962455444362981230987879927244284909188
   84580156166097919133875499200524063689912560717606
   05886116467109405077541002256983155200055935729725
   71636269561882670428252483600823257530420752963450")

(defun problem-8 ()
  (let ((digits (remove-if-not #'digit-char-p *problem-8-digits*)))
    (iter (for pos from 0 to (- (length digits) 5))
	  (maximizing (iter (for i from pos to (+ pos 4))
			    (multiply (digit-char-p (char digits i))))))))

;;; Problem 9

(defun problem-9 ()
  (iter (for c upfrom 1)
	(iter (for b from 1 below c)
	      (iter (for a from 1 below b)
		    (when (and (= (* c c)
				  (+ (* a a)
				     (* b b)))
			       (= 1000 (+ a b c)))
		      (return-from problem-9
			(* a b c)))))))

;;; Problem 10



(defun problem-10 ()
  (let ((sum 0))
    (map-primes (lambda (p)
		  (when (>= p 1000000)
		    (return-from problem-10
		      sum))
		  (incf sum p)))))

;;; Problem 11

(defparameter *problem-11-numbers*
  "08 02 22 97 38 15 00 40 00 75 04 05 07 78 52 12 50 77 91 08
49 49 99 40 17 81 18 57 60 87 17 40 98 43 69 48 04 56 62 00
81 49 31 73 55 79 14 29 93 71 40 67 53 88 30 03 49 13 36 65
52 70 95 23 04 60 11 42 69 24 68 56 01 32 56 71 37 02 36 91
22 31 16 71 51 67 63 89 41 92 36 54 22 40 40 28 66 33 13 80
24 47 32 60 99 03 45 02 44 75 33 53 78 36 84 20 35 17 12 50
32 98 81 28 64 23 67 10 26 38 40 67 59 54 70 66 18 38 64 70
67 26 20 68 02 62 12 20 95 63 94 39 63 08 40 91 66 49 94 21
24 55 58 05 66 73 99 26 97 17 78 78 96 83 14 88 34 89 63 72
21 36 23 09 75 00 76 44 20 45 35 14 00 61 33 97 34 31 33 95
78 17 53 28 22 75 31 67 15 94 03 80 04 62 16 14 09 53 56 92
16 39 05 42 96 35 31 47 55 58 88 24 00 17 54 24 36 29 85 57
86 56 00 48 35 71 89 07 05 44 44 37 44 60 21 58 51 54 17 58
19 80 81 68 05 94 47 69 28 73 92 13 86 52 17 77 04 89 55 40
04 52 08 83 97 35 99 16 07 97 57 32 16 26 26 79 33 27 98 66
88 36 68 87 57 62 20 72 03 46 33 67 46 55 12 32 63 93 53 69
04 42 16 73 38 25 39 11 24 94 72 18 08 46 29 32 40 62 76 36
20 69 36 41 72 30 23 88 34 62 99 69 82 67 59 85 74 04 36 16
20 73 35 29 78 31 90 01 74 31 49 71 48 86 81 16 23 57 05 54
01 70 54 71 83 51 54 69 16 92 33 48 61 43 52 01 89 19 67 48")

(defun problem-11-array ()
  (with-input-from-string (in *problem-11-numbers*)
    (iter (with array = (make-array '(20 20)))
	  (for x from 0 to (1- (array-total-size array)))
	  (setf (row-major-aref array x) (read in))
	  (finally (return array)))))

(defun problem-11 ()
  (let ((array (problem-11-array)))
    (iter (for x from 0 to 19)
	  (maximize (iter (for y from 0 to 19)
			  (when (<= x 16)
			    (maximize (iter (for i from x to (+ x 3))
					    (multiply (aref array i y))))
			    (when (<= y 16)
			      (maximize (iter (for i from 0 to 3)
					      (multiply (aref array
							      (+ x i)
							      (+ y i)))))))
			  (when (<= y 16)
			    (maximize (iter (for i from y to (+ y 3))
					    (multiply (aref array x i))))
			    (when (>= x 3)
			      (maximize (iter (for i from 0 to 3)
					      (multiply (aref array
							      (- x i)
							      (+ y i))))))
			    )
			  )))))

;;; Problem 12

(defun count-divisors (n)
  (let ((divisors 1))
    (map-primes (lambda (p)
		  (iter (for (values quot rem) = (truncate n p))
			(while (zerop rem))
			(sum 1 into power)
			(setf n quot)
			(finally (setf divisors (* divisors (1+ power)))))
		  (when (= n 1)
		    (return-from count-divisors
		      divisors))))))

;;; XXX Not submitted
(defun problem-12 ()
  (iter (for i upfrom 1)
	(sum i into triangle-num)
	(for divisors = (count-divisors triangle-num))
	(finding triangle-num such-that (> divisors 500))))

;;; Problem 13

(defparameter *problem-13-numbers* "37107287533902102798797998220837590246510135740250
46376937677490009712648124896970078050417018260538
74324986199524741059474233309513058123726617309629
91942213363574161572522430563301811072406154908250
23067588207539346171171980310421047513778063246676
89261670696623633820136378418383684178734361726757
28112879812849979408065481931592621691275889832738
44274228917432520321923589422876796487670272189318
47451445736001306439091167216856844588711603153276
70386486105843025439939619828917593665686757934951
62176457141856560629502157223196586755079324193331
64906352462741904929101432445813822663347944758178
92575867718337217661963751590579239728245598838407
58203565325359399008402633568948830189458628227828
80181199384826282014278194139940567587151170094390
35398664372827112653829987240784473053190104293586
86515506006295864861532075273371959191420517255829
71693888707715466499115593487603532921714970056938
54370070576826684624621495650076471787294438377604
53282654108756828443191190634694037855217779295145
36123272525000296071075082563815656710885258350721
45876576172410976447339110607218265236877223636045
17423706905851860660448207621209813287860733969412
81142660418086830619328460811191061556940512689692
51934325451728388641918047049293215058642563049483
62467221648435076201727918039944693004732956340691
15732444386908125794514089057706229429197107928209
55037687525678773091862540744969844508330393682126
18336384825330154686196124348767681297534375946515
80386287592878490201521685554828717201219257766954
78182833757993103614740356856449095527097864797581
16726320100436897842553539920931837441497806860984
48403098129077791799088218795327364475675590848030
87086987551392711854517078544161852424320693150332
59959406895756536782107074926966537676326235447210
69793950679652694742597709739166693763042633987085
41052684708299085211399427365734116182760315001271
65378607361501080857009149939512557028198746004375
35829035317434717326932123578154982629742552737307
94953759765105305946966067683156574377167401875275
88902802571733229619176668713819931811048770190271
25267680276078003013678680992525463401061632866526
36270218540497705585629946580636237993140746255962
24074486908231174977792365466257246923322810917141
91430288197103288597806669760892938638285025333403
34413065578016127815921815005561868836468420090470
23053081172816430487623791969842487255036638784583
11487696932154902810424020138335124462181441773470
63783299490636259666498587618221225225512486764533
67720186971698544312419572409913959008952310058822
95548255300263520781532296796249481641953868218774
76085327132285723110424803456124867697064507995236
37774242535411291684276865538926205024910326572967
23701913275725675285653248258265463092207058596522
29798860272258331913126375147341994889534765745501
18495701454879288984856827726077713721403798879715
38298203783031473527721580348144513491373226651381
34829543829199918180278916522431027392251122869539
40957953066405232632538044100059654939159879593635
29746152185502371307642255121183693803580388584903
41698116222072977186158236678424689157993532961922
62467957194401269043877107275048102390895523597457
23189706772547915061505504953922979530901129967519
86188088225875314529584099251203829009407770775672
11306739708304724483816533873502340845647058077308
82959174767140363198008187129011875491310547126581
97623331044818386269515456334926366572897563400500
42846280183517070527831839425882145521227251250327
55121603546981200581762165212827652751691296897789
32238195734329339946437501907836945765883352399886
75506164965184775180738168837861091527357929701337
62177842752192623401942399639168044983993173312731
32924185707147349566916674687634660915035914677504
99518671430235219628894890102423325116913619626622
73267460800591547471830798392868535206946944540724
76841822524674417161514036427982273348055556214818
97142617910342598647204516893989422179826088076852
87783646182799346313767754307809363333018982642090
10848802521674670883215120185883543223812876952786
71329612474782464538636993009049310363619763878039
62184073572399794223406235393808339651327408011116
66627891981488087797941876876144230030984490851411
60661826293682836764744779239180335110989069790714
85786944089552990653640447425576083659976645795096
66024396409905389607120198219976047599490197230297
64913982680032973156037120041377903785566085089252
16730939319872750275468906903707539413042652315011
94809377245048795150954100921645863754710598436791
78639167021187492431995700641917969777599028300699
15368713711936614952811305876380278410754449733078
40789923115535562561142322423255033685442488917353
44889911501440648020369068063960672322193204149535
41503128880339536053299340368006977710650566631954
81234880673210146739058568557934581403627822703280
82616570773948327592232845941706525094512325230608
22918802058777319719839450180888072429661980811197
77158542502016545090413245809786882778948721859617
72107838435069186155435662884062257473692284509516
20849603980134001723930671666823555245252804609722
53503534226472524250874054075591789781264330331690")

(defun problem-13 ()
  (with-input-from-string (in *problem-13-numbers*)
    (iter (with sum = (iter (for number = (read in nil nil))
			     (while number)
			     (sum number)))
	  (while (>= sum (expt 10 10)))
	  (setf sum (truncate sum 10))
	  (finally (return sum)))))



;;; Problem 14

(defun collatz-length (n &optional (acc 0))
  (if (= n 1)
      (+ acc 1)
      (collatz-length (if (evenp n)
			  (ash n -1)
			  (1+ (* 3 n)))
		      (1+ acc))))


(defun problem-14 ()
  (iter (for i from 999999 downto 1)
	(finding i maximizing (collatz-length i))))


;;; Problem 15

(defun grid-routes (n)
  (declare (optimize speed)
	   (type (unsigned-byte 10) n))
  (let ((cache (make-hash-table :size (expt (1+ n) 2))))
    (labels ((get-cache (x y)
	       (gethash (logior x (ash y 10)) cache))
	     (set-cache (x y val)
	       (setf (gethash (logior x (ash y 10)) cache)
		     val))
	     (rec (x y)
	       (declare (type (unsigned-byte 10) x y))
	       (if (or (= x n)
		       (= y n))
		   1
		   (or (get-cache x y)
		       (set-cache x y
				  (+ (rec (1+ x) y)
				     (rec x (1+ y))))))))
      (rec 0 0))))


(defun problem-15 ()
  (grid-routes 20))

;;; Problem 16

(defun problem-16 ()
  (iter (with n = (expt 2 1000))
	(for (values div mod) initially (values n 0)
	     then (truncate div 10))
	(sum mod)
	(while (> div 0))))

;;; Slightly cheating version:
; (reduce #'+ (map 'list 'digit-char-p (format nil "~A" (expt 2 1000))))

(defun problem-17 ()
  (length (remove-if (lambda (c)
		       (or (char= c #\Space)
			   (char= c #\-)))
		     (with-output-to-string (out)
		       (iter (for i from 1 to 1000)
			     (format out "~R" i))))))

;;; Problem 18

;;; Idea: Calculate a maximum upper bound and lower bound.

(defparameter *problem-18-file*
  (package-data-file "problem-18.txt"))

(defparameter *problem-67-file*
  (package-data-file "problem-67.txt"))

(defun parse-number-triangle (file)
  (with-open-file (in file)
    (iter (with triangle-array = (make-array 0
					     :adjustable t
					     :fill-pointer t))
      (for number = (read in nil nil))
	  (until (null number))
	  (vector-push-extend number triangle-array)
	  (finally (return triangle-array)))))

(defun listify-triangle (triangle)
  (coerce
   (iter outer
	 (for l from 1)
	 (generating t-pos from 0)
	 (unless (< (1+ t-pos) (length triangle))
	   (finish))
	 (collect (coerce (iter (repeat l)
				(collect (aref triangle (in outer (next t-pos)))))
			  'vector)))
   'vector))

(defun triangle-brute-force (triangle)
  (let* ((listified (listify-triangle triangle))
	 (levels (length listified)))
    (labels ((left (pos) pos)
	     (right (pos) (1+ pos))
	     (leafp (level)
	       (>= (1+ level) levels))
	     (val (level pos)
	       (aref (aref listified level) pos))
	     (max-down (l p)
	       (+ (val l p)
		  (if (leafp l)
		      0
		      (max (max-down (1+ l) (left p))
			   (max-down (1+ l) (right p)))))))
      (max-down 0 0))))

(defun triangle-cut-off-1 (triangle)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (let* ((listified (listify-triangle triangle))
	 (largest-single-value (iter (for el in-vector triangle)
				     (maximize el)))
	 (levels (length listified))
	 (max-so-far -1))
    (declare (type (integer -1 100000))
	     (type (integer 0 100) largest-single-value)
	     (type (integer 0 1000) levels))
    (labels ((left (pos) pos)
	     (right (pos) (1+ pos))
	     (leafp (level)
	       (>= (1+ level) levels))
	     (val (level pos)
	       (the (integer 0 1000) (svref (svref listified level) pos)))
	     (max-possible (l)
	       (* largest-single-value (- levels l)))
	     (max-down (l p acc)
	       (declare (type (integer 0 1000) l p)
			(type (integer 0 100000) acc))

	       (if (leafp l)
		   (let ((old max-so-far))
		     (declare (type fixnum old max-so-far))
		     (setq max-so-far (max max-so-far
					   (+ acc (val l p))))
		     (when (> max-so-far old)
		       (format t "~&~A~%" max-so-far)))
		   (let ((new-acc (+ acc (val l p)))
			 (nl (1+ l)))
		     (when (< (+ new-acc (max-possible nl))
			      max-so-far)
		       (return-from max-down))
		     (if (> (val nl (left p))
			    (val nl (right p)))
			 (progn
			   (max-down (1+ l) (left p) new-acc)
			   (max-down (1+ l) (right p) new-acc))
			 (progn
			   (max-down (1+ l) (right p) new-acc)
			   (max-down (1+ l) (left p) new-acc))
			 )))))
      (declare (inline left right val max-possible))
      (max-down 0 0 0)
      max-so-far)))


(defun triangle-bottum-up (triangle)
  (let* ((listified (listify-triangle triangle))
	 (levels (length listified))
	 (cache (listify-triangle (make-array (length triangle) :initial-element nil))))
    (labels ((max-up-cached (l p)
	       (if (or (>= p (+ l 1))
		       (< p 0))
		   0
		   (let ((line (svref cache l)))
		     (or (svref line p)
			 (setf (svref line p) (max-up l p))))))
	     (val (l p)
	       (if (or (>= p (+ l 1))
		       (< p 0))
		   0
		   (svref (svref listified l) p)))
	     (max-up (l p)
	       (+ (val l p)
		  (if (zerop l)
		      0
		      (if (> (val (1- l) (1- p))
			     (val (1- l) p))
			  (max
			   (max-up-cached (1- l) (1- p))
			   (max-up-cached (1- l) p))
			  (max
			   (max-up-cached (1- l) p)
			   (max-up-cached (1- l) (1- p))))))))
      (let ((positions-from-greatest
	     (mapcar #'car
		     (sort (iter (for el in-vector (svref listified (1- levels)))
				 (for pos upfrom 0)
				 (collect (cons pos el)))
			   #'>
			   :key #'cdr))))
	(iter (for next-pos in positions-from-greatest)
	      (for i upfrom 0)
	      (maximizing (max-up-cached (1- (length listified)) next-pos)))))))

;;; Problem 20

(defun problem-20 ()
  (reduce #'+ (map 'list #'digit-char-p (format nil "~A" (! 100)))))


;;; Problem 22

(defun name-value (name position)
  (* position
     (iter (for char in-vector name)
	   (sum (1+ (- (char-code char) (char-code #\A)))))))

(defun problem-22 ()
  (let ((names (with-open-file (in (package-data-file "names.txt"))
		 (iter (for name = (read in nil nil))
		       (while name)
		       (collect name)
		       (while (read-char in nil nil))))))
    (setf names (sort names #'string<=))
    (iter (for pos upfrom 1)
	  (for name in names)
	  (sum (name-value name pos)))))

;;; Problem 24

(defun problem-24 ()
  (let ((count 0))
    (map-permutations '(0 1 2 3 4 5 6 7 8 9)
		      (lambda (perm)
			(incf count)
			(when (= count 1000000)
			  (return-from problem-24
			    (funcall perm)))))))

;;; Problem 25

(defun problem-25 ()
  (let ((count 0))
    (map-fibonacci
     (lambda (n)
       (incf count)
       (when (> (log n 10) 999)
	 (return-from problem-25
	   count))))))

;;; Problem 36

(defun problem-36 ()
  (iter (for n from 1 to 1000000)
	(if (and (palindromp (let ((*print-base* 2))
			       (format nil "~A" n)))
		 (palindromp (let ((*print-base* 10))
			       (format nil "~A" n))))
	    (sum n))))

;;; Problem 48

(defun problem-48 ()
  (iter (with modulo = (expt 10 10))
	(for i from 1 to 1000)
	(for m 
	     initially 0
	     then (mod (+ m (exptmod i i modulo)) modulo))
	(finally (return m))))

;;; Problem 57

(defun digits (number)
  (values (ceiling (log number 10))))

(defun problem-57 ()
  (iter (repeat 1000)
	(for i initially 1/2 then (/ (+ 2 i)))
	(for approx-root = (1+ i))
	(count (> (digits (numerator approx-root))
		  (digits (denominator approx-root))))))

;;; Problem 79

 (defparameter *problem-79-numbers*
   '("319" "680" "180" "690" "129" "620" "762" "689" "762" "318" "368" "710" "720" "710" "629" "168"
     "160" "689" "716" "731" "736" "729" "316" "729" "729" "710" "769" "290" "719" "680" "318" "389"
     "162" "289" "162" "718" "729" "319" "790" "680" "890" "362" "319" "760" "316" "729" "380" "319"
     "728" "716"))

(defun try-for-size (set n)
  ;; Remove duplicates from the set.

  ;; Annotate each element in the set with its possible positions.

  ;; -> Constraint-Satisfaction Problem

  )

(defun problem-79 ()
  (iter (for n upfrom 4)
	(finding n such-that (try-for-size *problem-79-numbers* n))))

;;; Problem 160

(defun 5ify (n)
  (declare (type (integer 0 1000000000000) n)
           (optimize (speed 3)))
  (loop (multiple-value-bind (div rem)
	    (truncate n 10)
	  (if (zerop rem)
	      (setf n div)
	      (return-from 5ify
                ;; Changing that to 1000000, changes the global
                ;; solution which indicates major b0rkage... :-/
		(mod n 10000))))))

(defun 5sqrmult (n p)
  (if (= p 1)
      (5ify n)
      (if (evenp p)
	  (5sqrmult (5ify (* n n))
		    (ash p -1))
	  (5ify (* n (5sqrmult n (1- p)))))))

(defun ! (n)
  (iter (for i from 1 to n)
	(multiply i)))

(defun exists-ntimes (v)
  (cond ((= v 1) 
         11111116)
        ((< v 10)
         11111115)
        ((< v 100)
         11111114)
        ((< v 1000)
         11111113)
        ((< v 10000)
         11111112)
        ((< v 100000)
         11111111)
        (t (error "?"))))

;;; XXX Wrong!!
(defun problem-160 ()
  (declare (optimize speed))
  (iter (with prod = 1)
	(for i from 1 to 99999)
	(unless (zerop (mod i 10))
	  (setf prod (5ify (* prod
			      (5sqrmult
			       i (exists-ntimes i))))))
	(finally (return prod))))

(defun strip-zeroes (n)
  (declare (type unsigned-byte n)
           (optimize (speed 3)))
  (if (evenp n)
      (multiple-value-bind (d m)
          (truncate n 10)
        (if (zerop m)
            (strip-zeroes d)
            n))
      n))

;;; This one should be okay, but several orders of magnitude slower
;;; than the above version.
(defun problem-160-2 (&optional (n 1000000000000))
  (declare (optimize (speed 3)))
  (iter (with prod = 1)
        (for i from 1 to n)
        (declare (type unsigned-byte i))
        (when (zerop (logand i #xFFFFFF))
          (format t "~&~F% done~%" (* 100 (/ i n)))
          (force-output))
        (for fak = (mod (strip-zeroes i) (expt 10 10)))
        (declare (type (integer 0 #. (expt 10 10)) prod fak))
        (setf prod (mod 
                    (strip-zeroes (* prod fak)) 
                    (expt 10 10)))
        (finally (return prod))

        ))

(defun count-ntimes (v &optional (n 1000000000000))
  (declare (type (integer 0 1000000000000) n))
  (iter (with start = (get-universal-time))
        (for i from 1 to n)
        (when (zerop (logand i #xFFFFFF))
          (let* ((percent-done (* 100 (/ i n)))
                 (time-elapsed (- (get-universal-time) start))
                 (time-remaining (* 100 (/ time-elapsed 
                                           percent-done))))
            (format t "~&~,2F% done, ~,2F hours remaining~%"
                    percent-done
                    (/ time-remaining (* 60 60))))
          (finish-output))
        (count (= v (5ify i)))))

;;; Problem 21

(defun amicable-d (n)
  (iter (for i from 1 below n)
        (when (dividesp n i)
          (sum i))))

(defun problem-21 (&optional (n 10000))
  (iter 
    (with found = (make-hash-table))
    (for i from 1 below n)
    (unless (gethash i found)
      (let ((p (amicable-d i)))
        (when (and (/= p i)
                   (= i (amicable-d p)))
          (format t "Found ~A-~A~%" i p)
          (setf (gethash i found) t
                (gethash p found) t)
          (sum i)
          (sum p))))))

;;; EOF
