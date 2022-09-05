(ns sicp.core)

(defn ackermann [x y]
  (cond
    (= 0 y) 0
    (= 0 x) (* 2 y)
    (= 1 y) 2
    :else (ackermann (- x 1) (ackermann x (- y 1)))))

(comment

  (ackermann 1 10)

  (ackermann 2 4)

  (ackermann 3 3)

  (let [f (partial ackermann 0)
        g (partial ackermann 1)
        h (partial ackermann 2)]
    (map (juxt f g h) (range 5)))

  .)

(defn gcd [a b]
  (if (= b 0)
    a
    (gcd b (mod a b))))

(comment

  (gcd 10 15)

  .)

(def first-denomination {1 1, 2 5, 3 10, 4 25, 5 50})

(def cc (memoize (fn [amount kinds-of-coins]
                   (cond
                     (= amount 0) 1
                     (or (< amount 0) (= kinds-of-coins 0)) 0
                     :else (+
                            (cc
                             amount
                             (dec kinds-of-coins))
                            (cc
                             (- amount (first-denomination kinds-of-coins))
                             kinds-of-coins))))))

(defn count-change [amount] (cc amount 5))

(comment

  (time (count-change 2000))

  .)


; higher order functions SICP


; linear recursive sum (pops stack on too big)


(defn lin-sum [term a next b]
  (if (> a b)
    0
    (+
     (term a)
     (lin-sum term (next a) next b))))

(defn cube [x] (* x x x))

(comment
  (lin-sum cube 1 inc 10)

  .)

; iterative sum instead of linear recursive
(defn sum [term a next b]
  (loop [a a
         result 0]
    (if (> a b)
      result
      (recur (next a) (+ result (term a))))))

(defn pi-sum [a b]
  (let [pi-term (fn [x] (/ 1.0 (* x (+ x 2))))
        pi-next (fn [x] (+ x 4))]
    (sum pi-term a pi-next b)))

(comment
  (* 8 (pi-sum 1 100000))

  .)

(defn integral [f a b dx]
  (let [add-dx (fn [x] (+ x dx))]
    (*
     (sum f (+ a (/ dx 2.0)) add-dx b)
     dx)))

(comment
  (integral cube 0 1 0.00000001)

  .)

(comment

  (sum identity 0 inc 10)

  ; no more overflows! 
  (* 8 (pi-sum 1 1000000000))

  (integral cube 0 1 0.0000001)

  .)

(defn deriv [g dx]
  (fn [x]
    (/
     (- (g (+ x dx)) (g x))
     dx)))

(defn newton-transform [g dx]
  (fn [x]
    (- x (/ (g x) ((deriv g dx) x)))))

(defn average [x y]
  (/ (+ x y) 2))

(defn average-damp [f]
  (fn [x] (average x (f x))))

(defn abs [x] (Math/abs x))

(defn fixed-point [f tolerance first-guess]
  (let [close-enough? (fn [v1 v2] (< (abs (- v1 v2)) tolerance))
        my-try (fn [guess]
                 (let [next (f guess)]
                   (if (close-enough? guess next)
                     next
                     (recur next))))]
    (my-try first-guess)))

(defn newtons-method [g dx guess]
  (fixed-point (newton-transform g dx) guess))

(defn fixed-point-of-transform [g transform tolerance guess]
  (fixed-point (transform g) tolerance guess))

(defn my-sqrt [x]
  (fixed-point-of-transform (fn [y] (- (* y) x)) #(newton-transform % 0.0001) 0.0001 1.0))

(defn my-sqrt [x] (fixed-point #(/ x %) 0.0001 1.0))

(defn cos [a] (Math/cos a))

(comment
  ((deriv cube 0.00001) 5)

  (fixed-point Math/cos 0.1 1.0)

  (my-sqrt 2)

  .)

(defn make-rat [n d] (list n d))

(defn numer [x] (first x))
(defn denom [x] (second x))
(defn print-rat [x] (str (numer x) "/" (denom x)))

(comment
  (print-rat (make-rat 1 2))

  .)

(defn count-leaves [x]
  (cond
    (not (seq? x)) 1
    (empty? x) 0
    :else (+
           (count-leaves (first x))
           (count-leaves (rest x)))))

(comment

  (def x (cons (list 1 2) (list 3 4)))

  (seq? 1)

  (count x)
;; => 3

  (empty? nil)

  (count-leaves x)

  .)

(comment ex 2.25

         (-> '(1 3 (5 7) 9)
             rest
             rest
             first
             rest
             first)

         (-> '((7))
             first
             first)

         (-> '(1 (2 (3 (4 (5 (6 7))))))
             rest
             first
             rest
             first
             rest
             first
             rest
             first
             rest
             first
             rest
             first)

         (reverse [1 2 3])

         .)

(defn deep-reverse [coll]
  (if (coll? coll)
    (map deep-reverse (reverse coll))
    coll))

(comment

  (deep-reverse [1 2 [3 [4 5 6]]])

  .)

(defn unique-pairs [n]
  (mapcat
   (fn [i]
     (map
      (fn [j]
        (list i j))
      (range i (inc n))))
   (range 1 (inc n))))

(defn pair-sums [n]
  (map (fn [p] (cons (apply + p) p)) (unique-pairs n)))

(defn is-prime? [n]
  (>= 2
      (count
       (filter
        (fn [i] (zero? (mod n i)))
        (range 1 (inc n))))))

(comment

  (unique-pairs 6)

  (is-prime? 4)

  (->>
   6
   pair-sums
   (filter (comp is-prime? first)))

  .)

(defn permutations [c]
  (if (empty? c)
    (list nil)
    (mapcat
     (fn [cx]
       (map
        (fn [cp] (cons cx cp)) (permutations (remove #{cx} c)))) c)))

(comment

  (permutations [1 2 3]);; => ((1 2 3) (1 3 2) (2 1 3) (2 3 1) (3 1 2) (3 2 1))

  (let [l [1 2 3]]
    (map (fn [x] (remove #{x} l)) l))

  .)

(defn fringe [tree]
  (cond
    (nil? tree) []
    (not (coll? tree)) [tree]
    (empty? tree) []
    :else (concat (fringe (first tree)) (fringe (rest tree)))
    ))

(def enumerate-tree fringe)

(comment

  (not (coll? 1))

  (fringe (list 1 (list 2 3) (list (list 4 5) 6)))
  (fringe (list 1 2))

  .)

(defn reverse-right [lst]
  (reduce conj '() lst)
  )

(comment
  (reverse-right [1 2 3])



  .)

(defn accumulate [op initial sequence]
  (if (empty? sequence)
    initial
    (op (first sequence) (accumulate op initial (rest sequence)))))

(comment
  (accumulate cons [] [1 2 3 4])

  )

(defn enumerate-interval
  [low high]
  (if (> low high) []
      (cons low (enumerate-interval (inc low) high))))

(defn map' [op sequence]
  (accumulate (fn [x y] (cons (op x) y)) [] sequence))

(defn map'' [op sequence]
  (reduce (fn [a v] (conj a (op v))) [] sequence))

(defn append [seq1 seq2]
  (accumulate cons seq2 seq1))

(defn length [sequence]
  (accumulate (fn [a b] (inc b)) 0 sequence))

(defn length' [sequence]
  (reduce (fn [a b] (inc a)) 0 sequence))

(comment
  (append [1 2 3] [4 5 6])

  (length [1 2 3 4 5])
  (length' [1 2 3 4 5])

  )

(defn horner-eval [x coeff-seq]
  (accumulate
    (fn [this-coeff higher-terms]
      (+ (* higher-terms x) this-coeff))
    0
    coeff-seq))

(defn horner-eval' [x coeff-seq]
  (reduce
    (fn [higher-terms this-coeff]
      (+ (* higher-terms x) this-coeff))
    0
    (reverse coeff-seq)))

(comment

  (horner-eval 2 '(1 3 0 5 0 1))

  (let [x 2]
    (+ 1 (* 3 x) (* 5 (Math/pow x 3)) (Math/pow x 5)))

  (let [x 2]
    (+ (* (+ (* (+ (* (+ (* (+ (* (+ 1) x) 0) x) 5) x) 0) x) 3) x) 1))

  .)

(defn accumulate-n [op init seqs]
  (if (empty? (first seqs))
    []
    (cons
      (accumulate op init (first seqs))
      (accumulate-n op init (rest seqs)))))
