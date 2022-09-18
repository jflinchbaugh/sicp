(ns sicp.differentiation)

(defn error [m exp]
  (throw (Exception. (str m " " exp))))

(defn variable? [x] (symbol? x))

(defn same-variable? [v1 v2]
  (and (variable? v1) (variable? v2) (= v1 v2)))

(defn make-sum [a1 a2]
  (cond
    (and (number? a1) (zero? a1)) a2
    (and (number? a2)(zero? a2)) a1
    :else (list '+ a1 a2)))

(defn sum? [x]
  (and (list? x) (= '+ (first x))))

(defn addend [s]
  (second s))

(defn augend [s]
  (nth s 2))

(defn make-product [m1 m2]
  (cond
    (and (number? m1) (zero? m1)) 0
    (and (number? m2) (zero? m2)) 0
    (and (number? m1) (= 1 m1)) m2
    (and (number? m2) (= 1 m2)) m1
    :else (list '* m1 m2)))

(defn product? [x]
  (and (list? x) (= '* (first x))))

(defn multiplier [p]
  (second p))

(defn multiplicand [p]
  (nth p 2))

(defn deriv [exp var]
  (cond
    (number? exp) 0
    (variable? exp) (if (same-variable? exp var) 1 0)
    (sum? exp) (make-sum (deriv (addend exp) var) (deriv (augend exp) var))
    (product? exp) (make-sum
                     (make-product (multiplier exp) (deriv (multiplicand exp) var))
                     (make-product (deriv (multiplier exp) var) (multiplicand exp)))
    :else (error "unknown expression type -- DERIV" exp)))

(comment

  (deriv '(+ x 3) 'x)
  ;; => 1
  ;; => (+ 1 0)

  (deriv '(* x y) 'x)
  ;; => y
  ;; => (+ (* x 0) (* 1 y))

  (deriv '(* (* x y) (+ x 3)) 'x)
  ;; => (+ (* x y) (* y (+ x 3)))
  ;; => (+ (* (* x y) (+ 1 0)) (* (+ (* x 0) (* 1 y)) (+ x 3)))

  .)
