(ns sicp.differentiation)

(defn error [m exp]
  (throw (Exception. (str m " " exp))))

(defn variable? [x] (symbol? x))

(defn same-variable? [v1 v2]
  (and (variable? v1) (variable? v2) (= v1 v2)))

(defn make-sum [a1 a2]
  (cond
    (and (number? a1) (zero? a1)) a2
    (and (number? a2) (zero? a2)) a1
    (and (number? a1) (number? a2)) (+ a1 a2)
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
    (and (number? m1) (number? m2)) (* m1 m2)
    (and (number? m1) (= 1 m1)) m2
    (and (number? m2) (= 1 m2)) m1
    :else (list '* m1 m2)))

(defn product? [x]
  (and (list? x) (= '* (first x))))

(defn multiplier [p]
  (second p))

(defn multiplicand [p]
  (nth p 2))

(defn make-exponentation [b e]
  (cond
    (and (number? b) (number? e)) (Math/pow b e)
    (and (number? e) (zero? e)) 1
    (and (number? e) (= 1 e)) b
    :else (list '** b e)))

(defn exponentation? [x]
  (and (list? x) (= '** (first x))))

(defn base [e]
  (second e))

(defn exponent [e]
  (nth e 2))

(defn deriv [exp var]
  (cond
    (number? exp) 0
    (variable? exp) (if (same-variable? exp var) 1 0)
    (sum? exp) (make-sum (deriv (addend exp) var) (deriv (augend exp) var))
    (product? exp) (make-sum
                    (make-product (multiplier exp) (deriv (multiplicand exp) var))
                    (make-product (deriv (multiplier exp) var) (multiplicand exp)))
    (exponentation? exp) (make-product
                          (exponent exp)
                          (make-product
                            (make-exponentation
                              (base exp)
                              (make-sum (exponent exp) -1))
                            (deriv (base exp) var)))
    :else (error "unknown expression type -- DERIV" exp)))

(comment

  (deriv '(+ x 3) 'x)
  ;; => (+ 1 0)
  ;; => 1

  (deriv '(* x y) 'x)
  ;; => (+ (* x y) (* y (+ x 3)))
  ;; => y

  (deriv '(* (* x y) (+ x 3)) 'x)
  ;; => (+ (* (* x y) (+ 1 0)) (* (+ (* x 0) (* 1 y)) (+ x 3)))
  ;; => (+ (* x y) (* y (+ x 3)))

  (deriv '(+ (* x 2) (* x 7)) 'x)
  ;; => 9

  (deriv '(** x 2) 'x)
  ;; => (* 2 x)

  (deriv '(** x 3) 'x)
  ;; => (* 3 (** x 2))

  (deriv '(** (* 2 x) 3) 'x)
  ;; => (* 3 (* (** (* 2 x) 2) 2))

  (deriv '(** (+ 1 (* 3 x)) 2) 'x)
  ;; => (* 2 (* (+ 1 (* 3 x)) 3))

  (deriv '(** 3 x) 'x)

  (deriv '(+ (* 3 (** x 3)) (* 5 (** x 2)) x) 'x)
  ;; => (+ (* 3 (* 3 (** x 2))) (* 5 (* 2 x)))

  .)
