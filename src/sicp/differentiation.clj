(ns sicp.differentiation)

(defn error [m exp]
  (throw (Exception. (str m " " exp))))

(defn variable? [e])

(defn same-variable? [v1 v2])

(defn sum? [e])

(defn addend [e])

(defn augend [e])

(defn make-sum [a1 a2])

(defn product? [e])

(defn multiplier [e])

(defn multiplicand [e])

(defn make-product [m1 m2])

(defn deriv [exp var]
  (cond
    (number? exp) 0
    (variable? exp) (if (same-variable? exp var) 1 0)
    (sum? exp) (make-sum (deriv (addend exp) var) (deriv (augend exp) var))
    (product? exp) (make-sum
                     (make-product (multiplier exp) (deriv (multiplicand exp) var))
                     (make-product (deriv (multiplier exp) var) (multiplicand exp)))
    :else (error "unknown expression type -- DERIV" exp)))
