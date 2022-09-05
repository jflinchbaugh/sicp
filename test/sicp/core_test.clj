(ns sicp.core-test
  (:require [clojure.test :refer :all]
            [sicp.core :refer :all]))

(deftest test-permutations
  (testing "permutations"
    (is (= [nil] (permutations [])))
    (is (= [[1]] (permutations [1])))
    (is (=
          [[1 2 3] [1 3 2] [2 1 3] [2 3 1] [3 1 2] [3 2 1]]
          (permutations [1 2 3])))))

(deftest test-accumulate
  (testing "accumulate"
    (is (= 1 (accumulate * 1 [])))
    (is (= 6 (accumulate * 1 [1 2 3])))
    (is (= 1 (accumulate * 1 nil)))
    (is (= 6 (accumulate + 0 [1 2 3])))
    (is (= [1 2 3 4] (accumulate cons [] [1 2 3 4])))))

(deftest test-fringe
  (is (= [] (fringe nil)))
  (is (= [] (fringe [])))
  (is (= [1] (fringe 1)))
  (is (= [1 2] (fringe [1 2])))
  (is (= [1 2] (fringe [1 [2]])))
  (is (= [1 2 3 4 5 6] (fringe [1 [2 3] [[4 5] 6]]))))

(deftest test-enumerate-interval
  (is (= [] (enumerate-interval 1 0)))
  (is (= [1] (enumerate-interval 1 1)))
  (is (= [1 2] (enumerate-interval 1 2))))

(deftest test-map'
  (is (= [] (map' inc [])))
  (is (= [2] (map' inc [1])))
  (is (= [2 3] (map' inc [1 2]))))

(deftest test-map''
  (is (= [] (map'' inc [])))
  (is (= [2] (map'' inc [1])))
  (is (= [2 3] (map'' inc [1 2]))))

(deftest test-horner-eval
  (is (= 79 (horner-eval 2 [1 3 0 5 0 1]))))

(deftest test-horner-eval'
  (is (= 79 (horner-eval' 2 [1 3 0 5 0 1]))))

(deftest test-accumulate-n
  (is (= [10 20 30] (accumulate-n + 0 [[2 3 5] [1 2 17] [5 10 15]]))))
