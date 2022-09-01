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
  (is (= [1 2] (enumerate-interval 1 2)))
  )
