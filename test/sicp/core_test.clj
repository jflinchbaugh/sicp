(ns sicp.core-test
  (:require [clojure.test :refer :all]
            [sicp.core :refer :all]))

(deftest test-permutations
  (testing "permutations"
    (is (= [nil] (permutations [])))
    (is (= [[1]] (permutations [1])))
    (is (= [[1 2 3] [1 3 2] [2 1 3] [2 3 1] [3 1 2] [3 2 1]] (permutations [1 2 3])))))
