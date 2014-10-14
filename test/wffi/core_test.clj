(ns wffi.core-test
  (:require [clojure.test :refer :all]
            [wffi.core :refer :all]))

(deftest split-test
  (testing "split"
    (= (split #(not= % 1) [0 0 0 1 2 2 2])
       '((0 0 0) (1 2 2 2)))
    (= (split #(not= % 1) [])
       '(() ()))
    (= (split #(do nil) [1 2 3 4 5])
       '((1 2 3 4 5)))))

(deftest gather-test
  (testing "gather"
    (= (gather #(= % 1) [1
                         1
                         1 2
                         1 2 3
                         1])
       '((1)
         (1)
         (1 2)
         (1 2 3)
         (1)))))
