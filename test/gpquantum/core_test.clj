(ns gpquantum.core-test
  (:require [clojure.test :refer :all]
            [gpquantum.core :refer :all]))

(deftest init-test
  (testing "Does it compile and initialize?"
    (is (= or-cases {'(0 0) 0, '(0 1) 1, '(1 0) 1, '(1 1) 1}))))