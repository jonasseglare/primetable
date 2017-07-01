(ns primetable.core-test
  (:require [clojure.test :refer :all]
            [primetable.core :refer :all]))

(deftest table-test
  (is (= (make-first-factor-table 10)
         [nil nil nil nil 2 nil 2 nil 2 3])))
