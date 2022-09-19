(ns djb.set.core-test
  (:require [djb.set.core :as sut]
            [cljs.test :refer-macros [deftest is testing run-tests]]))

(testing "makes-set?"
  (deftest winner
    (let [winner [
                  {:colour :red, :fill :solid, :shape :lozenge, :number 1}
                  {:colour :red, :fill :none, :shape :lozenge, :number 1}
                  {:colour :red, :fill :lines, :shape :lozenge, :number 1}]]
      (is (= true (sut/makes-set? winner)))))

  (deftest loser
    (let [loser [
                  {:colour :red, :fill :solid, :shape :lozenge, :number 1}
                  {:colour :green, :fill :lines, :shape :lozenge, :number 3}
                  {:colour :red, :fill :lines, :shape :lozenge, :number 1}]]
      (is (= false (sut/makes-set? loser))))))

(comment
 (run-tests))
