(ns djb.set.game-test
  (:require [djb.set.game :as sut]
            [cljs.test :refer-macros [deftest is testing run-tests]]))

(defn extract-valid-set [state]
  (->> (:deck state)
       (filter #(= :red (:colour %)))
       (filter #(= 1 (:number %)))
       (filter #(= :solid (:fill %)))))

(testing "makes-set?"
  (deftest winner
    (let [good-cards [{:colour :red, :fill :solid, :shape :lozenge, :number 1}
                      {:colour :red, :fill :none, :shape :lozenge, :number 1}
                      {:colour :red, :fill :lines, :shape :lozenge, :number 1}]
          good-cards-too-few [{:colour :red, :fill :solid, :shape :lozenge, :number 1}
                              {:colour :red, :fill :lines, :shape :lozenge, :number 1}]
          bad-cards  [{:colour :red, :fill :solid, :shape :lozenge, :number 1}
                      {:colour :green, :fill :lines, :shape :lozenge, :number 3}
                      {:colour :red, :fill :lines, :shape :lozenge, :number 1}]
          one-card  [{:colour :red, :fill :solid, :shape :lozenge, :number 1}]
          no-cards []]
      (is (= true (sut/makes-set? good-cards)))
      (is (= false (sut/makes-set? bad-cards)))
      (is (= false (sut/makes-set? no-cards)))
      (is (= false (sut/makes-set? one-card)))
      (is (= false (sut/makes-set? good-cards-too-few))))))

(testing "dealing"
  (deftest starting-state
    (let [state (sut/fresh-state sut/cards)]
      (is (= 81 (count (:deck state))))))

  (deftest initial-deal
    (let [state (sut/fresh-state sut/cards)
          new-state (sut/deal state)]
      (is
       (= 12 (count (:cards-in-play new-state)))
       (= (- 81 12) (count (:deck new-state))))))

  (deftest deal-back-to-twelve
    (let [state (sut/fresh-state sut/cards)
          ;; deal twice, should be noop second time
          new-state (sut/deal (sut/deal state))]
      (is
       (= 12 (count (:cards-in-play new-state)))
       (= (- 81 12) (count (:deck new-state)))))))

(testing "selecting"
  (deftest take-set
    (let [dealt (sut/deal (sut/fresh-state sut/cards))
          cards-to-take (take 3 (:cards-in-play dealt))
          state (assoc dealt :current-selection cards-to-take)
          new-state (sut/take-set state)]
      (is (= 12 (count (:cards-in-play new-state))))
      (is (= (- 81 15) (count (:deck new-state))))
      (is (not (some (set cards-to-take) (:cards-in-play new-state))))
      (is (= 1 (count (:sets new-state))))))

  (deftest back-to-twelve-after-fifteen
    (let [state (-> (sut/fresh-state sut/cards)
                    sut/deal
                    sut/deal-up)
          new-state (-> (assoc state :current-selection (take 3 (:cards-in-play state)))
                        sut/take-set)]
      (tap> "Here in test")
      (tap> new-state)
      (is (= 12 (count (:cards-in-play new-state))))))

  (deftest test-select-card
    (let [state (sut/deal (sut/fresh-state sut/cards))
          bad-card :whatever
          good-cards (take 10 (:cards-in-play state))]
      (is (= 1 (count (:current-selection (reduce (fn [state card] (sut/select-card state card)) state (take 1 good-cards))))))
      (is (= 2 (count (:current-selection (reduce (fn [state card] (sut/select-card state card)) state (take 2 good-cards))))))
      (is (= 3 (count (:current-selection (reduce (fn [state card] (sut/select-card state card)) state (take 3 good-cards))))))
      (is (= 1 (count (:current-selection (reduce (fn [state card] (sut/select-card state card)) state (take 4 good-cards))))))
      (is (= 0 (count (:current-selection (sut/select-card state bad-card))))))))

(testing "computer assist"
  (deftest detect-sets
    (let [singular-red-cards (->> sut/cards
                                  (filter #(and (= :red (:colour %)) (= 1 (:number %)))))]
      (is (every? sut/makes-set? (sut/detect-sets singular-red-cards)))
      (is (= 12 (count (sut/detect-sets singular-red-cards)))))))
