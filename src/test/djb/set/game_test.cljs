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
       (= (- 81 12) (count (:deck new-state))))))

  (deftest remove-cards-from-play
    (let [state (sut/fresh-state sut/cards)
          dealt (sut/deal state)
          new-state (sut/remove-cards-from-play dealt (take 3 (:cards-in-play dealt)))]
      (is (= (- 12 3) (count (:cards-in-play new-state))))))

  (deftest take-set
    (let [dealt (sut/deal (sut/fresh-state sut/cards))
          cards-to-take (take 3 (:cards-in-play dealt))
          state (assoc dealt :current-selection cards-to-take)
          new-state (sut/take-set state)]
      (is (= 12 (count (:cards-in-play new-state))))
      (is (= (- 81 15) (count (:deck new-state))))
      (is (not (some (set cards-to-take) (:cards-in-play new-state))))
      (is (= 1 (count (:sets new-state))))))

  (deftest test-select-card
    (let [state (sut/deal (sut/fresh-state sut/cards))
          bad-card 99
          good-card (first (:cards-in-play state))]
      (is (= 1 (count (:current-selection (sut/select-card state good-card)))))
      (is (= 3 (count (:current-selection
                       (-> state
                           (sut/select-card good-card)
                           (sut/select-card good-card)
                           (sut/select-card good-card))))))
      (is (= 2 (count (:current-selection
                       (-> state
                           (sut/select-card good-card)
                           (sut/select-card good-card))))))
      (is (= 1 (count (:current-selection
                       (-> state
                           (sut/select-card good-card)
                           (sut/select-card good-card)
                           (sut/select-card good-card)
                           (sut/select-card good-card))))))
      (is (= 0 (count (:current-selection (sut/select-card state bad-card))))))))
