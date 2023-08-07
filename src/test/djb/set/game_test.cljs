(ns djb.set.game-test
  (:require [djb.set.game :as sut]
            [cljs.test :refer-macros [deftest is testing]]))

(defn debug [value message]
  (tap> {:location message :value value})
  value)

;; It's handy to have a few valid sets
(def red-cards (filter #(= (:colour %) :red) sut/cards))

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
  (defn select-cards
    "Select these cards from the table"

    [state cards]
    (reduce (fn [acc-state card]
              (sut/select-card acc-state card))
            state cards))

  (defn play-until-end
    "Remove sets until there are no more sets"
    [state]
    (let [available-sets (:sets-in-play state)]
      (if (and available-sets (not-empty available-sets))
        (let [new-state (select-cards state (first available-sets))]
          (recur new-state))
        state)))

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
          new-state (-> (assoc state :current-selection (set (take 3 (:cards-in-play state))))
                        sut/take-set)]
      (is (= 12 (count (:cards-in-play new-state))))))

  (deftest test-select-card
    (let [state (sut/deal (sut/fresh-state sut/cards))
          bad-card :whatever
          good-cards (take 10 (:cards-in-play state))
          current-selection-count-after-selecting (fn [n]
                                                    (count (:current-selection (select-cards state (take n good-cards)))))]
      (is (= 1 (current-selection-count-after-selecting 1)))
      (is (= 2 (current-selection-count-after-selecting 2)))
      (is (= 0 (current-selection-count-after-selecting 3)))
      (is (= 1 (current-selection-count-after-selecting 4)))
      (is (= 0 (count (:current-selection (sut/select-card state bad-card)))))))

  (deftest end-of-game
    (let [state (->
                 (sut/fresh-state (take 12 red-cards)) ;; the deck contains a tableful of valid sets
                 sut/deal
                 play-until-end)]
      (is (= 0 (count (:current-selection state))))
      (is (empty? (:sets-in-play state)))
      (is (= 0 (count (remove nil? (:cards-in-play state))))))))

(testing "replace-with-pad"
  (deftest with-els-to-replace
    (let [sequence (range 5)
          to-replace [1 2 3]
          replacements [:a :b :c]]
      (is (= (sut/replace-with-pad sequence to-replace replacements) [0 :a :b :c 4]))))

  (deftest with-no-els-to-replace
    (let [sequence (range 5)
          to-replace [1 2 3]
          replacements []]
      (is (= (sut/replace-with-pad sequence to-replace replacements) [0 nil nil nil 4])))))

(testing "computer assist"
  (deftest detect-sets
    (let [singular-red-cards (->> sut/cards
                                  (filter #(and (= :red (:colour %)) (= 1 (:number %)))))]
      (is (every? sut/makes-set? (sut/detect-sets singular-red-cards)))
      (is (= 12 (count (sut/detect-sets singular-red-cards)))))))
