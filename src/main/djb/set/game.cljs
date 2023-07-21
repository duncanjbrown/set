(ns djb.set.game
  (:require [clojure.math.combinatorics :as comb]))

(def cards
  (let [cards (for [colour [:red :green :purple]
                    fill [:none :lines :solid]
                    shape [:diamond :lozenge :squiggle]
                    number [1 2 3]]
                {:colour colour :fill fill :shape shape :number number})]
    (map #(merge {:id %1} %2) (range) cards)))

(defn fresh-state [cards] {:cards cards
                           :deck (shuffle cards)
                           :sets-in-play '()
                           :cards-in-play '()
                           :current-selection #{}
                           :sets '()
                           :highlighting? false})

(defn makes-set? [cards]
  (let [attrs [:colour :fill :shape :number]]
    (and (= 3 (count cards))
         (every? (fn [prop]
                   (let [vs (map #(get % prop) cards)
                         variations (count (distinct vs))]
                     (or (= variations 1) ; all the same
                         (= variations 3)))) ; all different
                 attrs))))

(defn detect-sets [cards]
  (filter makes-set? (comb/combinations cards 3)))

(defn initial-deal [state]
  (let [[new-cards-in-play new-deck] (split-at 12 (:deck state))]
    (-> state
        (assoc :deck new-deck 
               :cards-in-play new-cards-in-play)
        (assoc :sets-in-play (detect-sets new-cards-in-play)))))

(defn deal-up [state]
  (let [cards-in-play (concat (take 3 (:deck state)) (:cards-in-play state))]
    (-> state
        (assoc :deck (drop 3 (:deck state)) :cards-in-play cards-in-play)
        (assoc :sets-in-play (detect-sets cards-in-play)))))

(defn deal [state]
  (cond
    (empty? (:cards-in-play state)) (initial-deal state)
    (< 12 (:cards-in-play state)) (deal-up state)
    :else state))

(defn shuffle-cards [state]
  (let [cards-in-play (concat (take 12 (shuffle (:deck state))))]
    (-> state
        (assoc :cards-in-play cards-in-play)
        (assoc :sets-in-play (detect-sets cards-in-play)))))

;; we can track which cards we're about to remove...
(defn- indices-of-els [s els]
  (let [element-set (set els)]
    (keep-indexed (fn [i member] (when (contains? element-set member) i)) s)))

;; ...and replace them with the new cards
(defn- replace-els [s els replacements]
  (let [idxs (indices-of-els s els)]
    (apply assoc (vec s) (mapcat vector idxs replacements))))

(defn- take-set-and-deal-back-to-twelve [state]
  (let [{:keys [deck cards-in-play current-selection sets]} state
        [new-cards new-deck] (split-at 3 deck)
        new-cards-in-play (replace-els cards-in-play current-selection new-cards)
        new-sets (conj sets current-selection)
        new-sets-in-play (detect-sets new-cards-in-play)]
      (assoc state
             :sets-in-play new-sets-in-play
             :cards-in-play new-cards-in-play 
             :deck new-deck
             :sets new-sets
             :current-selection #{})))

(defn- take-set-and-do-not-deal [state]
  (let [{:keys [cards-in-play current-selection sets]} state
        new-cards-in-play (filter #(not (contains? current-selection %)) cards-in-play)
        new-sets (conj sets current-selection)
        new-sets-in-play (detect-sets new-cards-in-play)]
      (assoc state
             :sets-in-play new-sets-in-play
             :cards-in-play new-cards-in-play 
             :sets new-sets
             :current-selection #{})))

(defn take-set [state]
  (if (< (- (count (:cards-in-play state)) 3) 12)
    (take-set-and-deal-back-to-twelve state)
    (take-set-and-do-not-deal state)))

(defn take-sets-if-any [state]
  (if (makes-set? (:current-selection state))
    (take-set state)
    state))

(defn select-card [state card]
  (let [legal-card? (contains? (set (:cards-in-play state)) card)
        room-in-hand? (< (count (:current-selection state)) 3)]
    (cond (contains? (:current-selection state) card)
          (-> state
              (update :current-selection disj card))
          (and legal-card? room-in-hand?)
          (-> state
              (update :current-selection conj card)
              take-sets-if-any)
          legal-card?
          (assoc state :current-selection (set (list card)))
          :else
          (assoc state :current-selection #{}))))
