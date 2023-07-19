(ns djb.set.game)

(def cards
  (let [cards (for [colour [:red :green :purple]
                    fill [:none :lines :solid]
                    shape [:diamond :lozenge :squiggle]
                    number [1 2 3]]
                {:colour colour :fill fill :shape shape :number number})]
    (map #(merge {:id %1} %2) (range) cards)))

(defn fresh-state [cards] {:cards cards :deck (shuffle cards) :cards-in-play '() :current-selection #{} :sets '()})

(defn makes-set? [cards]
  (let [attrs [:colour :fill :shape :number]]
    (and (= 3 (count cards))
         (every? (fn [prop]
                   (let [vs (map #(get % prop) cards)
                         variations (count (distinct vs))]
                     (or (= variations 1) ; all the same
                         (= variations 3)))) ; all different
                 attrs))))

(defn initial-deal [state]
  (assoc state :deck (drop 12 (:deck state)) :cards-in-play (take 12 (:deck state))))

(defn deal-up [state]
  (let [cards-in-play (concat (take 3 (:deck state)) (:cards-in-play state))]
    (assoc state :deck (drop 3 (:deck state)) :cards-in-play cards-in-play)))

(defn deal [state]
  (cond
    (empty? (:cards-in-play state)) (initial-deal state)
    (< 12 (:cards-in-play state)) (deal-up state)
    :else state))

(defn remove-cards-from-play [state cards]
  (-> state
      (update :cards-in-play (partial remove (set cards)))))

(defn take-set [state]
  (let [{:keys [current-selection sets]} state
        new-state (remove-cards-from-play state current-selection)
        new-sets (conj sets current-selection)]
    (-> new-state
        (assoc :sets new-sets :current-selection #{})
        deal-up)))

(defn holding-set? [state]
  (makes-set? (:current-selection state)))

(defn take-sets-if-any [state]
  (if (holding-set? state)
    (take-set state)
    state))

(defn select-card [state card]
  (let [legal-card? (contains? (set (:cards-in-play state)) card)
        room-in-hand? (< (count (:current-selection state)) 3)]
    (cond (and legal-card? room-in-hand?)
          (-> state
              (update :current-selection conj card)
              take-sets-if-any)
          legal-card?
          (assoc state :current-selection (set (list card)))
          :else
          (assoc state :current-selection #{}))))
