  (ns djb.set.core
    (:require ["react-dom/client" :refer [createRoot]]
              [reagent.core :as r]
              [djb.set.graphics :as graphics]))

(def cards
  (let [cards (for [colour [:red :green :purple]
                    fill [:none :lines :solid]
                    shape [:diamond :lozenge :squiggle]
                    number [1 2 3]]
                {:colour colour :fill fill :shape shape :number number})]
    (map #(merge {:id %1} %2) (range) cards)))

(defn fresh-state [cards] {:cards cards :deck (shuffle cards) :cards-in-play '() :current-selection '() :sets '()})

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
  (let [new-cards-in-play (remove (set cards) (:cards-in-play state))]
    (assoc state :cards-in-play new-cards-in-play)))

(defn take-set [state]
  (let [set-cards (:current-selection state)
        new-state (remove-cards-from-play state set-cards)
        new-sets (conj (:sets state) set-cards)]
    (deal-up (assoc new-state :sets new-sets :current-selection '()))))

(defn holding-set? [state]
  (makes-set? (:current-selection state)))

(defn- take-sets-if-any [state]
  (if (holding-set? state)
    (take-set state)
    state))

(defn select-card [state card]
  (let [legal-card? (contains? (set (:cards-in-play state)) card)
        room-in-hand? (< (count (:current-selection state)) 3)]
    (cond (and legal-card? room-in-hand?)
          (take-sets-if-any (update state :current-selection conj card))
          legal-card?
          (assoc state :current-selection (list card))
          :else
          (assoc state :current-selection '()))))

(defn set-card-component [card]
  [:li.card
   (for [n (range (:number card))]
     (with-meta
       (graphics/shape (:shape card) (:colour card) (:fill card))
       {:key (str (:id card) "-" n)}))])

(defn card-component [astate card]
  (let [is-selected? (contains? (set (:current-selection @astate)) card)]
    [:li.card
     {:on-click #(reset! astate (select-card @astate card))
      :class (when is-selected? "selected")}
     (for [n (range (:number card))]
       (with-meta
         (graphics/shape (:shape card) (:colour card) (:fill card))
         {:key (str (:id card) "-" n)}))]))

(defn complete-sets [sets]
  (when (seq @sets)
    [:ul.cards.sets
     (map (fn [idx complete-set]
            #{:key idx}
            [:div.set
             (map
              (fn [card]
                ^{:key (:id card)} [set-card-component card]) complete-set)])
          (range) @sets)]))

(defn deal-button [astate]
  [:div.deal
   [:button {:type "button" :on-click #(reset! astate (deal @astate))} "Deal"]])

(defn three-more-cards-button [astate]
  [:div.deal
   [:button {:type "button" :on-click #(reset! astate (deal-up @astate))} "3 more cards"]])

(defn shuffle-button [astate]
  [:div.deal
   [:button {:type "button" :on-click #(reset! astate (assoc @astate :cards-in-play (take 12 (shuffle (:deck @astate)))))}
    "Shuffle"]])

(defn reset-button [astate]
  [:div.deal
   [:button {:type "button" :on-click #(reset! astate (fresh-state cards))} "Reset"]])

(defn ui [astate]
  (let [sets (r/cursor astate [:sets])
        cards-in-play (r/cursor astate [:cards-in-play])
        deck (r/cursor astate [:deck])
        complete-sets (partial complete-sets sets)
        card-component (partial card-component astate)
        deal-button (partial deal-button astate)
        shuffle-button (partial shuffle-button astate)
        three-more-cards-button (partial three-more-cards-button astate)
        reset-button (partial reset-button astate)]
    [:div.grid
     [:div.left
      [:h1 "SET"]
      [:div.debug
       [complete-sets]
       [:p (str (count @deck) " cards in deck")]
       [:div.controls [:div [deal-button] [shuffle-button] [three-more-cards-button] [reset-button]]]]]
     [:div.right
      [:div#set
       [:svg {:id "svg-defs"} (map graphics/svg-lines [:red :purple :green])]
       [:div.game
        [:div.tabletop [:ul.cards (for [card @cards-in-play] ^{:key (:id card)} [card-component card])]]]]]]))

(defonce root (createRoot (.getElementById js/document "root")))
(defonce astate (r/atom (fresh-state cards)))

(defn render [astate]
  (.render root (r/as-element [ui astate])))

(defn init []
  (render astate)
  (reset! astate (deal (fresh-state cards))))

(defn ^:dev/after-load re-render
  []
  (render astate))
