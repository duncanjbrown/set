(ns djb.set.ui
  (:require [djb.set.game :as game]
            [reagent.core :as r]
            [djb.set.graphics :as graphics]))

(defn set-card-component [card]
  [:li.card
   (for [n (range (:number card))]
     (with-meta
       (graphics/shape (:shape card) (:colour card) (:fill card))
       {:key (str (:id card) "-" n)}))])

(defn card-component [astate card]
  (let [is-selected? (contains? (set (:current-selection @astate)) card)]
    [:li.card
     {:on-click #(reset! astate (game/select-card @astate card))
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
   [:button {:type "button" :on-click #(reset! astate (game/deal @astate))} "Deal"]])

(defn three-more-cards-button [astate]
  [:div.deal
   [:button {:type "button" :on-click #(reset! astate (game/deal-up @astate))} "3 more cards"]])

(defn shuffle-button [astate]
  [:div.deal
   [:button {:type "button" :on-click #(reset! astate (assoc @astate :cards-in-play (take 12 (shuffle (:deck @astate)))))}
    "Shuffle"]])

(defn last-3-button [astate]
  (let [deck (take 3 (:deck @astate))]
    [:div.deal
      [:button {:type "button" :on-click #(reset! astate (assoc @astate :deck deck))}
        "Last 3"]]))

(defn reset-button [astate]
  [:div.deal
   [:button {:type "button" :on-click #(reset! astate (game/deal (game/fresh-state game/cards)))} "Reset"]])

(defn ui [astate]
  (let [sets (r/cursor astate [:sets])
        cards-in-play (r/cursor astate [:cards-in-play])
        deck (r/cursor astate [:deck])
        complete-sets (partial complete-sets sets)
        card-component (partial card-component astate)
        deal-button (partial deal-button astate)
        shuffle-button (partial shuffle-button astate)
        last-3-button (partial last-3-button astate)
        three-more-cards-button (partial three-more-cards-button astate)
        reset-button (partial reset-button astate)]
    [:div.grid
     [:div.left
      [:h1 "SET"]
      [:div.debug
       [complete-sets]
       [:p (str (count @deck) " cards in deck")]
       [:div.controls [:div [deal-button] [shuffle-button] [three-more-cards-button] [last-3-button] [reset-button]]]]]
     [:div.right
      [:div#set
       [:svg {:id "svg-defs"} (map graphics/svg-lines [:red :purple :green])]
       [:div.game
        [:div.tabletop [:ul.cards (for [card @cards-in-play] ^{:key (:id card)} [card-component card])]]]]]]))

