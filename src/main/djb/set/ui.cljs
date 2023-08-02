(ns djb.set.ui
  (:require [djb.set.game :as game]
            [reagent.core :as r]
            [clojure.string]
            [djb.set.graphics :as graphics]))

(defn set-card-component [{:keys [id number shape colour fill]}]
  [:li.card
   (for [n (range number)]
     ^{:key (str id "-" n)}
     [graphics/shape shape colour fill])])

(defn card-component [astate card]
  (let [is-selected? (contains? (set (:current-selection @astate)) card)
        highlights (keep-indexed (fn [i s] (when (contains? (set s) card) i)) (:sets-in-play @astate))
        set-classes (when (:highlighting? @astate) (map #(str "in-set set-" %) highlights))
        class (str (when is-selected? " selected ") (clojure.string/join " " set-classes))]
    [:li.card
     {:on-click #(reset! astate (game/select-card @astate card))
      :class class}
     (for [n (range (:number card))]
       ^{:key (str (:id card) "-" n)}
       [graphics/shape (:shape card) (:colour card) (:fill card)])]))

(defn complete-sets [sets]
  (when (seq @sets)
    [:ul.cards.sets
     (map-indexed
      (fn [idx complete-set]
        ^{:key idx}
        [:div.set (map (fn [card] ^{:key (:id card)} [set-card-component card]) complete-set)])
      @sets)]))

(defn action-button [astate action-fn label]
  [:div.deal
   [:button {:type "button" :on-click #(reset! astate (action-fn @astate))} label]])

(defn highlight-toggle [astate]
  [:div.highlight-toggle
   [:input {:type "checkbox"
            :checked (:highlighting? @astate)
            :id "highlight-toggle"
            :on-change #(swap! astate update :highlighting? not)}]
   [:label {:for "highlight-toggle"} " Highlight Sets"]])

(defn ui [astate]
  (let [sets (r/cursor astate [:sets])
        cards-in-play (r/cursor astate [:cards-in-play])
        deck (r/cursor astate [:deck])
        complete-sets (partial complete-sets sets)
        highlight-toggle (partial highlight-toggle astate)
        card-component (partial card-component astate)]
    (tap> @astate)
    [:div.grid
     [:div.left
      [:h1 "SET"]
      [:div.debug
       [complete-sets]
       [:p (str (count @deck) " cards in deck")]
       [:div.controls
        [action-button astate game/deal "Deal"]
        [action-button astate game/shuffle-cards "Shuffle"]
        [action-button astate game/deal-up "3 more cards"]
        [action-button astate #(assoc % :deck (take 3 (:deck %))) "Last 3"]
        [action-button astate #(game/deal (game/fresh-state game/cards)) "Reset"]
        [highlight-toggle]]]]
     [:div.right
      [:div#set
       [:svg {:id "svg-defs"} (map graphics/svg-lines [:red :purple :green])]
       [:div.game
        [:div.tabletop [:ul.cards (for [card @cards-in-play] ^{:key (:id card)} [card-component card])]]]]]]))

