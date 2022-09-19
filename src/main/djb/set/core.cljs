(ns djb.set.core
  (:require ["react-dom/client" :refer [createRoot]]
            [reagent.core :as r]))

(def cards
  (for [colour [:red :green :purple]
        fill [:none :lines :solid]
        shape [:diamond :lozenge :squiggle]
        number [1 2 3]] 
    {:colour colour :fill fill :shape shape :number number}))

(defn makes-set? [cards]
  (let [attrs [:colour :fill :shape :number]]
    (every? (fn [prop]
                (let [vs (map #(get % prop) cards)
                      variations (count (distinct vs))]
                  (or (= variations 1) ; all the same
                      (= variations 3)))) ; all different
            attrs)))

(def deck (atom (shuffle cards)))
(def cards-in-play (r/atom []))

(defn deal []
  (reset! cards-in-play (take 12 @deck))
  (swap! deck (partial drop 12)))

(defn reset []
  (reset! deck (shuffle cards))
  (deal))

(defn card-component [card]
  [:li.card
   [:div.symbol (apply str (vals card))]])

(defn deal-button []
  [:div.deal
   [:button {:type "button" :on-click deal} "Deal"]])

(defn reset-button []
  [:div.deal
   [:button {:type "button" :on-click reset} "Reset"]])

(println (count @cards-in-play))

(defn debug-tools []
  [:div.debug
   [:p (str (count @deck) " cards in deck")]
   [:p (str (count @cards-in-play) " cards in play")]])

(defn game []
  [:div.game
    [:div.debug [debug-tools]]
    [:div.controls [:div [deal-button] [reset-button]]]
    [:div.tabletop [:ul.cards (for [card @cards-in-play] ^{:key (hash card)} [card-component card])]]])

(defonce root (createRoot (.getElementById js/document "root")))

(defn render []
  (.render root (r/as-element [game])))

(defn init []
  (reset! deck (shuffle cards))
  (deal)
  (render))

(defn ^:dev/after-load re-render
  []
  (render))

(comment
  (init)
  (def hand (take 3 (shuffle cards))))
