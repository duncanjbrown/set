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
    (into {} (map #(vector %1 (merge {:id %1} %2)) (range) cards))))

(defonce deck (atom (shuffle (keys cards))))
(defonce cards-in-play (r/atom []))
(defonce current-selection (r/atom #{}))
(defonce sets (r/atom []))

(defn makes-set? [cards]
  (let [attrs [:colour :fill :shape :number]]
    (every? (fn [prop]
                (let [vs (map #(get % prop) cards)
                      variations (count (distinct vs))]
                  (or (= variations 1) ; all the same
                      (= variations 3)))) ; all different
            attrs)))

(defn adjudicate [_key buffer _old selection]
  (when (= (count selection) 3)
    (reset! buffer [])
    (let [selected-cards (vals (select-keys cards selection))]
      (when (makes-set? selected-cards)
        (swap! sets conj selection)))))

(add-watch current-selection :adjudicate adjudicate)

(defn deal []
  (reset! cards-in-play (take 12 @deck))
  (swap! deck (partial drop 12)))

(defn reset []
  (reset! deck (shuffle (keys cards)))
  (reset! current-selection #{})
  (deal))

(defn select-card [card-id]
  (swap! current-selection conj card-id))

(defn card-component [card-id]
  (let [card (get cards card-id)]
    [:li.card
      {:on-click #(select-card card-id)}
      (for [n (range (:number card))]
        (with-meta
         (graphics/shape (:shape card) (:colour card) (:fill card))
         {:key (str card-id "-" n)}))]))

(defn hand []
  [:ul.cards.hand
   (map (fn [card-id]
          ^{:key card-id} [card-component card-id])
    @current-selection)])

(defn complete-sets []
  (when (seq @sets)
    [:ul.cards.sets
     (map (fn [idx complete-set]
            #{:key idx}
            [:div.set
              (map
                (fn [card-id]
                  ^{:key card-id} [card-component card-id]) complete-set)])
          (range) @sets)]))

(defn deal-button []
  [:div.deal
   [:button {:type "button" :on-click deal} "Deal"]])

(defn reset-button []
  [:div.deal
   [:button {:type "button" :on-click reset} "Reset"]])

(defn debug-tools []
  [:div.debug
   [hand]
   [:h2 "Sets"]
   [complete-sets]
   [:p (str (count @deck) " cards in deck")]
   [:p (str (count @cards-in-play) " cards in play")]])

(defn game []
  [:div#set
    [:svg {:id "svg-defs"} (map graphics/svg-lines [:red :purple :green])]
    [:div.game
      [:div.debug [debug-tools]]
      [:div.controls [:div [deal-button] [reset-button]]]
      [:div.tabletop [:ul.cards (for [card-id @cards-in-play] ^{:key card-id} [card-component card-id])]]]])


(defonce root (createRoot (.getElementById js/document "root")))

(defn render []
  (.render root (r/as-element [game])))

(defn init []
  (reset! deck (shuffle (keys cards)))
  (deal)
  (render))

(defn ^:dev/after-load re-render
  []
  (render))

(comment
  ;; #rtrace (+ 1 2)
  ;; #rtrace2 (+ 2 2)
  (tap> "from comment")
  (init)
  (def hand (take 3 (shuffle cards))))
