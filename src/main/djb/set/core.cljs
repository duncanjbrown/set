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
(def current-selection (r/atom []))
(def sets (r/atom []))

(add-watch current-selection :referee (fn [_key _atom _old new]
                                        (when (= (count new) 3)
                                          (println (makes-set? new)))))

(defn deal []
  (reset! cards-in-play (take 12 @deck))
  (swap! deck (partial drop 12)))

(defn reset []
  (reset! deck (shuffle cards))
  (reset! current-selection [])
  (deal))

(defn select-card [card]
  (swap! current-selection conj card))

(def svgs
  {:diamond
    "M 16.62,36.00 C 16.62,36.00 71.00,8.50 71.00,8.50 71.00,8.50 124.88,36.00 124.88,36.00 124.88,36.00 71.00,64.50 71.00,64.50 71.00,64.50 16.62,36.00 16.62,36.00 Z",
   :lozenge
   "M 16.00,36.00 C 16.00,9.00 43.00,9.00 43.00,9.00 43.00,9.00 98.00,9.00 98.00,9.00 98.00,9.00 125.00,9.00 125.00,36.00 125.00,65.00 98.00,65.00 98.00,65.00 98.00,65.00 43.00,65.00 43.00,65.00 43.00,65.00 16.00,65.00 16.00,36.00 Z"
   :squiggle
   "M 16.00,36.00 C 16.00,17.00 29.22,5.03 43.00,9.00 56.78,12.97 84.01,25.35 98.00,23.62 111.99,21.90 106.88,9.00 112.00,9.00 117.12,9.00 125.00,17.00 125.00,36.00 125.00,55.00 108.30,72.27 98.00,65.00 87.70,57.73 57.48,50.49 43.00,51.50 28.52,52.51 33.25,65.00 29.00,65.00 24.75,65.00 16.00,55.00 16.00,36.00 Z"})

(defn svg-lines [colour id]
  [:defs
    [:pattern
      {:id id
       :patternTransform "rotate(90 2 2)",
        :height "2.5",
        :width "4",
        :patternUnits "userSpaceOnUse"}
      [:path {:stroke-width "2", :stroke colour, :d "M -0.5,0 L 4,0"}]]])

(defn get-shape [shape colour fill]
  (let [id (rand-int 10000)]
    [:svg
      {:xmlns "http://www.w3.org/2000/svg"
        :width "1.97222in"
        :height "1.05556in"
        :viewBox "0 0 142 76"}
      (when (= fill :lines)
        (svg-lines colour id))
      [:path
        {:d (shape svgs)
         :fill (case fill
                     :solid colour
                     :lines (str "url(#" id ")")
                     :none "white")
         :stroke colour
         :stroke-width 2}]]))

(get-shape :diamond "red" :none)

(defn card-component [card]
  [:li.card
   {:on-click #(select-card card)}
   (repeat (:number card) (get-shape (:shape card) (:colour card) (:fill card)))])

(defn deal-button []
  [:div.deal
   [:button {:type "button" :on-click deal} "Deal"]])

(defn reset-button []
  [:div.deal
   [:button {:type "button" :on-click reset} "Reset"]])

(defn debug-tools []
  [:div.debug
   [:p (str @current-selection)]
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
