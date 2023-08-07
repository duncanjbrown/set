(ns djb.set.graphics)

(def svgs
  {:diamond
   "M 16.62,36.00 C 16.62,36.00 71.00,8.50 71.00,8.50 71.00,8.50 124.88,36.00 124.88,36.00 124.88,36.00 71.00,64.50 71.00,64.50 71.00,64.50 16.62,36.00 16.62,36.00 Z",
   :lozenge
   "M 16.00,36.00 C 16.00,9.00 43.00,9.00 43.00,9.00 43.00,9.00 98.00,9.00 98.00,9.00 98.00,9.00 125.00,9.00 125.00,36.00 125.00,65.00 98.00,65.00 98.00,65.00 98.00,65.00 43.00,65.00 43.00,65.00 43.00,65.00 16.00,65.00 16.00,36.00 Z"
   :squiggle
   "M 16.00,36.00 C 16.00,17.00 29.22,5.03 43.00,9.00 56.78,12.97 84.01,25.35 98.00,23.62 111.99,21.90 106.88,9.00 112.00,9.00 117.12,9.00 125.00,17.00 125.00,36.00 125.00,55.00 108.30,72.27 98.00,65.00 87.70,57.73 57.48,50.49 43.00,51.50 28.52,52.51 33.25,65.00 29.00,65.00 24.75,65.00 16.00,55.00 16.00,36.00 Z"})

(defn svg-lines [colour]
  ^{:key (str "lines-" colour)}
  [:defs
   [:pattern
    {:id (str (name colour) "-lines")
     :patternTransform "rotate(90 2 2)",
     :height "5",
     :width "4",
     :patternUnits "userSpaceOnUse"}
    [:path {:stroke-width "3", :stroke colour, :d "M -1,-0.5 L 4,-0.5"}]]])

(defn shape [shape colour fill]
  [:svg
   {:xmlns "http://www.w3.org/2000/svg"
    :width "1.97222in"
    :height "1.05556in"
    :viewBox "0 0 142 76"}
   [:path
    {:d (shape svgs)
     :fill (case fill
             :solid colour
             :lines (str "url(#" (name colour) "-lines)")
             :none "white")
     :stroke colour
     :stroke-width 3}]])

