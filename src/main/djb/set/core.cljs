  (ns djb.set.core
    (:require ["react-dom/client" :refer [createRoot]]
              [reagent.core :as r]
              [djb.set.game :as game]
              [djb.set.ui :as ui]))

(defonce root (createRoot (.getElementById js/document "root")))
(defonce astate (r/atom nil))

(defn render [astate]
  (.render root (r/as-element [ui/ui astate])))

(defn init []
  (reset! astate (game/deal (game/fresh-state game/cards)))
  (render astate))

(defn ^:dev/after-load re-render
  []
  (render astate))
