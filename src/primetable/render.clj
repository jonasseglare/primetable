(ns primetable.render
  (:require [primetable.core :as core]
            [bluebell.utils.core :as utils]
            [bluebell.latex.core :as latex]))

(def settings
  {:columns 3
   :rows 4
   :pages 5})

(defn element-count [settings]
  (* (:columns settings)
     (:rows settings)
     (:pages settings)))

(defn generate-numbers [table]
  (drop 2 (map (fn [i f] {:value i
                          :factor f})
               (range (count table))
               table)))

(defn transpose-page [page]
  (apply map (cons vector (seq page))))

(defn columns-per-page [settings]
  (comp (utils/bundle (:rows settings))
        (utils/bundle (:columns settings))
        (map transpose-page)))

(defn make-page-data [settings]
  (let [table (core/make-first-factor-table (+ (element-count settings 2)))]
    {:page-data
     (reduce
      ((columns-per-page settings) conj)
      []
      (generate-numbers table))}
    :table table))

(defn render-number [table x]
  (if (nil? (nth table x)) x [:mathbf [::latex/arg x]]))

(defn element-to-latex [table e]
  ["$" (render-number table (:value e)) " & $ "
   (if (nil? (:factor e))
     (render-number table (:value e))
     [(render-number table (:factor e)) [:cdot]
      (render-number table (/ (:value e) (:factor e)))])
   "$"])
