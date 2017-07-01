(ns primetable.render
  (:require [primetable.core :as core]
            [bluebell.latex.core :as latex]))

(def settings
  {:columns 4
   :rows 30
   :pages 1})

(defn element-count [settings]
  (* (:columns settings)
     (:rows settings)
     (:pages settings)))

(defn generate-numbers [element-count]
  (let [v (core/make-first-factor-table (+ element-count 2))]
    (drop 2 (map (fn [i f] {:value i
                            :factor f})
                 (range (count v))
                 v))))
