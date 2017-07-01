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

(defn generate-numbers [element-count]
  (let [v (core/make-first-factor-table (+ element-count 2))]
    (drop 2 (map (fn [i f] {:value i
                            :factor f})
                 (range (count v))
                 v))))

(defn transpose-page [page]
  (apply map (cons vector (seq page))))

(defn columns-per-page [settings]
  (comp (utils/bundle (:rows settings))
        (utils/bundle (:columns settings))
        (map transpose-page)))

(defn make-page-data [settings]
  (reduce
   ((columns-per-page settings) conj)
   []
   (generate-numbers (element-count settings))))

;(def k (make-page-data settings))
