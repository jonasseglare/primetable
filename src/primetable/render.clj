(ns primetable.render
  (:require [primetable.core :as core]
            [bluebell.utils.core :as utils]
            [bluebell.utils.string :as string]
            [bluebell.latex.core :as latex]
            [bluebell.latex.io-utils :as io-utils]))

(def settings
  {:columns 5
   :rows 50
   :pages 20})

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
  (let [table (core/make-first-factor-table
               (+ (element-count settings) 2))]
    {:page-data
     (reduce
      ((columns-per-page settings) conj)
      []
      (generate-numbers table))
     :table table}))

(defn render-number [table x]
  (if (nil? (nth table x)) x (latex/mathbf x)))

(defn element-to-latex [table e]
  (latex/cols
   (latex/inline-math (render-number table (:value e)))
   (if (nil? (:factor e))
     "---"
     (latex/inline-math
      (latex/spaced
       (render-number table (:factor e))
       (latex/cmd "cdot")
       (render-number table (/ (:value e) (:factor e))))))))

(defn row-to-latex [table row]
  (apply latex/cols (map #(element-to-latex table %) row)))


(def col-pair "V\\\"arde & Faktorer")

(defn table-header [cols]
  (apply latex/cols (take cols (repeat col-pair))))

(defn get-cols [page]
  (count (first page)))

(defn table-spec [n]
  (string/join-strings "|" (take n (repeat  "rl"))))

(defn page-to-latex [table page]
  (let [cols (get-cols page)]
    (latex/block
     {:name "table"}
     (latex/tabular
      (table-spec cols)
      (apply latex/rows
             `(~(table-header cols)
               ~(latex/cmd "hline")
               ~@(map #(row-to-latex table %) page)))))))

(defn make-document-sub [data]
  [(latex/cmd "documentclass" (latex/sq "10pt,a4paper,notitlepage") (latex/br "article"))
   (latex/usepackage "a4wide")
   (latex/block
    {:name "document"}
    (mapv #(page-to-latex
            (:table data)
            %)
          (:page-data data)))])

(defn make-document [settings]
  (make-document-sub (make-page-data settings)))

(comment
  (def pd (make-page-data settings))
  (def pages (:page-data pd))
  (def table (:table pd))
  (def s (page-to-latex table (first pages)))
  (println (latex/render s))
  (println (latex/render (make-document pd)))
  (io-utils/display (make-document settings))

  )
