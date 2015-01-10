(ns fwpd
  (:require [clojure.string :as s]))
(def filename "suspects.csv")

(def headers->keywords {"Name" :name
                        "Glitter Index" :glitter-index})

(defn str->int [str]
  (Integer. str))

(def conversions {:name identity
                  :glitter-index str->int})

(defn parse [string]
  (map #(s/split % #",")
       (s/split string #"\n")))

(defn mapify [rows]
  (let [headers (map #(get headers->keywords %) (first rows))
        unmapped-rows (rest rows)]
    (map (fn [unmapped-row]
           (into {}
                 (map (fn [header column]
                        [header ((get conversions header) column)])
                      headers
                      unmapped-row)))
         unmapped-rows)))
(defn glitter-filter [minimum-glitter records]
  (filter #(>= (:glitter-index %) minimum-glitter) records))

(glitter-filter 3 (mapify (parse (slurp filename))))
