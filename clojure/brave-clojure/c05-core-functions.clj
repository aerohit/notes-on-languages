;### Mapping over functions
; **map** creates a lazy sequence by applying a function to the passed collection.

; mapping over a vector, list and a set
(defn titleize [topic]
  (str topic " for the Brave and True"))

(map titleize ["Hamsters" "Ragnarok"])
(map titleize '("Hamsters" "Ragnarok"))
(map titleize #{"Ragnarok" "Hamsters"})

; mapping over a map
(defn label-key-val [[k v]]
  (str "key: " k " val: " v))

(map label-key-val {:name "rohit" :occupation "coder"})

; since the map returns a list, it could be converted back into a map using **into**
(into {}
      (map (fn [[k v]] [k (inc v)])
           {:max 30 :min 10}))

; passing map multiple collections
(map str [:a :b :c] '(:A :B :C))

; passing map a collection of functions
(defn sum [numbers]
  (reduce + numbers))

(defn avg [numbers]
  (/ (sum numbers) (count numbers)))

(defn stats [numbers]
  (map #(% numbers) [sum avg count]))

(stats [1 3 5])
(stats [1 3 5 7])

; **reducing** over a map
; could be used to update values
(reduce (fn [new-map [k v]]
          (assoc new-map k (inc v)))
        {}
        {:min 10 :max 30})

; could be used to filter based on values
(reduce (fn [new-map [k v]]
          (if (> v 4) (assoc new-map k v)
            new-map))
        {}
        {:a 3 :b 30})

; **take**, **drop**, **take-while** and **drop-while**
(take 3 [1 2 3 4 5])
(drop 3 [1 2 3 4 5])

(def food-journal
  [{:month 1 :day 1 :human 5.3 :critter 2.3}
   {:month 1 :day 2 :human 5.1 :critter 2.0}
   {:month 2 :day 1 :human 4.9 :critter 2.1}
   {:month 2 :day 2 :human 5.0 :critter 2.5}
   {:month 3 :day 1 :human 4.2 :critter 3.3}
   {:month 3 :day 2 :human 4.0 :critter 3.8}
   {:month 4 :day 1 :human 3.7 :critter 3.9}
   {:month 4 :day 2 :human 3.7 :critter 3.6}])

(take-while #(< (:month %) 3) food-journal)
(drop-while #(< (:month %) 3) food-journal)


; **filter** and **some**
(filter #(= (:month %) 2) food-journal)

(some #(> (:critter %) 5) food-journal)
(some #(> (:critter %) 3) food-journal)

; notice that some didn't return the actual entry, to get that
(some #(and (> (:critter %) 3) %) food-journal)

; **sort** and **sort-by**
(sort  [3 1 2])
(sort-by count  ["aaa" "c" "bb"])

; **concat**
(concat [1 2] [3 4])


; clojure realized 32 items at a time and then caches the results
; following is an example to show just that
(def identitites
  [{:alias "Batman" :real "Bruce Wayne"}
   {:alias "Spiderman" :real "Peter Parker"}
   {:alias "Santa" :real "Your mom"}
   {:alias "Easter Bunny" :real "Your dad"}
   {:real "real 5", :alias "alias 5"}
   {:real "real 6", :alias "alias 6"}
   {:real "real 7", :alias "alias 7"}
   {:real "real 8", :alias "alias 8"}
   {:real "real 9", :alias "alias 9"}
   {:real "real 10", :alias "alias 10"}
   {:real "real 11", :alias "alias 11"}
   {:real "real 12", :alias "alias 12"}
   {:real "real 13", :alias "alias 13"}
   {:real "real 14", :alias "alias 14"}
   {:real "real 15", :alias "alias 15"}
   {:real "real 16", :alias "alias 16"}
   {:real "real 17", :alias "alias 17"}
   {:real "real 18", :alias "alias 18"}
   {:real "real 19", :alias "alias 19"}
   {:real "real 20", :alias "alias 20"}
   {:real "real 21", :alias "alias 21"}
   {:real "real 22", :alias "alias 22"}
   {:real "real 23", :alias "alias 23"}
   {:real "real 24", :alias "alias 24"}
   {:real "real 25", :alias "alias 25"}
   {:real "real 26", :alias "alias 26"}
   {:real "real 27", :alias "alias 27"}
   {:real "real 28", :alias "alias 28"}
   {:real "real 29", :alias "alias 29"}
   {:real "real 30", :alias "alias 30"}
   {:real "real 31", :alias "alias 31"}
   {:real "real 32", :alias "alias 32"}
   {:real "real 33", :alias "alias 33"}
   {:real "real 34", :alias "alias 34"}
   {:real "real 35", :alias "alias 35"}
   {:real "real 36", :alias "alias 36"}
   {:real "real 37", :alias "alias 37"}
   {:real "real 38", :alias "alias 38"}
   {:real "real 39", :alias "alias 39"}])

(defn snitch [id]
  (prn id)
  (:real id))

(def revealed-identities (map snitch identitites))

(first revealed-identities)
(first revealed-identities)

;### Infinite sequences

(take 8 (repeat "na"))
(take 8 (repeatedly (fn [] (rand-int 10))))

(defn even-numbers
  ([] (even-numbers 0))
  ([n] (cons n (lazy-seq (even-numbers (+ n 2))))))

(take 10 (even-numbers))


;### The collection abstraction
(count [])
(every? even? [2 4 6])
(empty? [])

(into {} (map identity {:key "val"}))
(into #{} (map identity [:garlic-clove :garlic-clove]))
(into {:favorite-emotion "gloomy"} [[:sunlight-reaction "Glitter!"]])
(into ["cherry"] '("pine" "spruce"))
(into {:favorite-animal "kitty"} {:least-favorite-smell "dog" :color "blue"})

(conj [0] [1])
(conj [0] 1 2 3)
(conj {:time "midnight"} [:place "ye olde cemetarium"])

; **conj** and **into** are so similar that one could be defined in terms of the other
(defn my-conj [target & additions]
  (into target additions))

(my-conj [0] 1 2 3)

(defn my-into [target additions]
  (apply conj target additions))
(my-into [0] [1 2 3])

(max 2 3 4)
(max [2 3 4])
(apply max [2 3 4])

(def add10 (partial + 10))
(add10 4)

(def add-missing-element
  (partial conj ["water" "earth" "air"]))
(add-missing-element "unobtainium" "adamantium")

(defn my-partial [partialized-fn & args]
  (fn [& more-args]
    (apply partialized-fn (into more-args (reverse args)))))

(def add20 (my-partial + 20))
(add20 4)

(defn lousy-logger [log-level message]
  (condp = log-level
    :warn (clojure.string/lower-case message)
    :emergency (clojure.string/upper-case message)))

(def warn (partial lousy-logger :warn))
(warn "Red light ahead")

(defn my-complement [fun]
  (fn [& args]
    (not (apply fun args))))
(def my-pos? (my-complement neg?))
(my-pos? 1)
(my-pos? -1)
