;### Control Flow

; 'if' lets you wrap a single form
(if true
  "abra cadabra"
  "hocus pocus")

; 'do' lets you wrap multiple forms.

(if true
  (do (println "Success!")
      "abra cadabra")
  (do (println "Failure :(")
      "hocus pocus"))

; 'when' is a combination of if and do, without the else form.

(when true
  (println "Success!")
  "abra cadabra")

;### Naming things

; 'def' lets you bind a name to a value
(def failed-protagonist-names
  ["Larry Potter"
   "Doreen the Explorer"
   "The Incredible Bulk"])

;### nil, true, false, Truthiness, Equality
(nil? 1)
(nil? nil)

; only nil and false are falsy, everything else is truthy

;### Maps
(def blasphemous-map {:a 1
                      :b "boring example"
                      :c {:fname "rohit"}})

; getting values with 'get'
(get blasphemous-map :b)
(get blasphemous-map :d)
(get blasphemous-map :d "default-value")

; 'get-in' function lets you lookup in nested maps
(get-in blasphemous-map [:c :fname])

; another way to lookup in a map is to treat the map as a function and the kes as its argument
(blasphemous-map :b)

; but the idiomatic way is to use keys as functions and maps as the arguments
(:b blasphemous-map)
(:d blasphemous-map "default-value")

; 'hash-map' function could be used to create a map
(hash-map :a 1 :b 2)
