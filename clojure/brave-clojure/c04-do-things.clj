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

;### Vectors
(def blasphemous-vector [3 2 1])

(get blasphemous-vector 0)
(blasphemous-vector 0)

; vectors could be created with the **vector** function
(vector "creepy" "full" "moon")

; cong adds to the end of the vector
(conj [1 2 3] 4)

;### Lists
(def blasphemous-list '(1 2 3 4))

; **get** doesn't work for lists
(get blasphemous-list 0)

; **nth** works, but the performance is slow
(nth blasphemous-list 0)

; lists could be created with the **list** function
(list 1 2 3)

; conj adds to the beginning of the list
(conj blasphemous-list 5)

;### Sets
(def blasphemous-set #{:a :b :c})

(conj blasphemous-set :c)

; to check whether a value exists in a set
(get blasphemous-set :a)
(get blasphemous-set :d)

(:a blasphemous-set)

; a set could be created from a vector or a list by **set** function
(set [3 3 3 4 4])

; an unobvious use is to check whether a value exists in a collection
(get (set [3 3 3 4 4]) 3)
(get (set [3 3 3 4 4]) 5)

; **hash-set** and **sorted-set** could be used to create sets from values
(hash-set 1 1 3 1 2)
(sorted-set :b :c :a :1)
