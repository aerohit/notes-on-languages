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


;### Quoting
; quoting a symbol tells clojure to use the symbol itself as a data structure
(prn 'failed-protagonist-names)
(eval 'failed-protagonist-names)
(first '(failed-protagonist-names 0 1))
(second '(failed-protagonist-names 0 1))

; ??? Say what?
; "Function call" is just another term for an expression where the operator is a
; function expression. A function expression is just an expression which returns
; a function.

; eg of a function expression
(or + -)
((or + -) 1 2 3)
; 'or' returns the first truthy or last falsey value, 'and' returns the first falsey or
; last truthy value
((and (= 1 1) +) 1 2 3)
((first [+ 0]) 1 2 3)

; even though map is called on a vector/set, the returned value is a list
(map inc [0 1 2 3])
(map inc #{0 1 2 3})

;### Defining functions
; multi-arity functions
(defn x-chop
  "Describe the kind of chop"
  ([name chop-type]
   (str "I " chop-type " chop " name "! Take that!"))
  ([name]
   (x-chop name "karate")))

(x-chop "Kanye")

; variable arity function
(defn codger-communication [whippersnapper]
  (str "Get off my lawn, " whippersnapper "!!!"))

(defn codger [& whippersnappers]
  (map codger-communication whippersnappers))

(codger "Billy" "Anne-Marie")

(defn favorite-things [name & things]
  (str "Hi, " name ", here are my favorite things: " (clojure.string/join ", " things)))

(favorite-things "Doreen" "gum" "shoes" "kara-te")

;### Destructuring
; Destructuring lists and vectors
(defn my-first [[first-thing]]
  first-thing)

(my-first [1 2 3])

(defn chooser [[first-choice second-choice & other-choices]]
  (prn (str "Your first choice is: " first-choice))
  (prn (str "Your second choice is: " second-choice))
  (prn (str "And your other choices are " (clojure.string/join ", " other-choices))))

(chooser ["Marmalade" "Handsome Jack" "Pigpen" "Aquaman"])

; Destructuring maps
(defn announce-treasure-location [{lat :lat lng :lng}]
  (prn (str "Latitude " lat ", Longitude " lng)))

(announce-treasure-location {:lat 28.22 :lng 81.33})

; the above function could be written with a simpler syntax
(defn announce-treasure-location [{:keys [lat lng] :as treasure-location}]
  (prn (str "Latitude " lat ", Longitude " lng)))

;### Anonymous functions
((fn [x] (* x 8)) 4)
(#(* % 8) 4)
(#(str %1 " and " %2) "corn bread" "butter beans")
(#(str %1 ", " %2 " and " %&) :corn-bread :butter-beans :grapes)

; returning functions
(defn inc-maker [inc-by]
  #(+ inc-by %))

(def inc3 (inc-maker 3))
(inc3 5)


;### The shire's new top model
(def asym-hobbit-body-parts [{:name "head" :size 3}
                             {:name "left-eye" :size 1}
                             {:name "left-ear" :size 1}
                             {:name "mouth" :size 1}
                             {:name "nose" :size 1}
                             {:name "neck" :size 2}
                             {:name "left-shoulder" :size 3}
                             {:name "left-upper-arm" :size 3}
                             {:name "chest" :size 10}
                             {:name "back" :size 10}
                             {:name "left-forearm" :size 3}
                             {:name "abdomen" :size 6}
                             {:name "left-kidney" :size 1}
                             {:name "left-hand" :size 2}
                             {:name "left-knee" :size 2}
                             {:name "left-thigh" :size 4}
                             {:name "left-lower-leg" :size 3}
                             {:name "left-achilles" :size 1}
                             {:name "left-foot" :size 2}])

(defn needs-matching-part? [part]
  (re-find #"^left-" (:name part)))

(defn make-matching-part [part]
  {:name (clojure.string/replace (:name part) #"^left-" "right-")
   :size (:size part)})

(defn symmetrize-body-parts [asym-body-parts]
  (loop [remaining-parts asym-body-parts final-parts []]
    (if (empty? remaining-parts)
      final-parts
      (let [[part & more-parts] remaining-parts
            final-parts (conj final-parts part)]
        (if (needs-matching-part? part)
          (recur more-parts (conj final-parts (make-matching-part part)))
          (recur more-parts final-parts))))))

(symmetrize-body-parts asym-hobbit-body-parts)

(loop [iteration 0]
  (prn iteration)
  (if (> iteration 3)
    (prn "Goodbye")
    (recur (inc iteration))))
