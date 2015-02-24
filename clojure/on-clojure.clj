;#    Concepts

;###  Macros
;#### Builtin macros

;#### __when-let__ and __if-let__
;     The following forms are equivalent
(when :truthy-thing
  (let [res :truthy-thing] (prn res)))

(when-let [res :truthy-thing] (prn res))


;###  Persistent Data Structures
;  -  A DS which preserves a previous version of itself when modified.

;#### Structural sharing
;     Creating a tree

(defn xconj [t v]
  (cond
    (nil? t)        {:val v :L nil :R nil}
    (< v (:val t))  {:val (:val t)
                     :L (xconj (:L t) v)
                     :R (:R t)}
    :else           {:val (:val t)
                     :L (:L t)
                     :R (xconj (:R t) v)}))

(defn xseq [t]
  (when t (concat (xseq (:L t)) [(:val t)] (xseq (:R t)))))

(xseq (xconj (xconj (xconj nil 5) 3) 2))

;#### The art of laziness

; (steps [1 2 3 4]) => [1 [2 [3 [4 []]]]]

;     A recursive solution like the following would stackoverflow for large lists

(defn rec-steps [[x & xs]]
  (if x
    [x (rec-steps xs)]
    []))
(rec-steps [1 2 3 4])

;     Some advice while trying to be lazy

;     * Use the **lazy-seq** macro at the outermost level of your lazy sequence–producing expression(s).
;     * If you happen to be consuming another sequence during your operations, then use **rest** instead of **next**.
;     * Prefer higher-order functions when processing sequences.
;     * Don’t hold on to your head.

;     **rest** vs **next**
(def very-lazy (-> (iterate #(do (println \.) (inc %)) 1)
                   rest rest rest))

(def less-lazy (-> (iterate #(do (println \.) (inc %)) 1)
                   next next next))

(println (first very-lazy))
(println (first less-lazy))

;     Using **lazy-seq** to build sequences

(defn simple-range [lower upper]
  (lazy-seq
    (when (< lower upper)
      (cons lower (simple-range (inc lower) upper)))))
(simple-range 1 10)

;     Implementing steps with **lazy-seq**
(defn lazy-steps [s]
  (lazy-seq
    (if (seq s)
      [(first s) (lazy-steps (rest s))]
      [])))
(println (lazy-steps [1 2 3 4]))
(class (lazy-steps [1 2 3 4]))
(dorun (lazy-steps (range 2000000)))


;#### Infinite sequences

(defn triangle [n]
  (/ (* n (inc n)) 2))
(def triangle-nums (map triangle (iterate inc 1)))

(take 10 triangle-nums)
(take 10 (filter even? triangle-nums))
(nth triangle-nums 99)


;#### Explicit laziness:
;     The **delay** and **force** macros
(defn defer-expensive [cheap expensive]
  (if-let [good-enough (force cheap)]
    good-enough
    (force expensive)))
(defer-expensive
  (delay :cheap)
  (delay (do (Thread/sleep 5000) :expensive)))
(defer-expensive
  (delay false)
  (delay (do (Thread/sleep 5000) :expensive)))

(defn inf-triangles [l]
   {:head (triangle l)
    :tail (delay (inf-triangles (inc l)))})

(defn head [l] (:head l))
(defn tail [l] (force (:tail l)))
(def tri-nums (inf-triangles 1))
(head tri-nums)
(head (tail tri-nums))

(defn taker [n l]
  (loop [t n src l ret []]
    (if (zero? t)
      ret
      (recur (dec t) (tail src) (conj ret (head src))))))
(defn nthr [l n]
  (if (zero? n)
    (head l)
    (recur (tail l) (dec n))))
(taker 10 tri-nums)
(nthr tri-nums 99)

;###  Functional Programming

;     Functions in all their avtars

;     Composite types (collections) are a function of their elements
([:a :b] 0)
(map [:a :b :c :d] #{0 2})

;     First-class functions
;     * Can be created on demand
;     * Can be stored in a data structure
;     * Can be passed as an argument to a function
;     * Can be returned as the value of a function
;     Using composition
(def fifth (comp first rest rest rest rest))
(defn fnth [n]
  (apply comp
         (cons first
               (take (dec n) (repeat rest)))))
(def sixth (fnth 6))
(sixth (range 1 10))

;     Using partial application
((partial + 5) 100 200)








;#### Unsolved mysteries
;
;     * What does **do** mean?
;     * What does **dorun** mean?
