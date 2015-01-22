;### Recursion
(defn sum
  ([values]
   (sum values 0))
  ([values accumulator]
   (if (empty? values)
     accumulator
     (recur (rest values) (+ (first values) accumulator)))))
; Since clojure doesn't provide tail call optimization, use **recur** instead of explicitly
; calling the function recursively.

(sum [1 2 3])

;### Functional composition
((comp clojure.string/lower-case clojure.string/trim) "  Unclean String  ")

(defn two-comp [f g]
  (fn [& args]
    (f (apply g args))))
((two-comp clojure.string/lower-case clojure.string/trim) "  Unclean String  ")

(defn my-comp
  ([] identity)
  ([& fns]
   (let [fns (reverse fns)]
     (fn [& args]
       (loop [ret (apply (first fns) args)
              fns (next fns)]
         (if fns
           (recur ((first fns) ret) (next fns))
           ret))))))

(defn negate [x]
  (* -1 x))

((my-comp) 1)
((my-comp negate) 1)
((my-comp negate negate) 1)
((my-comp negate negate negate) 1)

;### Memoization
; Referential transparancy allows for memoization
; Following methods sleeps for 1 second before returning
(defn sleepy-identity [x]
  (Thread/sleep 1000)
  x)

; but it could be memoized so that subsequent calls with the same arguments
; return immediately
(def memo-sleepy-identity (memoize sleepy-identity))
