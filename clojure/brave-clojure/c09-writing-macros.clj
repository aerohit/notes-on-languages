(defmacro postfix
  "I'm too indie for a prefix notation"
  [expression]
  (conj (butlast expression) (last expression)))

(postfix (1 2 +))
(macroexpand '(postfix (1 2 +)))

(defmacro code-critic [{:keys [good bad]}]
  (list 'do
        (list 'println
              "This is bad code:"
              (list 'quote bad))
        (list 'println
              "This is good code:"
              (list 'quote good))))

(code-critic {:good (+ 1 1) :bad (1 + 1)})

(defmacro my-when [condition & body]
  (list 'if condition (cons 'do body)))

(macroexpand '(when (the-cows-come :home)
                (call me :pappy)
                (slap me :silly)))

(defmacro unless [test & branches]
  (conj (reverse branches) test 'if))

(macroexpand '(unless (done-been slapped? me)
                      (slap me :silly)
                      (say "I reckon that'll learn me")))

;### Syntax quoting
(defmacro code-critic [{:keys [good bad]}]
  `(do (println "This is bad code:"
                (quote ~bad))
       (println "This is good code:"
                (quote ~good))))

(defmacro code-praiser [code]
  (list 'println
        "Sweet gorilla of Manila, this is good code:"
        (list 'quote code)))
(defmacro code-praiser [code]
  `(println
     "Sweet gorilla of Manila, this is good code:"
     (quote ~code)))

(defn criticize-code [criticism code]
  `(println ~criticism (quote ~code)))

(defmacro code-critic [{:keys [good bad]}]
  `(do ~(criticize-code "This is bad code: " bad)
       ~(criticize-code "This is good code: " good)))

(macroexpand '(code-critic {:good (+ 1 1) :bad (1 + 1)}))

;### Unquote splicing
(defmacro code-critic [{:keys [good bad]}]
  `(do ~@(map #(apply criticize-code %)
             [["This is bad code: " bad]
              ["This is good code: " good]])))

(macroexpand '(code-critic {:good (+ 1 1) :bad (1 + 1)}))
