;### Reader Macros
(read-string "(+ 3 4)")
(read-string "#(+ 1 %)")
(read-string "'(a b c)")
(read-string "@var")
(read-string "; ignore-me\n(+ 1 2)")

;### Evaluation
(def addition-list (list + 1 2))
(eval addition-list)

(eval (read-string "\"t\""))
(eval "t")

(eval (read-string "true"))
(eval (read-string ":zsc"))
(eval (read-string "()"))
(eval (read-string "(if true 1 2)"))

;### Defining Macros
(defmacro ignore-last-operand [func-call]
  (butlast func-call))
(ignore-last-operand (+ 1 2 3))
(ignore-last-operand (+ 1 2 (prn "Won't be printed")))

; When you call a function, each of its operands is evaluated
; before being passed to the function as an argument. By contrast,
; when you call a macro, the operands are not evaluated.
; The data structure returned by a function is not evaluated,
; but the data structure returned by a macro is.
(macroexpand '(ignore-last-operand (+ 1 2 3)))
(macroexpand '(ignore-last-operand (+ 1 2 (prn "Won't be printed"))))

(when :in-doubt "do something")
(macroexpand '(when :in-doubt "do something"))
(eval (macroexpand '(when :in-doubt "do something")))

(defmacro backwards [form]
  (reverse form))

(backwards (" Bond" "I am" str))
