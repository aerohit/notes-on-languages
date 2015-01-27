(ns playsync.core
  (:require [clojure.core.async
             :as a
             :refer [>! <! >!! <!! go chan buffer close! thread
                     alts! alts!! timeout]]))

(def echo-chan (chan))
(go (println (<! echo-chan)))
(>!! echo-chan "ketchup")

; core.async also supports buffered channels, sliding-buffer and dropping-buffer.


;### Go blocks, thread, blocking and parking

; There are two varieties of waiting: parking and blocking. Blocking
; is the kind of waiting you're familiar with: a thread stops execution
; until a task is complete. Usually this happens when you're performing
; some kind of I/O. This kind of waiting keeps the thread alive, doing
; no work, so that if you want your program to continue doing work you
; have to create a new thread. In the last chapter, you learned how to
; do this with future.

; Parking moves the waiting task off the thread, freeing up the thread
; to do the work of processes that aren't waiting. Clojure's smart enough
; to move the parked process back on to a thread as soon its put or take
; is done. It's like parking allows interleaving on a single thread,
; similar to the way that using multiple threads allows interleaving
; on a single core:

; Parking is only possible within go blocks, and it's only possible when
; you use >! and <!, or parking put and parking take. As you've no doubt
; guessed, >!! and <!! are blocking put and blocking take.

; There are definitely times when you should prefer blocking over parking,
; like when your process will take a long time before putting or taking,
; and for those occasions you should use thread:

(thread (println (<!! echo-chan)))
(>!! echo-chan "mustard")

; thread acts almost exactly like future: it creates a new thread,
; executing a process on that thread. However, instead of returning
; an object which you can dereference, thread returns a channel.
; When thread's process stops, the process's return value is put on
; the channel that thread returns:

(let [t (thread "chilli")]
  (<!! t))

; The reason you should use thread instead of a go block when you're
; performing a long-running task is so that you don't "clog up" your thread pool.


;### Hotdog machine
(defn hotdog-machine-v1 []
  (let [money-channel (chan)
        hotdog-channel (chan)]
    (go (<! money-channel)
        (>! hotdog-channel "hotdog"))
    [money-channel hotdog-channel]))

(let [[in out] (hotdog-machine-v1)]
  (>!! in "pocket lint")
  (<!! out))


(defn hotdog-machine-v2 [hotdog-count]
  (let [in (chan)
        out (chan)]
    (go (loop [hc hotdog-count]
          (if (> hc 0)
            (let [input (<! in)]
              (if (= 3 input)
                (do (>! out "hotdog")
                    (recur (dec hc)))
                (do (>! out "wilted lettuce")
                    (recur hc))))
            (do (close! in)
                (close! out)))))
    [in out]))

(let [[in out] (hotdog-machine-v2 2)]
  (>!! in "pocket lint")
  (println (<!! out))

  (>!! in 3)
  (println (<!! out))

  (>!! in 3)
  (println (<!! out))

  (>!! in 3)
  (<!! out))



; process pipelines
(let [c1 (chan)
      c2 (chan)
      c3 (chan)]
  (go (>! c2 (clojure.string/upper-case (<! c1))))
  (go (>! c3 (clojure.string/reverse (<! c2))))
  (go (println (<! c3)))
  (>!! c1 "redrum"))


;### Choice
(defn upload [headshot c]
  (go (Thread/sleep (rand 100))
      (>! c headshot)))

(let [c1 (chan)
      c2 (chan)
      c3 (chan)]
  (upload "serious.jpg" c1)
  (upload "fun.jpg" c2)
  (upload "sassy.jpg" c3)
  (let [[headshot channel] (alts!! [c1 c2 c3 (timeout 50)])]
    (if headshot
      (println "Sending headshot notification for" headshot)
      (println "Timed out!"))))

;### Queues

(defn append-to-file
  [filename s]
  (spit filename s :append true)
  (println s)
  )

(defn format-quote
  [quote]
  (str "=== BEGIN QUOTE ===\n" quote "=== END QUOTE ===\n\n"))

(defn random-quote
  []
  (format-quote (slurp "http://www.iheartquotes.com/api/v1/random")))

(defn snag-quotes [filename num-quotes]
  (let [c (chan)]
    (go (while true) (append-to-file filename (<! c)))
    (dotimes [n num-quotes] (go (>! c (random-quote))))))


(snag-quotes "quotes.txt" 50)


;### Avoiding callbacks ...
; by creating pipelines

(defn upper-caser [in]
  (let [out (chan)]
    (go (while true (>! out (clojure.string/upper-case (<! in)))))
    out))

(defn reverser [in]
  (let [out (chan)]
    (go (while true (>! out (clojure.string/reverse (<! in)))))
    out))

(defn printer [in]
  (go (while true (println (<! in)))))

(def in-chan (chan))
(def upper-caser-out (upper-caser in-chan))
(def reverser-out (reverser upper-caser-out))
(printer reverser-out)

(>!! in-chan "redrum")
(>!! in-chan "repaid")
