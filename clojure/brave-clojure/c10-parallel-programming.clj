;### Futures

(future (Thread/sleep 4000)
        (prn "I selpt for a while."))

; "future" does the task provided to it in a different thread,
; preventing the current thread from blocking.
(defn demo-future []
  (let [res (future (prn "This should be printed only once")
                    (+ 1 1))]
    (prn "deref res" (deref res))
    (prn "@res" @res)))

; dereferencing a future would block if it hasn't finished.
(defn wait-future []
  (let [res (future (Thread/sleep 4000)
                    (prn "This should be printed only once")
                    (+ 1 1))]
    (prn "The result is " @res)
    (prn "Would have to wait for a while before it gets printed.")))

;### Timing out on futures
; while derefing a future, a timeout and a default value to return
; in case of timeout could be provided
(defn timeout-future []
  (let [res (future (Thread/sleep 10000)
                    (+ 1 1))]
    (prn "The result is " (deref res 3000 0))))

; a future can be interrogated to know if it has completed by **realized?**
(realized? (future (Thread/sleep 1000)))

;### Delay
; when a task is started inside a future, it starts immediately.
; but the task could be delayed by **delay**
(def jackson-5-delay
  (delay (let [msg "Just a dumb message"]
           (prn "Printed msg:" msg)
           msg)))
; this delay could be realized using either **deref** or **force**
; using force makes it more explicit that you are causing a task to start
; instead of waiting for it to finish
(force jackson-5-delay)

; like future, delay is run only once and the result is cached.
; One way you can use a delay is to fire off a statement the
; first time one future out of a group of related futures finishes.
; For example, pretend your app uploads a set of headshots to a
; headshot-sharing site and notifies the owner as soon as the
; first one is up, as in the following:

(def gimli-headshots ["serious.jpg" "fun.jpg" "playful.jpg"])
(defn email-user [email-address]
  (prn "Sending email notification to " email-address))
(defn upload-document [document]
  (prn "Uploading " document)
  true)
(defn start-uploading []
  (let [notify (delay (email-user "aaabc@ddd.com"))]
    (doseq [headshot gimli-headshots]
      (future (upload-document headshot)
              (force notify)))))
; in the above example, though "notify" would be forced thrice,
; but the actual task of sending the email would take place only once
; the other two times would just used the __cached__ result.

; delay helps solve the mutual exclusion problem, but i don't have a clue how.

;### Promise
; Promises allow you to express the expectation of a result independently
; of the task that should produce it and when that task should run. You
; create promises with promise and deliver a result to them with deliver.
; You obtain the result by dereferencing:

(def my-promise (promise))
(deliver my-promise (+ 2 3))
(prn @my-promise)
; A promise could be delivered only once and dereferencing would block
; till the result is available.

; Let's try to search something in a collection of data.
(def yak-butter-international
  {:store "Yak Butter International"
    :price 90
    :smoothness 90})
(def butter-than-nothing
  {:store "Butter than Nothing"
   :price 150
   :smoothness 83})
;; This is the butter that meets our requirements
(def baby-got-yak
  {:store "Baby Got Yak"
   :price 94
   :smoothness 99})

(defn mock-api-call
  [result]
  (Thread/sleep 1000)
  result)

(defn satisfactory?
  "If the butter meets our criteria, return the butter, else return false"
  [butter]
  (and (<= (:price butter) 100)
       (>= (:smoothness butter) 97)
       butter))

; if we did it synchronously
(time (some (comp satisfactory? mock-api-call)
      [yak-butter-international butter-than-nothing baby-got-yak]))

; we could parallize this using futures and promise
(time
  (let [butter-promise (promise)]
    (doseq [butter [yak-butter-international butter-than-nothing baby-got-yak]]
      (future (if-let [satisfactory-butter (satisfactory? (mock-api-call butter))]
                (deliver butter-promise satisfactory-butter))))
    (prn "And the winner is: " @butter-promise)))

; the above could be used as a way to protect against reference cell concurrency
; problem, because a cell could be written to only once.

; promises could also be used to register callbacks
(let [ferengi-wisdom-promise (promise)]
  (future (prn "Here's some ferengi wisdom:" @ferengi-wisdom-promise))
  (Thread/sleep 1000)
  (deliver ferengi-wisdom-promise "Whisper your way to success."))


;### Atoms
; updates on atoms take place synchronously.
(def fred (atom {:cuddle-hunger-level 0
                 :percent-deteriorated 0}))

(let [zombie-state @fred]
  (if (>= (:percent-deteriorated zombie-state) 50)
    (future (println (:percent-deteriorated zombie-state)))))

(swap! fred
       (fn [current-state]
         (merge-with + current-state {:cuddle-hunger-level 1})))

(defn increase-cuddle-hunger-level [zombie-state increase-by]
  (merge-with + zombie-state {:cuddle-hunger-level increase-by}))

(swap! fred increase-cuddle-hunger-level 10)
(swap! fred update-in [:cuddle-hunger-level] + 10)

(reset! fred {:cuddle-hunger-level 0
              :percent-deteriorated 0})

;### Watches and Validators
; watches are functions which are triggered everytime a reference type changes.

(defn shuffle-speed [zombie]
  (* (:cuddle-hunger-level zombie)
     (- 100 (:percent-deteriorated zombie))))

(defn shuffle-alert [key watched old-state new-state]
  (let [sph (shuffle-speed new-state)]
    (if (> sph 5000)
      (do (prn "Run, you fool, new speed: " sph " key: " key))
      (do (prn "All's well with key: " key " sph: " sph)))))

(add-watch fred :fred-shuffle-alert shuffle-alert)
(swap! fred update-in [:percent-deteriorated] + 1)

; validators are a mechanism to specify what states are allowd for a reference type.

(defn percent-deteriorated-validator [{:keys [percent-deteriorated]}]
  (and (>= percent-deteriorated 0)
       (<= percent-deteriorated 100)))
; the validator could be attached at atom creation
(def bobby (atom
             {:cuddle-hunger-level 0 :percent-deteriorated 0}
             :validator percent-deteriorated-validator))
(swap! bobby update-in [:percent-deteriorated] + 200)

; throw an exception to get a more descriptive error message.
(defn percent-deteriorated-validator [{:keys [percent-deteriorated]}]
  (or (and (>= percent-deteriorated 0)
           (<= percent-deteriorated 100))
      (throw (IllegalStateException. "That's not mathy!"))))
(def bobby (atom
             {:cuddle-hunger-level 0 :percent-deteriorated 0}
             :validator percent-deteriorated-validator))
(swap! bobby update-in [:percent-deteriorated] + 200)


;### Refs
; refs allow updating state of multiple identities using transaction semantics
; these transaction semantics are Atomic, Consistent and Isolated (ACI)

(def sock-varieties
  #{"darned" "argyle" "wool" "horsehair" "mulleted"
    "passive-aggressive" "striped" "polka-dotted"
    "athletic" "business" "power" "invisible" "gollumed"})

(defn sock-count
  [sock-variety sock-count]
  {:variety sock-variety
   :count sock-count})

(defn generate-sock-gnome
  "Create an initial sock gnome state with no socks"
  [sock-name]
  {:name sock-name
   :socks #{}})

(def sock-gnome (ref (generate-sock-gnome "Barumpharumph")))
(def dryer (ref {:name "LG 1337"
                 :socks (set (map #(sock-count % 2) sock-varieties))}))

(:socks @dryer)

; alter
(defn steal-sock [gnome dryer]
  (dosync
    (when-let [pair (some #(if (= (:count %) 2) %) (:socks @dryer))]
      (let [updated-count (sock-count (:variety pair) 1)]
        (alter gnome update-in [:socks] conj updated-count)
        (alter dryer update-in [:socks] disj pair)
        (alter dryer update-in [:socks] conj updated-count)))))

(steal-sock sock-gnome dryer)
(:socks @dryer)
(:socks @sock-gnome)

(defn similar-socks [target-sock sock-set]
  (filter #(= (:variety %) (:variety target-sock)) sock-set))

(similar-socks (first (:socks @sock-gnome)) (:socks @dryer))

; when alter is called on a ref, the change isn't immediately visible
; outside the current transaction. but this change is of-course visible
; inside this transaction
; to illustrate this point, look at the example below

(def counter (ref 0))
(future
  (dosync
   (alter counter inc)
   (println "Thread" @counter)
   (Thread/sleep 500)
   (alter counter inc)
   (println "Thread" @counter)))
(Thread/sleep 250)
(println "Main" @counter)
(Thread/sleep 750)
(println "Main" @counter)

; the transaction would only commit its changes when it ends.

; commute
; commute has slightly looser semantics about retrying.
; it has better performance than alter, but not all operations
; are commutable.

; safe commuting
(defn sleep-print-update [sleep-time thread-name update-fn]
  (fn [state]
    (Thread/sleep sleep-time)
    (prn (str thread-name ": " state))
    (update-fn state)))

(def counter (ref 0))
(future (dosync (commute counter (sleep-print-update 100 "Thread A" inc))))
(future (dosync (commute counter (sleep-print-update 150 "Thread B" inc))))

; unsafe commuting

(def receiver-a (ref #{}))
(def receiver-b (ref #{}))
(def giver (ref #{1}))
(future (dosync (let [gift (first (seq @giver))]
                  (Thread/sleep 10)
                  (commute receiver-a conj gift)
                  (commute giver disj gift))))
(future (dosync (let [gift (first (seq @giver))]
                  (Thread/sleep 15)
                  (commute receiver-b conj gift)
                  (commute giver disj gift))))

; more to read about refs: "ensure" and "write skew"

;### Vars
; dynamic vars could be created whose binding could be changed
(def ^:dynamic *notification-address* "dobby@elf.org")
; for creating dynamic var, you have to use ^:dynamic and surround
; them with earmuffs.

; the binding could be temporarily changed as:
(binding [*notification-address* "test@elf.org"]
  *notification-address*)

; usages of vars
; suppose you want to spec a notification mail
(defn notify [message]
  (str "TO: " *notification-address* "\n"
       "MESSAGE: " message))
(notify "test message")
; but since you wouldn't want to send the mail to real address during
; spec, use binding
(binding [*notification-address* "test@elf.org"]
  (notify "test message"))

; more real-life scenario is changing the dynamic binding
; of resources such as standard output

(binding [*out* (clojure.java.io/writer "print-output")]
  (println "A man who carries a cat by the tail learns
           something he can learn in no other way.
           -- Mark Twain"))
(slurp "print-output")

; dynamic vars are also used for configuration.
(binding [*print-length* 1]
  (println ["Print" "just" "one!"]))

; it's also possible to set! dynamic vars which have been bound.
(def ^:dynamic *troll-thought* nil)
(defn troll-riddle [your-answer]
  (let [number "man meat"]
    (when (thread-bound? #'*troll-thought*)
      (set! *troll-thought* number))
    (if (= number your-answer)
      "TROLL: You can cross the bridge!"
      "TROLL: Time to eat you, succulent human!")))
(binding [*troll-thought* nil]
  (println (troll-riddle 2))
  (println "SUCCULENT HUMAN: OOOOH! The answer was" *troll-thought*))

;### pmap
(def alphabet-length 26)

(def letters (mapv (comp str char (partial + 65)) (range alphabet-length)))

(defn random-string [length]
  (apply str (take length (repeatedly #(rand-nth letters)))))

(defn random-string-list [list-length string-length]
  (doall (take list-length (repeatedly (partial random-string string-length)))))

(def orc-names (random-string-list 3000 7000))

(time (dorun (map clojure.string/lower-case orc-names)))
(time (dorun (pmap clojure.string/lower-case orc-names)))

; sometimes the overhead of managing concurrent threads may dominate the
; actuall task time. in such situations, increase the grain size.
(def numbers [1 2 3 4 5 6 7 8 9 10])
(partition-all 3 numbers)
(pmap inc numbers)
(apply concat
       (pmap (fn [number-group] (doall (map inc number-group)))
             (partition-all 3 numbers)))
(time
  (dorun
    (apply concat
           (pmap (fn [name] (doall (map clojure.string/lower-case name)))
                 (partition-all 1000 orc-names)))))

(defn ppmap [grain-size f & colls]
  (apply concat
         (apply pmap
                (fn [& pgroups] (doall (apply map f pgroups)))
                (map (partial partition-all grain-size) colls))))
(time (dorun (ppmap 1000 clojure.string/lower-case orc-names)))

;### clojure.core.reducers
(require '[clojure.core.reducers :as r])
(def numbers (vec (range 1000000)))

; fold gives a much better performance over reduce for vectors and maps.
; but the performance is comparable for other data types.
(time (reduce + numbers))
(time (r/fold + numbers))

; reducers improve the performance of **map** and **filter** when applied
; in succession.
(time (dorun (map inc numbers)))
(time (dorun (into [] (r/map inc numbers))))

(time (reduce + (r/filter odd? (r/map inc numbers))))

(time (dorun (filter odd? (map inc numbers))))
(time (dorun (into [] (r/filter odd? (r/map inc numbers)))))

; another benefit of using r/map, r/filter is that on passing the result to
; r/fold does parallel reduction, whereas passing output of map, filter would
; do serial reduction on passing to r/fold


