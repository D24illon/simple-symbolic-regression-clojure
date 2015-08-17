(ns simple-symbolic-regression-clojure.gp-test
  (:use midje.sweet)
  (:use [simple-symbolic-regression-clojure.gp])
  (:use (bigml.sampling [simple :as simple]))
  )

;; helpers for testing

(defn cycler
  [numbers]
  (atom {:step 0, :values (cycle numbers)})
  )

(defn step-cycler
  [c]
  (let [result (nth (:values @c) (:step @c))]
    (do
      (swap! c assoc :step (inc (:step @c)))
      result
  )))

;; a bit of exploration on how to test stochastic functions

(fact "we can select a subsample of a collection using bigml.sampling"
  (count (take 5 (simple/sample (range 1000)))) => 5
  ; (take 5 (simple/sample (range 1000))) => [some collection of 5 numbers]
  )

(fact "but we can stub that by name"
  (with-redefs [simple/sample (fn [s] (cycle [1 2]))]
    (take 5 (simple/sample (range 1000))) => (just [1 2 1 2 1]))
  )
 
(fact "we can stub core random number generation if we want to"
  (with-redefs [rand-int (fn [arg] 88)]
    [(rand-int 11) (rand-int 22) (rand-int 33)] => (just [88 88 88]))
  )

(fact "we can call step-cycler to produce a number from the cycler"
  (let [cc (cycler [-1 2 -3 4])]
    (step-cycler cc) => -1
    (step-cycler cc) => 2
    (step-cycler cc) => -3
    (step-cycler cc) => 4
    ))

(fact "we can thus stub random number generation with a cycler"
  (let [stubby (cycler [8 7 2 5])]
    (with-redefs [rand-int (fn [arg] (step-cycler stubby))]
      (take 7 (repeatedly #(rand-int 1000))) => (just [8 7 2 5 8 7 2])))
  )

(fact "it works for other random things too"
  (let [stubby (cycler [0.8 0.7 0.2 0.5])]
    (with-redefs [rand (fn [arg] (step-cycler stubby))]
      (take 7 (repeatedly #(rand 9.0))) => (just [0.8 0.7 0.2 0.5 0.8 0.7 0.2])))
  )

;; random token

(fact "random-token produces a random token, including ERCs if told to"
    (random-token [1]) => 1
    (into #{} (repeatedly 100 #(random-token [1 2 3 4]))) => (just [1 2 3 4])
    (into #{} (repeatedly 100 #(random-token ['(rand-int 5)]))) => (just [0 1 2 3 4])
    (into #{} (repeatedly 100 #(random-token [:x :+ '(rand-int 5)]))) => (just [0 1 2 3 4 :x :+])
  )

(fact "random-token returns nil when passed an empty collection"
    (random-token []) => nil
  )

;; uniform crossover

(fact "uniform crossover takes two collections and picks the value at each position with equal probability from each parent"
  (let [stubby (cycler [0.0 1.0])] ;; start with mom, alternate with dad
    (with-redefs [rand (fn [] (step-cycler stubby))]
      (uniform-crossover [1 1 1 1 1 1 1 1 1 1] [2 2 2 2 2 2 2 2 2]) => (just [2 1 2 1 2 1 2 1 2])))
  )

(fact "uniform crossover works with different-length collections"
  (count (uniform-crossover [1 1 1 1 1] [2 2 2 2 2 2 2 2 2])) => 5
  (count (uniform-crossover [1 1 1 1 1] [2])) => 1
  )

(fact "uniform crossover works with empty collections"
  (count (uniform-crossover [1 1 1 1 1] [])) => 0
  (count (uniform-crossover [] [1 1 1 1 1])) => 0
  )


;; uniform mutation

(fact "uniform mutation takes a collection, and changes each position to a new sampled value with specified probability"
    (uniform-mutation [1 1 1] [4] 1.0) => (just [4 4 4])
    (uniform-mutation [1 1 1] [4] 0.0) => (just [1 1 1])
    (into #{} (uniform-mutation [1 1 1 1 1 1 1 1 1 1] [4 5] 1.0)) => (just #{4 5})
    (into #{} (uniform-mutation [1 1 1 1 1 1 1 1 1 1] ['(+ 9 (rand-int 2))] 1.0)) => (just #{9 10})
  )

(fact "uniform mutation works with empty collections"
    (uniform-mutation [] [4] 1.0) => (just [])
    (uniform-mutation [1 1 1 1] [] 1.0) => (just [1 1 1 1])
    )

;; uneven one-point-crossover

(fact "one-point crossover takes two collections and splices them together at a randomly chosen internal point"
  (let [stubby (cycler [3 3 2 4 1 5 7 0 0 7])] ;; these will be used in pairs to pick slice points
    (with-redefs [rand-int (fn [arg] (step-cycler stubby))]
      (one-point-crossover [1 1 1 1 1 1 1 1] [2 2 2 2 2 2 2 2]) => (just [1 1 1 2 2 2 2 2])
      (one-point-crossover [1 1 1 1 1 1 1 1] [2 2 2 2 2 2 2 2]) => (just [1 1 2 2 2 2])
      (one-point-crossover [1 1 1 1 1 1 1 1] [2 2 2 2 2 2 2 2]) => (just [1 2 2 2])
      (one-point-crossover [1 1 1 1 1 1 1 1] [2 2 2 2 2 2 2 2]) => (just [1 1 1 1 1 1 1 2 2 2 2 2 2 2 2])
      (one-point-crossover [1 1 1 1 1 1 1 1] [2 2 2 2 2 2 2 2]) => (just [2])
      )) 
  )
