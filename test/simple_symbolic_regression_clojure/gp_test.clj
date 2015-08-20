(ns simple-symbolic-regression-clojure.gp-test
  (:use midje.sweet)
  (:use [simple-symbolic-regression-clojure.gp])
  )

;; helpers for testing

(defn cycler
  [returns]
  (atom (cycle returns))
  )

(defn step-cycler
  [c]
  (let [result (first @c)]
    (do
      (swap! c rest)
      result
  )))

;; a bit of exploration on how to test stochastic functions

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
      (take 7 (repeatedly #(rand-int 1000))) => [8 7 2 5 8 7 2]))
  )

(fact "it works for other random things too"
  (let [stubby (cycler [0.8 0.7 0.2 0.5])]
    (with-redefs [rand (fn [arg] (step-cycler stubby))]
      (take 7 (repeatedly #(rand 9.0))) => [0.8 0.7 0.2 0.5 0.8 0.7 0.2]))
  )

;; random token

(fact "random-token produces a random token, including ERCs if told to"
    (random-token [1]) => 1
    (into #{} (repeatedly 100 #(random-token [1 2 3 4]))) => #{1 2 3 4}
    (into #{} (repeatedly 100 #(random-token ['(rand-int 5)]))) => #{0 1 2 3 4}
    (into #{} (repeatedly 100 #(random-token [:x :+ '(rand-int 5)]))) => #{0 1 2 3 4 :x :+}
  )

(fact "random-token returns nil when passed an empty collection"
    (random-token []) => nil
  )

;; random script

(fact "random-script creates a vector of a given number of calls to random-tokens"
  (let [stubby (cycler [9 :x])]
    (with-redefs [random-token (fn [args] (step-cycler stubby))]
      (random-script [1 2 3 4 5] 8) => [9 :x 9 :x 9 :x 9 :x]
  )))

;; uniform crossover

(fact "uniform crossover takes two collections and picks the value at each position with equal probability from each parent"
  (let [stubby (cycler [0.0 1.0])] ;; start with mom, alternate with dad
    (with-redefs [rand (fn [] (step-cycler stubby))]
      (uniform-crossover [1 1 1 1 1 1 1 1 1 1] [2 2 2 2 2 2 2 2 2]) => [2 1 2 1 2 1 2 1 2]))
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
    (uniform-mutation [1 1 1] [4] 1.0) => [4 4 4]
    (uniform-mutation [1 1 1] [4] 0.0) => [1 1 1]
    (into #{} (uniform-mutation [1 1 1 1 1 1 1 1 1 1] [4 5] 1.0)) => #{4 5}
    (into #{} (uniform-mutation [1 1 1 1 1 1 1 1 1 1] ['(+ 9 (rand-int 2))] 1.0)) => #{9 10}
  )

(fact "uniform mutation works with empty collections"
    (uniform-mutation [] [4] 1.0) => []
    (uniform-mutation [1 1 1 1] [] 1.0) => [1 1 1 1]
    )

;; uneven one-point-crossover

(fact "one-point crossover takes two collections and splices them together at a randomly chosen internal point"
  (let [stubby (cycler [3 3 2 4 1 5 7 0 0 7])] ;; these will be used in pairs to pick slice points
    (with-redefs [rand-int (fn [arg] (step-cycler stubby))]
      (one-point-crossover [1 1 1 1 1 1 1 1] [2 2 2 2 2 2 2 2]) => [1 1 1 2 2 2 2 2]
      (one-point-crossover [1 1 1 1 1 1 1 1] [2 2 2 2 2 2 2 2]) => [1 1 2 2 2 2]
      (one-point-crossover [1 1 1 1 1 1 1 1] [2 2 2 2 2 2 2 2]) => [1 2 2 2]
      (one-point-crossover [1 1 1 1 1 1 1 1] [2 2 2 2 2 2 2 2]) => [1 1 1 1 1 1 1 2 2 2 2 2 2 2 2]
      (one-point-crossover [1 1 1 1 1 1 1 1] [2 2 2 2 2 2 2 2]) => [2]
      ))
  )

;; Individuals

(facts "can construct an Individual and access its script and score"
       (fact "works with non-nil scores"
             (let [script [:x :y +]
                   individual (make-individual script 12)]
               (:script individual) => script
               (:score individual) => 12))
       (fact "works with nil score"
             (let [script [:x :y +]
                   individual (make-individual script nil)]
               (:script individual) => script
               (:score individual) => nil))
       (fact "works with no score given"
             (let [script [:x :y +]
                   individual (make-individual script)]
               (:script individual) => script
               (:score individual) => nil))
       )

(fact "can set the score of an individual"
      (let [script [:x :y +]
            score 27
            individual (make-individual script)
            scored-individual (set-score individual score)]
        (:script individual) => script
        (:score individual) => nil
        (:script scored-individual) => script
        (:score scored-individual) => score
        ))

(fact "can create a random individual (unscored)"
      (into #{} (:script (random-individual ['(rand-int 7) :x + - * /] 1000))) =>
        #{0 1 2 3 4 5 6 :x + - * /}
      )

;; selection

(fact "given a set of Individuals, with unique scores, I can return the lowest-scoring one"
  (let [dudes [(make-individual [1] 12) 
               (make-individual [2] 2)
               (make-individual [2] 1)]]
    (count (winners dudes)) => 1
    (:score (first (winners dudes))) => 1
  ))

(fact "given a set of Individuals, I can return all the lowest-scoring ones"
  (let [dudes [(make-individual [1] 1) 
               (make-individual [2] 200)
               (make-individual [3] 1)
               (make-individual [4] 1)]]
    (count (winners dudes)) => 3
    (sort (map :script (winners dudes))) => [[1] [3] [4]]
  ))

(fact "given a set of Individuals, some without scores, I will not return the scoreless ones"
  (let [dudes [(make-individual [1] nil) 
               (make-individual [2] 2)
               (make-individual [2] 1)]]
    (count (winners dudes)) => 1
    (:score (first (winners dudes))) => 1
  ))

(fact "given a set of Individuals, none with scores, I will return an empty list"
  (let [dudes [(make-individual [1] nil) 
               (make-individual [2] nil)
               (make-individual [2] nil)]]
    (count (winners dudes)) => 0
  ))


;; exploratory

(def population 
  (atom (into #{} 
    (repeatedly 10 
      #(make-individual (random-script ['(rand-int 100) '(rand) '(rand-int 1000) '(rand-int 10) :+ :- :* :÷ :x] 10) nil))
    )))

; (def x-plus-6-rubrics 
;   (repeatedly 20 #(let [x (rand-int 100)] (Rubric. {:x x} (+ 6 x)))))

; (println x-plus-6-rubrics)