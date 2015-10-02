(ns simple-symbolic-regression-clojure.gp
  (:use [simple-symbolic-regression-clojure.interpreter])
  (:require [clojure.core.reducers :as r])
  )

;;; Evolutionary operators


(defn uniform-crossover
  "Takes two collections, and returns a new collection containing items taken with
  equal probability from the two 'parents' at each location. The length of the
  crossover-product is the shorter of the two lengths."
  [mom dad]
  (map (fn [a b] (if (< 0.5 (rand)) a b)) mom dad)
  )


(defn random-token
  "Argument is a list of tokens, including functions to generate values if you like;
  every token is evaluated as it is returned, and they are sampled with uniform probability."
  [tokens]
  (if (empty? tokens)
    nil
    (eval (nth tokens (rand-int (count tokens))))
  ))


(defn uniform-mutation
  "Takes a collections, a vector of tokens (including functions), and a probability;
  resamples eachposition with i.i.d. probability, using the token list to replace them.
  The result will not change length."
  [mom tokens prob]
  (map
    (fn [i]
      (if (or (> (rand) prob) (empty? tokens))
        i
        (random-token tokens)
        ))
    mom)
  )


(defn one-point-crossover
  "Takes two collections, and returns a new collection containing items taken from the
  front of the first and the tail of the second, switching at a randomly-chosen breakpoint
  in each."
  [mom dad]
  (let [mom-cut (rand-int (count mom))
        dad-cut (rand-int (count dad))]
    (concat (take mom-cut mom) (drop dad-cut dad))
  ))


;;; Individuals


(defrecord Individual [script score])


(defn make-individual
  ([script]
   (make-individual script nil))
  ([script score]
   (->Individual script score)))


(defn set-score [individual score]
  ;(let [score-agent (agent nil)]
  (assoc individual :score score))

(defn get-score [individual]
  (when-not (deref (:score individual))
    (await (:score individual)))
  (deref (:score individual)))


;;; Generating random scripts, individuals, etc.


(defn random-script
  "Takes a collection of token-generators and a size, and samples the generators using
  random-token to produce a vector of `size` samples."
  [token-list size]
  (repeatedly size #(random-token token-list))
  )


(defn random-individual
  "takes a token list and size, and returns an un-scored Individual"
  [tokens size]
  (make-individual (random-script tokens size)))


(defn random-population
  [pop-size constructor-fn]
  (repeatedly pop-size constructor-fn))


;;; Scoring (this is where the parallelism probably wants to go!)


(defn score-using-rubrics
  "assigns the score value of an Individual by invoking `total-score-on` a set of Rubrics"
  [individual rubrics]
  (let [score-agent (agent nil)]
   (set-score individual (send score-agent total-score-on (:script individual) rubrics))))


(defn score-population
  "takes an unscored population and returns the same ones with scores assigned"
  [population rubrics]
  (map #(score-using-rubrics % rubrics) population))


;;; Main evolutionary loop


; TODO: This is too problem specific and should be in core not in gp,
; which will require making it an argument to make-baby?
(def token-generator
  ['(rand-int 100) :x :+ :- :* :÷])


(defn make-unscored-baby
  "creates a new *unscored* Individual by sampling a population (with uniform probability) and applying one-pt crossover and mutation"
  [population mutation-rate]
  (let [mom (:script (rand-nth population))
        dad (:script (rand-nth population))
        crossover (if (< (rand) 0.5) one-point-crossover uniform-crossover)
        baby-script (uniform-mutation
                     (crossover mom dad)
                     token-generator
                     mutation-rate)]
    (make-individual baby-script)))


(defn one-seasonal-cycle
  "doubles the population size by calling `make-baby`, sorts the entire population by score (worse is bigger), removes the worst-scoring ones"
  [population mutation-rate rubrics]
  (let [carrying-capacity (count population)
        new-brood (repeatedly
                    carrying-capacity
                    #(make-unscored-baby population mutation-rate))
        scored-brood (score-population new-brood rubrics)]
    (take carrying-capacity (sort-by get-score (concat population scored-brood)))
    ))


(defn future-history
  "creates a lazy list of iterations by applying `one-seasonal-cycle` to an initial population"
  [initial-pop mutation-rate rubrics]
  (iterate #(one-seasonal-cycle % mutation-rate rubrics) initial-pop))


(defn winners
  "takes a list of Individuals, scored, and returns
  a list containing all the Individuals with the lowest non-nil score;
  if no Individual has been scored, it returns an empty list"
  [individuals]
  (let [scored-ones (filter #(some? (get-score %)) individuals)
        best (get-score (first (sort-by get-score scored-ones)))]
    (filter #(= best (get-score %)) scored-ones)
  ))
