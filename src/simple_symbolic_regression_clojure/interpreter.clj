(ns simple-symbolic-regression-clojure.interpreter
  (:use [clojure.math.numeric-tower])
  )

;;; Interpreter

(defn translate-op [op]
  "Translate operators from the symbolic regression language to
   the appropriate Clojure operator for evaluation. A key goal
   here is replacing +, -, and * with +', -', and *' so we don't
   have unexpected overflow exceptions."
  (condp = op
    :+ +'
    :- -'
    :* *'
    :÷ /
    op))

(defn legal-division-stack? [stack]
  (not (zero? (peek stack))))

(defn legal-binary-op-stack? [op stack]
  (and (>= (count stack) 2)
       (or (not (= op :÷))
           (legal-division-stack? stack))))

(defn process-binary-operator [op stack]
  "Apply a binary operator to the given stack, returning the updated stack"
  (if (legal-binary-op-stack? op stack)
    (let [arg2 (peek stack)
          arg1 (peek (pop stack))
          new-stack (pop (pop stack))]
      (conj new-stack
            ((translate-op op) arg1 arg2)))
    stack))

(defn binary-operator? [token]
  (contains? #{:+ :- :* :÷} token))

; We might consider throwing an exception in the :else
; case instead of returning to help alert (human) programmers
; of errors in things like bindings and undefined tokens.

(defn process-token
   "Process given token returning the updated stack"
  ([stack token]
   (process-token {} stack token))
  ([bindings stack token]
   (cond
    (contains? bindings token) (conj stack (bindings token))
    (number? token) (conj stack token)
    (binary-operator? token) (process-binary-operator token stack)
    :else stack)
   ))

(defn run-script [script bindings]
  "loop over every item in the script and modify the stack accordingly"
  (reduce (partial process-token bindings) [] script)
  )

(defn interpret [script bindings]
  (let [stack (run-script script bindings)
        answer (peek stack)]
    {:result answer, :stack stack}))

;;; Rubrics

(defrecord Rubric [input output])

(def score-penalty 100000000000000000000N)

(defn score-on [script rubric]
  (if-let [result (:result (interpret script (:input rubric)))]
    (abs (- (:output rubric) result))
    score-penalty))

(defn total-score-on [_ script rubrics]
  (reduce + (map (partial score-on script) rubrics)))
