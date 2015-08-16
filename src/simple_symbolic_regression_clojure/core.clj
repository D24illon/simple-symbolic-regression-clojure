(ns simple-symbolic-regression-clojure.core)

(defn translate-op [op]
  "Translate operators from the symbolic regression language to
   the appropriate Clojure operator for evaluation. A key goal
   here is replacing +, -, and * with +', -', and *' so we don't
   have unexpected overflow exceptions."
  (condp = op
    + +'
    - -'
    * *'
    op))

(defn legal-division-stack? [stack]
  (not (zero? (peek stack))))

(defn legal-binary-op-stack? [op stack]
  (and (>= (count stack) 2)
       (or (not (= op /))
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
  (contains? #{+ - * /} token))

(defn process-token [stack token]
  "Process given token returning the updated stack"
  (cond
   (number? token) (conj stack token)
   (binary-operator? token) (process-binary-operator token stack)
   :else stack)
  )

(defn run-script [script bindings]
  "loop over every item in the script and modify the stack accordingly"
  (reduce process-token [] script)
  )

(defn interpret [script bindings]
  (let [stack (run-script script bindings)
        answer (peek stack)]
    {:result answer, :stack stack}))
