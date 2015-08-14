(ns simple-symbolic-regression-clojure.core-test
  (:use midje.sweet)
  (:use [simple-symbolic-regression-clojure.core]))

(fact "it gets a empty program and an empty binding it returns nil with an empty stack"
      (interpret [] {}) => {:result nil, :stack []})

(fact "the interpreter gets script [7] and no binding it returns 7 with stack [7]"
      (interpret [7] {}) => {:result 7, :stack [7]})

(fact "process-token puts the token onto the stack if its a literal"
      (process-token [] 8) => (just [8])
      (peek (process-token [1 2 3] 4)) => 4
      (process-token [1 2 3] 4) => (just [1 2 3 4]))

(fact "run-script should process a list of literals by pushing them into the stack"
      (run-script [] {}) => []
      (run-script [1 2 3] {}) => [1 2 3])


(facts "about process-token with +"
       (fact "process-token with + and an empty stack"
             (process-token [] +) => [])
       (fact "process-token with + and only one number"
             (process-token [7] +) => [7])
       (fact "process-token with + and two numbers"
             (process-token [3 5] +) => [8]
             (process-token [1.2 -3.4] +) => [-2.2]
             (process-token [2/3 5/6] +) => [3/2]
             )
       )

(facts "about process-token with -"
       (fact "process-token with - and an empty stack"
             (process-token [] -) => [])
       (fact "process-token with - and only one number"
             (process-token [7] -) => [7])
       (fact "process-token with - and two numbers"
             (process-token [3 5] -) => [-2]
             (process-token [1.2 -3.4] -) => [4.6]
             (process-token [2/3 5/6] -) => [-1/6]
             )
       )


;; interpreter

(fact "the interpreter does addition"
      (interpret [1 2 +] {}) => {:result 3, :stack [3]}
      (interpret [1 +] {}) => {:result 1, :stack [1]}
      (interpret [+] {}) => {:result nil, :stack []}
      (interpret [1 2 3 4 +] {}) => {:result 7, :stack [1 2 7]}
      (interpret [1 2 3 + 4 5 7 +] {}) => {:result 12, :stack [1 5 4 12]}
      (interpret [1 2 3 4 5 + + +] {}) => {:result 14, :stack [1 14]}
      )
