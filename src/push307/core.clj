(ns push307.core
  (:gen-class))

;;;;;;;;;;
;; Examples

; An example Push state
(def example-push-state
  {:exec '(0 "hello miller" integer_+ integer_-)
   :integer '(1 2 3 4 5 6 7)
   :string '("abc" "def")
   :input {:in1 4 :in2 6}})

(def example-push-state-input
  {:exec '(in1 "hello miller" integer_+ integer_-)
   :integer '(1 2 3 4 5 6 7)
   :string '("abc" "def")
   :input {:in1 4 :in2 6}})

; An example Push program
(def example-push-program
  '(3 5 integer_* "hello" 4 "world" integer_-))

(def example-push-program-input
  '(3 5 integer_* "hello" in1 "world" integer_-))

(def empty-state-with-in1
  {:exec '()
   :integer '()
   :string '()
   :input {:in1 4}})

(def example-push-program2
  '(3 5 integer_* ("hello" 4) "world" integer_-))

; An example individual in the population
; Made of a map containing, at mimimum, a program, the errors for
; the program, and a total error
(def example-individual
  {:program '(3 5 integer_* "hello" 4 "world" integer_-)
   :errors [8 7 6 5 4 3 2 1 0 1]
   :total-error 37})


;;;;;;;;;;
;; Instructions must all be either functions that take one Push
;; state and return another or constant literals.
(def instructions
  (list
   'in1
   'integer_+
   'integer_-
   'integer_*
   'integer_%
   0
   1
   ))


;;;;;;;;;;
;; Utilities

(def empty-push-state
  {:exec '()
   :integer '()
   :string '()
   :input {}})

(def full-state
  {:exec '(integer_+ integer_-)
   :integer '(2 1 3 4)
   :string '("hi" "hello" "bye")
   :input {:in1 4 :in2 "yeet"}})

(def div-0-state
  {:exec '(+ = -)
   :integer '(1 0 3 4)
   :string '("hi" "hello" "bye")
   :input {:in1 4 :in2 "yeet"}})

(defn push-to-stack
  "Pushes item onto stack in state, returning the resulting state."
  [state stack item]
  ;;:STUB
  (if (= stack :input)
    (assoc state stack (assoc (state stack) (keyword (str "in" (+ 1 (count (keys (state stack))))))
                        item)) 
    (assoc state stack (conj (state stack) item))))

(defn empty-stack?
  "Returns true if the stack is empty in state."
  [state stack]
  ;;:STUB
  (= 0 (count (state stack))))

(defn pop-stack
  "Removes top item of stack, returning the resulting state."
  [state stack]
  ;;:STUB
  (if (empty-stack? state stack)
    state
    (assoc state stack (rest (state stack)))))

(defn peek-stack
  "Returns top item on a stack. If stack is empty, returns :no-stack-item"
  [state stack]
  ;;:STUB
  (if (empty-stack? state stack)
    :no-stack-item
    (first (state stack))))

(defn get-args-from-stacks
  "Takes a state and a list of stacks to take args from. If there are enough args
  on each of the desired stacks, returns a map of the form {:state :args}, where
  :state is the new state with args popped, and :args is a list of args from
  the stacks. If there aren't enough args on the stacks, returns :not-enough-args."
  [state stacks]
  (loop [state state
         stacks stacks
         args '()]
    (if (empty? stacks)
      {:state state :args (reverse args)}
      (let [stack (first stacks)]
        (if (empty-stack? state stack)
          :not-enough-args
          (recur (pop-stack state stack)
                 (rest stacks)
                 (conj args (peek-stack state stack))))))))

;; Original one, I didnt like the reverse statement so I took it out and it works fine, gunna ask him tomorrow
(defn make-push-instruction
  "A utility function for making Push instructions. Takes a state, the function
  to apply to the args, the stacks to take the args from, and the stack to return
  the result to. Applies the function to the args (taken from the stacks) and pushes
  the return value onto return-stack in the resulting state."
  [state function arg-stacks return-stack]
  (let [args-pop-result (get-args-from-stacks state arg-stacks)]
    (if (= args-pop-result :not-enough-args)
      state
      (let [result (apply function (reverse (:args args-pop-result))) ;; this reverse line dont make sense to me
            new-state (:state args-pop-result)]
        (push-to-stack new-state return-stack result)))))

(defn tmake-push-instruction
  "A utility function for making Push instructions. Takes a state, the function
  to apply to the args, the stacks to take the args from, and the stack to return
  the result to. Applies the function to the args (taken from the stacks) and pushes
  the return value onto return-stack in the resulting state."
  [state function arg-stacks return-stack]
  (let [args-pop-result (get-args-from-stacks state arg-stacks)]
    (if (= args-pop-result :not-enough-args)
      state
      (let [result (apply function (:args args-pop-result))
            new-state (:state args-pop-result)]
        (push-to-stack new-state return-stack result)))))


;;;;;;;;;;
;; Instructions

(defn in1
  "Pushes the input labeled :in1 on the inputs map onto the :exec stack.
  Can't use make-push-instruction, since :input isn't a stack, but a map."
  [state]
  ;;:STUB
  (push-to-stack (pop-stack state :exec) :exec ((state :input) :in1)))
  ;;(assoc state :exec (conj (rest (state :exec)) ((state :input) :in1))))

(defn push-input
  "Takes a state and an input keyword and pushes the mapping of that input keyword to the
  top of the exec stack. We added this, not sure why you would have only one function for inputs
  or why you would set the input to nil"
  [state
   input] ;; input here is :in1 not something else like a string or int
  (let [input-val ((state :input) input)]
    (push-to-stack state :exec input-val)))

(def single-int-state
  {:exec '()
   :integer '(1)
   :string '()
   :input {:in1 4}})
                      

(defn integer_+
  "Adds the top two integers and leaves result on the integer stack.
  If integer stack has fewer than two elements, noops."
  [state]
  (if (< (count (get state :integer)) 2)
    (pop-stack state :exec)
    (make-push-instruction state +' [:integer :integer] :integer)))

;;;; This is an example of what would be necessary to implement integer_+
;;;; without the useful helper function make-push-instruction.
;; (defn integer_+_without_helpers
;;   [state]
;;   (if (< (count (:integer state)) 2)
;;     state
;;     (let [arg1 (peek-stack state :integer)
;;           arg2 (peek-stack (pop-stack state :integer) :integer)
;;           popped-twice (pop-stack (pop-stack state :integer) :integer)]
;;       (push-to-stack popped-twice :integer (+' arg1 arg2)))))


(defn integer_-
  "Subtracts the top two integers and leaves result on the integer stack.
  Note: the second integer on the stack should be subtracted from the top integer."
  [state]
  ;;:STUB
  ;; make null pointers stop plz
  (if (< (count (get state :integer)) 2)
    (pop-stack state :exec)
    (make-push-instruction state -' [:integer :integer] :integer)))

(defn integer_*
  "Multiplies the top two integers and leaves result on the integer stack."
  [state]
  ;;:STUB
  (if (< (count (get state :integer)) 2)
    (pop-stack state :exec)
    (make-push-instruction state *' [:integer :integer] :integer)))

(defn divide_by_zero?
  [state]
  (= (first (state :integer))) 0)
  

(defn integer_%
  "This instruction implements 'protected division'.
  In other words, it acts like integer division most of the time, but if the
  denominator is 0, it returns the numerator, to avoid divide-by-zero errors."
  [state]
  ;;:STUB
  ;; this one might be brok too
  (if (< (count (get state :integer)) 2)
    (pop-stack state :exec)
    (if (divide_by_zero? state)
      (assoc state :integer (conj (get (pop-stack (pop-stack state :integer) :integer) :integer) 0))
      (make-push-instruction state quot [:integer :integer] :integer))))


;;;;;;;;;;
;; Interpreter

(defn load-exec
  [program state]
  (assoc state :exec (concat program (state :exec))))



(defn interpret-one-step
  "Helper function for interpret-push-program.
  Takes a Push state and executes the next instruction on the exec stack,
  or if the next element is a literal, pushes it onto the correct stack.
  Returns the new Push state."
  [push-state]
  ;;:STUB
  (if (not (empty-stack? push-state :exec))
    (let [element (peek-stack push-state :exec)]
      (cond
        (instance? String element) (push-to-stack (pop-stack push-state :exec) :string element)
        (instance? Number element) (push-to-stack (pop-stack push-state :exec) :integer element)
        (= 'in1 element) (in1 push-state) ;; required b/c else statement applies first item in :exec stack and then pops it, so without this inputs just get removed form exec stack
        (seq? element) (interpret-one-step (load-exec element (pop-stack push-state :exec)))
        :else (pop-stack
               ((eval (first
                         (get (get-args-from-stacks push-state '(:exec))
                              :args)))
               push-state) :exec)))
    push-state))

(defn interpret-push-program
  "Runs the given program starting with the stacks in start-state. Continues
  until the exec stack is empty. Returns the state of the stacks after the
  program finishes executing."
  [program start-state]
  :STUB
  (let [state (load-exec program start-state)]
    (loop [state state]
      (if (empty-stack? state :exec)
          state
          (recur (interpret-one-step state))))))


;;;;;;;;;;
;; GP

(defn make-random-push-program
  "Creates and returns a new program. Takes a list of instructions and
  a maximum initial program size."
  [instructions max-initial-program-size]
  :STUB
  (let [program-size (+ (rand-int max-initial-program-size) 1)]
    (repeatedly program-size #(rand-nth instructions))))

;;(def testing-population
;;  (init-population 3 3))

;;(def testing-errors

;; testing: (tournament-selection (map #(regression-error-function %) (init-population 3 3)) 3)

;;(def testing-pop
;;  (map #(regression-error-function %) (init-population 5 15)))

(defn tournament-selection
  "Selects an individual from the population using a tournament. Returned 
  individual will be a parent in the next generation. Can use a fixed
  tournament size."
  [population
   tournament-size]
  :STUB
  (let [tournament-members (repeatedly tournament-size #(rand-nth population))]
    ;; This finds the individual with the smallest total-error 
    (apply min-key #(% :total-error) tournament-members)))

(defn prob-pick
  ([prob] (< (rand) prob))
  ([prob x] (prob-pick prob)))

(def test-prog-A
  {:exec '(in1 in2 in3 in4)
   :integer '(1 2 3 4 5 6 7)
   :string '("abc" "def")
   :input {:in1 4 :in2 6}})
(def test-prog-B
  {:exec '(in5 "hello miller" integer_+ integer_-)
   :integer '(1 2 3 4 5 6 7)
   :string '("abc" "def")
   :input {:in1 4 :in2 6}})

;;(def prog-A
;;  (prog-to-individual '(in1 in2 in3)))

(defn crossover
  "Crosses over two programs (note: not individuals) using uniform crossover.
  Returns child program."
  [prog-a
   prog-b]
  :STUB
  (loop [prog-a prog-a
         prog-b prog-b
         new '()]
    (if (empty? prog-a)
      (concat new (filter #(prob-pick 0.5 %) prog-b))
      (if (empty? prog-b)
        (concat new (filter #(prob-pick 0.5 %) prog-a))
        (recur (rest prog-a)
               (rest prog-b)
               (if (= (rand-int 2) 0)
                 (apply list (conj (apply vector new) (first prog-a)))
                 (apply list (conj (apply vector new) (first prog-b)))))))))

(defn uniform-addition
  "Randomly adds new instructions before every instruction (and at the end of
  the program) with some probability. Returns child program."
  [prog
   instructions]
  :STUB
  ;; Added instructions as a parameter
  (if (= prog ())
    prog
    (let [child (reduce concat
                        (map (fn [x]
                               (if (prob-pick 0.05)
                                 (list x (nth instructions (rand-int (count instructions))))
                                 (list x))) prog))]
      (if (prob-pick 0.05)
        (conj child (nth instructions (rand-int (count instructions))))
        child))))


;;(defn uniform-deletion
;;  "Randomly deletes instructions from program at some rate. Returns child program."
;;  [prog]
;;  :STUB
;;  (filter #(not (prob-pick 0.05)) prog))

(defn uniform-deletion
  [program]
  (filter #(not (prob-pick 0.05 %)) program))

(defn prog-to-individual
  ([prog]
  {:program prog
   :errors '[]
   :total-error 0})
  ([prog error-list total-error]
   {:program prog
    :errors (first error-list)
    :total-error (first error-list)}))

(defn vector-to-individual
  [ls]
  (prog-to-individual
   (nth (first ls) 1)
   (nth (nth ls 1) 1)
   (nth (nth ls 2) 1)))

;;(def testing-pop
;;  (map #(regression-error-function %) (init-population 5 15)))

(defn select-and-vary
  "Selects parent(s) from population and varies them, returning
  a child individual (note: not program). Chooses which genetic operator
  to use probabilistically. Gives 50% chance to crossover,
  25% to uniform-addition, and 25% to uniform-deletion."
  [population
   tournament-size]
  :STUB
  (let [seed (rand)
        parent1 (into () (:program (tournament-selection population tournament-size)))
        parent2 (into () (:program (tournament-selection population tournament-size)))]
    (cond
      (< seed 0.5) (crossover parent1 parent2)
      (and (>= seed 0.5) (< 0.75)) (uniform-addition parent1 parent2)
      (>= seed 0.75) (uniform-deletion parent1))))

(defn report
  "Reports information on the population each generation. Should look something
  like the following (should contain all of this info; format however you think
  looks best; feel free to include other info).

-------------------------------------------------------
               Report for Generation 3
-------------------------------------------------------
Best program: (in1 integer_% integer_* integer_- 0 1 in1 1 integer_* 0 integer_* 1 in1 integer_* integer_- in1 integer_% integer_% 0 integer_+ in1 integer_* integer_- in1 in1 integer_* integer_+ integer_* in1 integer_- integer_* 1 integer_%)
Best program size: 33
Best total error: 727
Best errors: (117 96 77 60 45 32 21 12 5 0 3 4 3 0 5 12 21 32 45 60 77)
  "
  [population generation]
  :STUB
  (println)
  (println "-------------------------------------------------------")
  (printf  "                    Report for Generation %s           " generation)
  (println)
  (println "-------------------------------------------------------")

  (let [best-prog (apply min-key #(% :total-error) population)]
    (printf "Best program: ") ;; (printf "Best program: %s" (best-prog :program))
    (println (best-prog :program))
    (println)
    (printf "Best program size: %s" (count (best-prog :program)))
    (println)
    (printf "Best total error: %s" (best-prog :total-error))
    (println)
    (printf "Best errors: %s" (best-prog :errors))))

;;   (printf "Total population error: %s" (reduce + (map #(% :total-error) (map regression-error-function %) yabo)))

(defn report2
  [pop gen]
  (report pop gen)
  (println)
  (printf "Total population error: %s" (reduce + (map #(% :total-error) pop)))
  (println)
  (printf "Average program size: %s" (quot (reduce + (map #(count (% :program)) pop)) (count pop))))

(defn init-population
  [size max-program-size]
  (map #(prog-to-individual %) (take size (repeatedly #(make-random-push-program instructions max-program-size)))))

;; NOT TESTED
(defn get-new-population
  [population population-size tournament-size]
  (loop [ new-pop '()]
    (if (= (count new-pop) population-size)
      new-pop
      (recur (conj new-pop (select-and-vary population tournament-size))))))

(defn push-gp
  "Main GP loop. Initializes the population, and then repeatedly
  generates and evaluates new populations. Stops if it finds an
  individual with 0 error (and should return :SUCCESS, or if it
  exceeds the maximum generations (and should return nil). Should print
  report each generation.
  --
  The only argument should be a map containing the core parameters to
  push-gp. The format given below will decompose this map into individual
  arguments. These arguments should include:
   - population-size
   - max-generations
   - error-function
   - instructions (a list of instructions)
   - max-initial-program-size (max size of randomly generated programs)"
  [{:keys [population-size max-generations error-function instructions max-initial-program-size]}]
  :STUB
  (loop [count 0
         population (map #(error-function %) (init-population population-size max-initial-program-size))]
    (report2 population count)
    (if (>= count max-generations)
      nil
      (if (= 0 (get (apply min-key #(% :total-error) population) :total-error))
        :SUCCESS
        (recur (+ count 1)
               (map #(error-function (prog-to-individual %)) (get-new-population (map #(error-function %) population) population-size 20))



             )))))


;;;;;;;;;;
;; The functions below are specific to a particular problem.
;; A different problem would require replacing these functions.
;; Problem: f(x) = x^3 + x + 3

(defn target-function
  "Target function: f(x) = x^3 + x + 3
  Should literally compute this mathematical function."
  [x]
  ;;:STUB
  (+ (* x x x) x 3)
  )

(def target-push-program
  '(in1 in1 in1 integer_* integer_* integer_* in1 3 integer_+ integer_+))

(def testing-prog
  '(in1 1 integer_+))

(def init-ind
  (prog-to-individual testing-prog))

(def test-cases-easy
  (list -3 -2 -1 0 1 2 3))

(def test-cases
  (list -50 -23 -18 -7 -3 -2 -1 0 1 2 3 7 18 23 50))

(defn abs
  [x]
  (if (< x 0)
    (*' -1 x)
    x))

(defn evaluate-one-case
  [individual state value]
  (interpret-push-program (:program individual) (push-to-stack state :input value)))

(defn abs-difference-in-error-lists
  [l1 l2]
  (loop [l1 l1
         l2 l2
         final '()]
    (if (= (count l1) 0)
      (reverse final)
      (recur (rest l1)
             (rest l2)
             (conj final (abs (- (first l1) (first l2))))))))

(defn get-error-list
  [individual]
  (map #(if (= (:integer %) '())
             1000
             (first (:integer %)))
             (map #(evaluate-one-case individual empty-push-state %) test-cases)))

(def test-indi
  {:program '(integer_% integer_-)
   :errors []
   :total-error 0})

(def test-shit
  {:exec '()
   :integer '()
   :string '()
   :input {:in1 -3}})

(defn regression-error-function
  "Takes an individual and evaluates it on some test cases. For each test case,
  runs program with the input set to :in1 in the :input map part of the Push state.
  Then, the output is the integer on top of the integer stack in the Push state
  returned by the interpreter. Computes each error by comparing output of
  the program to the correct output.
  Returns the individual with :errors set to the list of errors on each case,
  and :total-error set to the sum of the errors.
  Note: You must consider what to do if the program doesn't leave anything
  on the integer stack."
  [individual]
  :STUB
  (let [target-list (map #(target-function %) test-cases)
        program-list (get-error-list individual)
        errors (abs-difference-in-error-lists target-list program-list)
        ]
    {:program (:program individual)
     :errors errors
     :total-error (reduce + errors)}))
  ;; (push-to-stack empty-push-state :input 4)
  ;; (interpret-push-program (:program empty-ind) (push-to-stack empty-push-state :input 4))
  ;; (assoc init-ind :errors (into (vector) (difference-in-error-lists '(1 2 3) '(4 4 4))))

  ;; Push the input to empty state
  ;; evaluate the program in that state
  ;; use last item in int stack as compare value
  ;; compare it to errors list
  


;;;;;;;;;;
;; The main function. Uses some problem-specific functions.

(defn -main
  "Runs push-gp, giving it a map of arguments."
  [& args]
  (push-gp {:instructions instructions
            :error-function regression-error-function
            :max-generations 100
            :population-size 200
            :max-initial-program-size 50}))
