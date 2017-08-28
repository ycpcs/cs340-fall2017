(ns clojure-review.core)

; Note: for all functions that return sequences, it doesn't
; really matter what kind of sequence you return, as long as
; it contains the expected values.

; Complete the function so that it returns a vector containing two
; copies of the parameter x.
;
; Examples:
;   (make-pair 3) => [3 3]
;   (make-pair [:howdy]) => [[:howdy] [:howdy]]
;   (make-pair "Yum!") => ["Yum!" "Yum!"]
;
(defn make-pair [x]
  [x x])

; Complete the function so that it applies f to
; the result of applying f to x.
; In other word, it "double applies" the function f to x.
;
; Examples:
;   (double-apply inc 2) => 4
;   (double-apply first [[:a :b] :c [:d [:e]]]) => :a
;   (double-apply rest [[:a :b] :c [:d [:e]]]) => ([:d [:e]])
;
(defn double-apply [f x]
  (f (f x)))

; Return a function which takes a function f, and returns a function
; with a single parameter that returns the result of applying f to
; the result of applying f to the returned function's parameter.
; (I.e., return a function that double applies f to the
; returned function's parameter.)
;
; Examples:
;   ((double-applicator inc) 2) => 4
;   ((double-applicator first) [[:a :b] :c [:d [:e]]]) => :a
;   ((double-applicator rest) [[:a :b] :c [:d [:e]]]) => ([:d [:e]])
;
(defn double-applicator [f]
  (fn [x]
    (double-apply f x)))

; Complete the function so that it takes a sequence of values
; and returns a "flattened" sequence.  Specifically, for each
; element in the sequence:
;
;   - if the element is not a sequence, make it part of the
;     result sequence
;   - if the element is a sequence, make all of the
;     values resulting from flattening the element part of
;     the result sequence
;
; The elements in the result sequence should be in the same
; order as the order in which they occurred in the original
; sequence.
;
; Requirement: use recursion with an explicit helper
; function.  The helper function should have an accumulator parameter.
;
; Hint: you can use the sequential? function to check whether a
; value is a sequence.
;
; Examples:
;    (my-flatten [:a [[[:b] :c]]]) => (:a :b :c)
;    (my-flatten [:spam :spam :spam :spam :spam [:baked-beans [:spam] [[:spam]] [[[:spam]]]]])
;       => (:spam :spam :spam :spam :spam :baked-beans :spam :spam :spam)
;

(defn my-flatten-work [a-seq acc]
  (cond
    (empty? a-seq) acc
    (sequential? (first a-seq)) (let [aa (my-flatten-work (first a-seq) acc)]
                                  (recur (rest a-seq) aa))
    :else (recur (rest a-seq) (conj acc (first a-seq)))))

(defn my-flatten [a-seq]
  (my-flatten-work a-seq []))

; Given a vector a-vec and a sequence a-seq, return a vector
; containing all of the elements of a-vec followed by all of the
; elements of a-seq.
;
; Requirements: your solution must be tail recursive, either with
; a helper function or loop/recur.  Each iteration should use
; conj to append one element of a-seq onto the current accumulator
; value.
;
; Examples:
;    (conj-all [:a :b :c] [:d :e :f]) => [:a :b :c :d :e :f]
;    (conj-all [] ["x" "y" "z"]) => ["x" "y" "z"]
;    (conj-all ["baked" "beans"] []) => ["baked" "beans"]
;
(defn conj-all [a-vec a-seq]
  (loop [v a-vec
         s a-seq]
    (if (empty? s)
      v
      (recur (conj v (first s)) (rest s)))))
