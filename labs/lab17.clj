(ns clojure-review2.core)

; Note: for functions returning sequences, it doesn't matter what
; kind of sequence you return as long as the sequence contains
; the correct values.

; This function takes two parameters: f (a function) and
; y (a value).  It should return a function taking a
; single parameter x, which applies f to the arguments x and y.
;
; Examples:
;   ((create-applicator + 2) 3) => 5
;   ((create-applicator - 3) 9) => 6
;   ((create-applicator conj :heythere) [:hey :hey]) => [:hey :hey :heythere]

(defn create-applicator [f y]
  (fn [x]
    (f x y)))

; This function takes three parameters: f (a function),
; y (a value), and s (a sequence).  It should return
; a sequence whose elements are formed from the elements
; of the sequence by applying f to a list element and y.
;
; Requirement: use create-applicator and map.
;
; Examples:
;   (apply-to-sequence + 1 [1 2 3]) => [2 3 4]
;   (apply-to-sequence * 4 [1 2 3 4]) => [4 8 12 16]
;   (apply-to-sequence conj :bacon [[:eggs] [:avocado] [:scallops]])
;      => [[:eggs :bacon] [:avocado :bacon] [:scallops :bacon]]
;   (apply-to-sequence + 1 []) => []

(defn apply-to-sequence [f y s]
  (map (create-applicator f y) s))

; This function takes three parameters: f (a function),
; y (a value), and s (a sequence).  It should return
; a sequence whose elements are formed from the elements
; of the sequence by applying f to a list element and y.
;
; Requirement: don't use map or create-applicator.
; Instead, use tail recursion, either using a helper function,
; or using loop/recur.  The recursion should be on s (the
; sequence).  Think about an appropriate base case.
;
; Examples:
;   (apply-to-sequence-the-hard-way + 1 [1 2 3]) => [2 3 4]
;   (apply-to-sequence-the-hard-way * 4 [1 2 3 4]) => [4 8 12 16]
;   (apply-to-sequence-the-hard-way conj :bacon [[:eggs] [:avocado] [:scallops]])
;      => [[:eggs :bacon] [:avocado :bacon] [:scallops :bacon]]
;   (apply-to-sequence-the-hard-way + 1 []) => []

(defn apply-to-sequence-the-hard-way [f y s]
  (loop [ss s
         acc []]
    (if (empty? ss)
      acc
      (recur (rest ss) (conj acc (f (first ss) y))))))

; This function takes a sequence and returns a sequence in which
; each pair of adjacent elements in the original sequence is swapped
; (first and second, third and fourth, etc.)
;
; As a special case, if the sequence contains an odd number of elements,
; leave the last element as-is.
;
; You can implement this however you'd like.  A tail recursion using
; a helper function or loop/recur isn't a bad approach.  Think carefully
; about an appropriate base case or base cases.
;
; Hint: the "first" function gets the first element of a sequence,
; and the "second" function gets the second element of a sequence.
;
; Examples:
;   (swapify [:a :b :c :d]) => [:b :a :d :c]
;   (swapify ["n" "u" "x" "i"]) => ["u" "n" "i" "x"]
;   (swapify []) => []
;   (swapify [:x :y :z]) => [:y :x :z]
;   (swapify [:a]) => [:a]

(defn swapify [s]
  (loop [ss s
         acc []]
    (cond
      (empty? ss) acc
      (empty? (rest ss)) (conj acc (first ss))
      :else (recur (rest (rest ss)) (conj (conj acc (second ss)) (first ss))))))
