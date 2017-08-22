---
layout: default
title: "Recursion on sequences"
---

{% highlight clojure %}
;; Recursion to build a list using elements of an input
;; sequence.  Not tail recursive!
(defn inc-members [s]
  (if (empty? s)
    '()
    (conj (inc-members (rest s)) (inc (first s)))))

;; Recursion to build a vector using elements of an input
;; sequence.  Not tail recursive!
(defn inc-members2 [s]
  (if (empty? s)
    []
    (conj (inc-members2 (rest s)) (inc (first s)))))

;; Helper function to build a result sequence by processing
;; elements of an input sequence tail-recursively.
(defn work [s acc]
  (if (empty? s)
    acc
    (recur (rest s)
           (conj acc (inc (first s))))))

;; Call the helper function, using a list as the accumulator.
(defn inc-members3 [s]
  (work s '()))

;; Call the helper function, using a vector as the accumulator.
(defn inc-members4 [s]
  (work s []))

;; This is equivalent to inc-members4, but uses loop/recur
;; instead of an explicit helper function.
(defn inc-members5 [s]
  (loop [ss s
         acc []]
    (if (empty? ss)
      acc
      (recur (rest ss)
             (conj acc (inc (first ss)))))))

;; Try the following:
;;    (inc-members [1 2 3])
;;    (inc-members2 [1 2 3])
;;    (inc-members3 [1 2 3])
;;    (inc-members4 [1 2 3])
;;    (inc-members5 [1 2 3])
{% endhighlight %}
