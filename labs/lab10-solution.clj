;; test prices map
(def fruit-prices
  {:apple 0.75,
   :orange 0.80,
   :pomegranate 2.50,
   :banana 0.50,
   :plum 1.20,
   :peach 1.00,
   :persimmon 1.75,
   :lime 0.60})

;; Test invoice
(def yummy-fruit
  [[:persimmon 2]
   [:orange 3]
   [:peach 1]
   [:plum 10]
   [:pomegranate 5]])

;; tally-item function
(defn tally-item [prices line-item]
  (* (get prices (first line-item)) (second line-item)))


;; Here is how to compute the total price of an invoice
(reduce +
        (map (fn [line-item] (tally-item fruit-prices line-item)) yummy-fruit))

