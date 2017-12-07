(ns statecolors)

; Adjacency list vector for US states.
(def state-adjacency-list
  ; Source of data:
  ;  http://writeonly.wordpress.com/2009/03/20/adjacency-list-of-states-of-the-united-states-us/
  ; Note: I modified the data so that the Four Corners states aren't
  ; considered adjacent to their diagnoal neighbors.  The reason is
  ; that adjacency to the diagonal neighbors would make the graph
  ; non-planar, meaning that the feasibility of 4-coloring is not
  ; guaranteed.
  [[:AK]
   [:AL :MS :TN :GA :FL]
   [:AR :MO :TN :MS :LA :TX :OK]
   [:AZ :CA :NV :UT :NM] ; :CO
   [:CA :OR :NV :AZ]
   [:CO :WY :NE :KS :OK :NM :UT] ; :AZ
   [:CT :NY :MA :RI]
   [:DC :MD :VA]
   [:DE :MD :PA :NJ]
   [:FL :AL :GA]
   [:GA :FL :AL :TN :NC :SC]
   [:HI]
   [:IA :MN :WI :IL :MO :NE :SD]
   [:ID :MT :WY :UT :NV :OR :WA]
   [:IL :IN :KY :MO :IA :WI]
   [:IN :MI :OH :KY :IL]
   [:KS :NE :MO :OK :CO]
   [:KY :IN :OH :WV :VA :TN :MO :IL]
   [:LA :TX :AR :MS]
   [:MA :RI :CT :NY :NH :VT]
   [:MD :VA :WV :PA :DC :DE]
   [:ME :NH]
   [:MI :WI :IN :OH]
   [:MN :WI :IA :SD :ND]
   [:MO :IA :IL :KY :TN :AR :OK :KS :NE]
   [:MS :LA :AR :TN :AL]
   [:MT :ND :SD :WY :ID]
   [:NC :VA :TN :GA :SC]
   [:ND :MN :SD :MT]
   [:NE :SD :IA :MO :KS :CO :WY]
   [:NH :VT :ME :MA]
   [:NJ :DE :PA :NY]
   [:NM :AZ :CO :OK :TX] ; :UT
   [:NV :ID :UT :AZ :CA :OR]
   [:NY :NJ :PA :VT :MA :CT]
   [:OH :PA :WV :KY :IN :MI]
   [:OK :KS :MO :AR :TX :NM :CO]
   [:OR :CA :NV :ID :WA]
   [:PA :NY :NJ :DE :MD :WV :OH]
   [:RI :CT :MA]
   [:SC :GA :NC]
   [:SD :ND :MN :IA :NE :WY :MT]
   [:TN :KY :VA :NC :GA :AL :MS :AR :MO]
   [:TX :NM :OK :AR :LA]
   [:UT :ID :WY :CO :AZ :NV] ; :NM
   [:VA :NC :TN :KY :WV :MD :DC]
   [:VT :NY :NH :MA]
   [:WA :ID :OR]
   [:WI :MI :MN :IA :IL]
   [:WV :OH :PA :MD :VA :KY]
   [:WY :MT :SD :NE :CO :UT :ID]])

; Build a map of state names to their indices in the adjancency list vector
(def state-to-index-map
  (zipmap (map (fn [lst] (first lst)) state-adjacency-list) (range 0 (count state-adjacency-list))))

; Possible colors
; FIXME: for some reason I can only get a successful result
; with 5 colors.  Perhaps the graph is still not planar, even
; after fixing the Four Corners problem?
(def colors [:red :green :blue :yellow :purple]) ; orange

; Create a vector of Refs, one for each state.
; Each ref stores the current color for the corresponding state.
(def state-colors
  (vec (repeatedly (count state-adjacency-list) (fn [] (ref :red)))))

; Reset all of the state colors back to red.
; This is helpful if the computation fails because
; it cannot make progress due to a "deadlocked"
; configuration of states.
(defn reset-state-colors []
  (loop [r state-colors]
    (if (empty? r)
      true
      (do
        (dosync
          (ref-set (first r) :red))
        (recur (rest r))))))

; Get a list of adjacent states given the index of a state
(defn get-neighbors [index]
  (rest (nth state-adjacency-list index)))

; Return a set containing the current colors of the given
; state's neighbors.
(defn get-neighbor-colors [index]
  (let [neighbors (get-neighbors index)]
    (into #{} (map (fn [state]
                     (let [index (state-to-index-map state)]
                       (deref (nth state-colors index))))
                   neighbors))))

; Choose a color that is not in the given set of colors,
; returning the default if there is no such color.
(defn choose-other-color [s default]
  (letfn [(work [candidates good]
            (cond
              (empty? candidates) (if (empty? good) (list default) good)
              (not (contains? s (first candidates))) (recur (rest candidates) (conj good (first candidates)))
              :else (recur (rest candidates) good)))]
    (rand-nth (work colors []))))

; Worker function to try computing a color for a state
; by examining the colors of adjacent states and (if possible)
; picking a color that is not used by the neighboring states.
(defn worker [index count maxiters ok]
  (if (= count maxiters)
    ; Reached the end of the computation:
    ; return the final color, along with boolean indicating whether
    ; the final color is legal (as far as we can tell)
    [(deref (nth state-colors index)) ok]
    ; In a transaction, attempt to find a color for the state.
    (do
      ; The found-legal-color variable will be set to the
      ; result of the transaction: true if we found a
      ; legal color for the state, false if not.
      ; This value is sent into the next recursive call
      ; so we always have an idea of whether or not this
      ; worker was able to find a legal color for its state.
      (let [found-legal-color
             ; Start a transaction.
             (dosync
               ; Find state's current color and the colors of
               ; of its neighbors.
               (let [my-color (deref (nth state-colors index))
                     neighbor-colors (get-neighbor-colors index)]
                 (if (contains? neighbor-colors my-color)
                   ; The state is using the same color as one of
                   ; its neighbors, so choose a new color
                   ; by setting the state's ref to a new color.
                   ; Evaluate to true or false depending on
                   ; whether the new color is different from
                   ; its neighbors' colors.
                   (let [new-color (choose-other-color neighbor-colors my-color)]
                     (do (ref-set (nth state-colors index) new-color)
                         (not (contains? neighbor-colors new-color))))
                   ; Current color is ok
                   true)))]
      ; Continue recursively.
      (recur index (+ count 1) maxiters found-legal-color)))))

; Create parallel tasks for each state, use them to find a possible
; color for each state.  Each worker will use maxiters as its number
; of iterations.
(defn find-state-colors [maxiters]
  (apply pcalls (map (fn [i] (fn [] (worker i 1 maxiters false)))
                     (range 0 (count state-adjacency-list)))))

; Check whether a given state has neighbors which are all
; of different colors.
(defn check-state [i neighbors]
  (if (empty? neighbors)
      true
      (let [neighbor-index (state-to-index-map (first neighbors))]
        (if (= (deref (nth state-colors i)) (deref (nth state-colors neighbor-index)))
            false
            (recur i (rest neighbors))))))

; Check whether given solution (list of state color assignments) is valid.
(defn check-state-colors []
  (letfn [(work [i n]
            (if (= i n)
                true
                (if (not (check-state i (get-neighbors i)))
                    false
                    (recur (+ i 1) n))))]
    (work 0 (count state-colors))))
