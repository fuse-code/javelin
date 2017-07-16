(ns javelin.core
  (:refer-clojure :exclude [dosync])
  (:require [javelin.impl :as impl]
            [javelin.macros :as macros]
   #? (:clj [net.cgrand.macrovich :as macro]))
  #? (:cljs (:require-macros [javelin.macros :as macros]
                             [net.cgrand.macrovich :as macro])))

(macro/usetime

;; Construction/mutation

(defn cell
  "Returns a new input cell containing value x. The :meta option can be used
  to create the cell with the given metadata map."
  ([value] (impl/cell value))
  ([value & {:keys [meta graph]}] (impl/cell value :meta meta :graph graph)))

(defn formula
  "Returns a function that returns a formula cell with f as its formula, and
  if updatefn is provided the returned cell is a lens.

  See also: the javelin.core/cell= macro.

      (def x (cell 100))
      (def y (cell 200))

      (def z1 (cell= (+ x y)))
      (def z2 ((formula +) x y))

  The formula cells z1 and z2 are equivalent."
  ([f] (formula f nil))
  ([f updatefn] (impl/formula f updatefn)))

(defn lens
  "Returns a new lens whose value is the same as c's with update function f.
  This is equivalent to `((formula identity f) c)`."
  [c f]
  ((formula identity f) c))

(def ^:deprecated lift
  "This function is deprecated, please use #'javelin.core/formula instead."
  formula)

(defn set-cell!
  "Converts c to an input cell in place, setting its contents to x. It's okay
  if c was already an input cell. Changes will be propagated to any cells that
  depend on c."
  [c x]
  (impl/set-cell! c x))

(defn set-formula!
  "Given a Cell and optional formula function f and the cells f depends on,
  sources, updates the formula for this cell in place. If f and/or sources
  is not spcified they are set to nil."
  ([this] (impl/set-formula! this))
  ([this f] (impl/set-formula! this f))
  ([this f sources] (impl/set-formula! this f sources))
  ([this f sources updatefn] (impl/set-formula! this f sources updatefn)))

(defn destroy-cell!
  "Unlinks this Cell from the cell graph and resets all internal state. Watches
  are preserved when keep-watches? is true, otherwise they are all removed."
  ([this] (impl/destroy-cell! this))
  ([this keep-watches?] (impl/destroy-cell! this keep-watches?)))

;; Predicates

(defn cell?
  "Returns c if c is a Cell, nil otherwise."
  [c] (impl/cell? c))

(defn formula?
  "Returns c if c is a formula cell, nil otherwise."
  [c] (impl/formula? c))

(defn lens?
  "Returns c if c is a lens, nil otherwise."
  [c] (impl/lens? c))

(defn input?
  "Returns c if c is an input cell, nil otherwise."
  [c] (impl/input? c))

(defn constant?
  "Returns c if c is a constant formula cell, nil otherwise."
  [c] (impl/constant? c))

;; Graph

(defn default-graph
  "Returns the default graph."
  [] impl/default-graph)

(defn graph
  "Returns a graph to attach the cells to.
  When given a cell, returns the graph that this cell belongs to."
  ([] (graph (gensym)))
  ([name-or-cell]
   (if (cell? name-or-cell)
     (impl/graph-of name-or-cell)
     (impl/graph name-or-cell))))

;; More

(defn dosync*
  "Calls the thunk with no arguments within a transaction. Propagation of
  updates to formula cells is deferred until the transaction is complete.
  Input cells *will* update during the transaction. Transactions may be
  nested.

  See also: the javelin.core/dosync macro."
  [thunk] (impl/dosync* thunk))

(defn deref*
  "If x is a Cell dereferences x and returns the value, otherwise returns x."
  [x] (impl/deref* x))

;; Complex operations

(defn alts!
  "Given a number of cells, returns a formula cell whose value is a seq of
  values from cells that changed in the last update. Note that multiple cells
  may update atomically, which is why the formula's value is a seq.

  Consider:

      (def a (cell {:x 1 :y 2}))
      (def x (cell= (:x a)))
      (def y (cell= (:y a)))
      (def z (alts! x y))

  then,

      (deref z) ;=> (1 2)

      (swap! a assoc :x 42)
      (deref z) ;=> (42)

      (reset! a {:x 10 :y 20})
      (deref z) ;=> (10 20)
  "
  [& cells]
  (let [olds    (atom (repeat (count cells) ::none))
        tag-neq #(vector (not= %1 %2) %2)
        diff    #(->> %2 (map tag-neq %1) (filter first) (map second) distinct)
        proc    #(let [news (diff (deref olds) %&)] (reset! olds %&) news)]
    (apply (formula proc) cells)))

(defn- safe-nth [c i]
  (try (nth c i) (catch #? (:clj Exception :cljs js/Error) _)))

(defn cell-map
  "Given a function f and a cell c that contains a seqable collection of items,
  returns a seq of formula cells such that the ith formula cell is equivalent
  to (cell= (f (nth c i)))."
  [f c]
  (let [cseq ((formula seq) c)]
    (map #((formula (comp f safe-nth)) cseq %) (range 0 (count @cseq)))))

(defn cell-doseq*
  "Given a function f and a cell c that contains a seqable collection of items,
  calls f for side effects once for each item in c, passing one argument: a
  formula cell equivalent to (cell= (nth c i)) for the ith item in c. Whenever
  c grows beyond its previous maximum size f is called as above for each item
  beyond the maximum size. Nothing happens when c shrinks.

  See also: the javelin.core/cell-doseq macro.

  Consider:

      (def things (cell [:a :b :c]))
      (cell-doseq*
        things
        (fn doit [x]
          (prn :creating @x)
          (add-watch x nil #(prn :updating %3 %4))))

      ;; the following is printed:

      :creating :a
      :creating :b
      :creating :c

  Shrink things by removing the last item:

      (swap! things pop)

      ;; the following is printed (because the 3rd item in things is now nil,
      ;; since things only contains 2 items) -- note that the doit function is
      ;; not called (or we would see a :creating message):

      :updating :c nil

  Grow things such that it is one item larger than it ever was:

      (swap! things into [:u :v])

      ;; the following is printed (because things now has 4 items, so the 3rd
      ;; item is now :u and the max size increases by one with the new item :v):

      :updating nil :u
      :creating :v

  A weird imagination is most useful to gain full advantage of all the features."
  [c f]
  (let [pool-size (atom 0)]
    (-> c ((formula (fn [items]
                      (let [cnt (count items)]
                        (when (< @pool-size cnt)
                          (dotimes [i (- cnt @pool-size)]
                            (f ((formula safe-nth) c (+ i @pool-size))))
                          (reset! pool-size cnt)))))))))

)

(macro/deftime

;(def with-let   macros/with-let)
;(def cell=      macros/cell=)
;(def set-cell!= macros/set-cell!=)
;(def defc       macros/defc)
;(def defc=      macros/defc=)
;(def formula-of macros/formula-of)
;(def formulet   macros/formulet)
;(def cell-let   macros/cell-let)
;(def dosync     macros/dosync)
;(def cell-doseq macros/cell-doseq)

)
