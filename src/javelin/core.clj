(ns javelin.core
  (:refer-clojure :exclude [accessor update])
  (:require [clojure.core :as clj]
            [clojure.data.priority-map :refer [priority-map]]))

(declare cell? lens? input? cell propagate! set-formula!)

(def ^:dynamic *tx* nil)
(def ^:private last-rank     (ref 0))
(defn-         next-rank [ ] (commute last-rank inc))
(defn          deref*    [x] (if (cell? x) @x x))

(defprotocol IAccessCell
  (-rank     [this])
  (-thunk    [this])
  (-prev     [this])
  (-sinks    [this])
  (-constant [this])
  (-update   [this])
  (-set-prev [this v])
  (-set-rank [this v])
  (-add-sink [this sink])
  (-remove-sink [this sink])
  (-destroy [this keep-watches?])
  (-set-cell [this v])
  (-set-value [this v])
  (-set-formula [this f srcs updatefn])
  (-notify-watches [this old new]))

(defn- bf-seq [branch? children root]
  (let [walk (fn walk [queue]
               (when-let [node (peek queue)]
                 (lazy-seq
                  (cons node (walk (into (pop queue)
                                         (when (branch? node)
                                           (children node))))))))]
    (walk (conj clojure.lang.PersistentQueue/EMPTY root))))

(deftype Cell [meta state rank prev sources sinks thunk watches update constant]
  ;; internal accessors - run in an enclosing clj/dosync
  IAccessCell
  (-rank     [this] @rank)
  (-thunk    [this] @thunk)
  (-prev     [this] @prev)
  (-sinks    [this] @sinks)
  (-constant [this] @constant)
  (-update   [this] @update)
  (-set-prev [this v] (ref-set prev v))
  (-set-rank [this v] (ref-set rank v))

  (-remove-sink [this sink]
    (alter sinks disj sink))
  (-add-sink [this sink]
    (alter sinks conj sink))

  (-destroy [this keep-watches?]
    (let [srcs @sources]
      (ref-set sources [])
      (ref-set update nil)
      (ref-set thunk nil)
      (when-not keep-watches?
        (ref-set watches {}))
      (doseq [src (filter cell? srcs)]
        (-remove-sink src this))))

  (-set-cell [this x]
    (ref-set state x)
    (set-formula! this))

  (-set-value [this v]
    (ref-set prev @state)
    (ref-set state v))

  (-set-formula [this f srcs updatefn]
    (when f
      (ref-set constant true)
      (ref-set sources (conj (vec srcs) f))
      (doseq [source @sources]
        (when (cell? source)
          (when (and @constant (not (-constant source)))
            (ref-set constant false))
          (-add-sink source this)
          (if (> (-rank source) @rank)
            (doseq [dep (bf-seq identity #(-sinks %) source)]
              (-set-rank dep (next-rank))))))
      (let [compute #(apply (deref* (peek %)) (map deref* (pop %)))
            thunk*  #(-set-value this (compute @sources))]
        (ref-set thunk thunk*))
      (ref-set update updatefn))
    (propagate! this))

  (-notify-watches [this old new]
    (doseq [[k f] @watches]
      (f k this old new)))

  Object
  (toString [this] (pr-str @state))

  clojure.lang.IObj
  (withMeta [this meta]
    (Cell. meta state rank prev sources sinks thunk watches update constant))
  (meta [this] meta)

  clojure.lang.IRef
  (deref [this] @state)
  (getWatches [this] @watches)
  (addWatch [this k f] (clj/dosync (alter watches assoc k f)) this)
  (removeWatch [this k] (clj/dosync (alter watches dissoc k)) this)

  clojure.lang.IAtom
  (reset [this x]
    (cond
      (lens? this)  (@update x)
      (input? this) (clj/dosync
                      (ref-set state x)
                      (propagate! this))
      :else         (throw (ex-info "can't swap! or reset! formula cell"
                                    {:cell this})))
    @state)
  (swap [this f]           (reset! this (f @state)))
  (swap [this f arg]       (reset! this (f @state arg)))
  (swap [this f arg1 arg2] (reset! this (f @state arg1 arg2)))
  (swap [this f x y args]  (reset! this (apply f @state x y args))))

(defmethod clojure.core/print-method Cell
  [piece ^java.io.Writer writer]
  (.write writer (str "#object [javelin.core.Cell " piece "]")))

(defn- propagate [pm]
  (loop [queue pm]
    (when (seq queue)
      (let [next      (key (peek queue))
            value     (if-let [f (-thunk next)] (f) @next)
            old       (-prev next)
            continue? (not= value old)
            reducer   #(assoc %1 %2 (-rank %2))
            siblings  (pop queue)
            children  (-sinks next)]
        (when continue?
          (-set-prev next value)
          (-notify-watches next old value))
        (recur (if continue? (reduce reducer siblings children) siblings))))))

(defn add-cell-to-tx! [cell]
  (clj/dosync
    (alter *tx* assoc cell (-rank cell))))

(defn- propagate! [cell]
  (if *tx*
    (doto cell add-cell-to-tx!)
    (do (propagate (priority-map cell (-rank cell))) cell)))

(defn dosync* [thunk]
  (if *tx*
    (thunk)
    (binding [*tx* (ref (priority-map))]
      (thunk)
      (let [tx @*tx*]
        (binding [*tx* nil]
          (clj/dosync
            (propagate tx)))))))

;; Public

(defn destroy-cell!
  ([this] (destroy-cell! this nil))
  ([this keep-watches?] (clj/dosync (-destroy this keep-watches?))))

(defn set-formula!* [this f srcs updatefn]
  (clj/dosync
    (-set-formula this f srcs updatefn)))

(defn set-formula!
  "Given a Cell and optional formula function f and the cells f depends on,
  sources, updates the formula for this cell in place. If f and/or sources
  is not spcified they are set to nil."
  ([this]
   (clj/dosync
     (destroy-cell! this true)
     (set-formula!* this nil nil nil)))
  ([this f]
   (clj/dosync
     (destroy-cell! this true)
     (set-formula!* this f [] nil)))
  ([this f sources]
   (clj/dosync
     (destroy-cell! this true)
     (set-formula!* this f sources nil)))
  ([this f sources updatefn]
   (clj/dosync
     (destroy-cell! this true)
     (set-formula!* this f sources updatefn))))

(defn cell?
  "Returns c if c is a Cell, nil otherwise."
  [c]
  (when (instance? Cell c) c))

(defn formula?
  "Returns c if c is a formula cell, nil otherwise."
  [c]
  (when (-thunk c) c))

(defn lens?
  "Returns c if c is a lens, nil otherwise."
  [c]
  (when (and (cell? c) (-update c)) c))

(defn input?
  "Returns c if c is an input cell, nil otherwise."
  [c]
  (when (and (cell? c) (not (formula? c))) c))

(defn constant?
  "Returns c if c is a constant formula cell, nil otherwise."
  [c]
  (when (and (cell? c) (-constant c)) c))

(defn set-cell!
  "Converts c to an input cell in place, setting its contents to x. It's okay
  if c was already an input cell. Changes will be propagated to any cells that
  depend on c."
  [c x]
  (clj/dosync
    (-set-cell c x)))

;; Same as js
(defn formula
  "Returns a function that returns a formula cell with f as its formula, and
  if updatefn is provided the returned cell is a lens.

  See also: the javelin.core/cell= macro.

      (def x (cell 100))
      (def y (cell 200))

      (def z1 (cell= (+ x y)))
      (def z2 ((formula +) x y))

  The formula cells z1 and z2 are equivalent."
  ([f]
   (formula f nil))
  ([f updatefn]
   (fn [& sources]
     (set-formula!* (cell ::none) f sources updatefn))))

;; Same as js
(defn lens
  "Returns a new lens whose value is the same as c's with update function f.
  This is equivalent to ((formula identity f) c)."
  [c f]
  ((formula identity f) c))

(defn cell
  "Returns a new input cell containing value x. The :meta option can be used
  to create the cell with the given metadata map."
  ([x] (cell x :meta nil))
  ([x & {:keys [meta]}]
   (Cell. meta
          (ref x)                        ;; state
          (clj/dosync (ref (next-rank))) ;; rank
          (ref x)                        ;; prev
          (ref [])                       ;; sources
          (ref #{})                      ;; sinks
          (ref nil)                      ;; thunk
          (ref {})                       ;; watches
          (ref nil)                      ;; update
          (ref false))))                 ;; constant

;; Copied from js
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

(defn- safe-nth   [c i] (try (nth c i) (catch Exception _)))

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
