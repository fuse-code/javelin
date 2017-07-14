(ns javelin.core
  (:refer-clojure :exclude [accessor update])
  (:require [clojure.data.priority-map  :refer [priority-map]]))

(defn- make-cell [meta state rank prev sources sinks thunk update constant]
  (->>
    {::rank     rank
     ::prev     prev
     ::sources  sources
     ::sinks    sinks
     ::thunk    thunk
     ::update   update
     ::constant constant}
    (merge meta)
    (atom state :meta)))

(def  ^:private last-rank     (atom 0))
(defn-          next-rank [ ] (swap! last-rank inc))

(defn- accessor [key]
  (fn
    ([this] (get (meta this) key))
    ([this val] (alter-meta! this assoc key val))))

(def ^:private rank     (accessor ::rank))
(def ^:private prev     (accessor ::prev))
(def ^:private sources  (accessor ::sources))
(def ^:private sinks    (accessor ::sinks))
(def ^:private thunk    (accessor ::thunk))
(def ^:private update   (accessor ::update))
(def ^:private constant (accessor ::constant))

(defn- propagate! [cell]
  (loop [queue (priority-map cell (rank cell))]
    (when (seq queue)
      (let [next      (key (peek queue))
            value     (if-let [f (thunk next)] (f) @next)
            continue? (not= value (prev next))
            reducer   #(assoc %1 %2 (rank %2))
            siblings  (pop queue)
            children  (sinks next)]
        (if continue? (prev next value))
        (recur (if continue? (reduce reducer siblings children) siblings)))))
  cell)

;; Public

(declare cell? lens? input? cell)

(defn deref*    [x] (if (cell? x) @x x))

(defn destroy-cell!
  ([this]
   (destroy-cell! this nil))
  ([this keep-watches?]
   (let [srcs (sources this)]
     (sources this [])
     (update this nil)
     (thunk this nil)
     (when-not keep-watches?
       (doseq [[k _] (.getWatches ^clojure.lang.ARef this)]
         (.removeWatch ^clojure.lang.ARef this k)))
     (doseq [src (filter cell? srcs)]
       (sinks src (disj (sinks src) this))))))

(defn- bf-seq [branch? children root]
  (let [walk (fn walk [queue]
               (when-let [node (peek queue)]
                 (lazy-seq
                  (cons node (walk (into (pop queue)
                                         (when (branch? node)
                                           (children node))))))))]
    (walk (conj clojure.lang.PersistentQueue/EMPTY root))))

(defn- propagate-input-cell! [this x]
  ;; This won't work for lenses as the value has already been set into the Ref when we get here
  ;; TODO: change implementation to a custom type overriding IAtom
  (cond (lens? this)  ((update this) x)
        (input? this) (propagate! this)
        :else         (throw (Exception. "can't swap! or reset! formula cell"))))

(defn set-formula!* [this f srcs updatefn]
  (when f
    (constant this true)
    (sources this (conj (vec srcs) f))
    (doseq [source (sources this)]
      (when (cell? source)
        (when (and (constant this) (not (constant source)))
          (constant this false))
        (sinks source (conj (sinks source) this))
        (if (> (rank source) (rank this))
          (doseq [dep (bf-seq identity #(sinks %) source)]
            (rank dep (next-rank))))))
    (let [compute #(apply (deref* (peek %)) (map deref* (pop %)))
          thunk*  #(reset! this (compute (sources this)))]
      (remove-watch this ::cell)
      (thunk this thunk*))
    (update this updatefn))
  (when-not f
    (add-watch this ::cell
      (fn [_ c _ n] (propagate-input-cell! c n))))
  (propagate! this))

(defn set-formula!
  "Given a Cell and optional formula function f and the cells f depends on,
  sources, updates the formula for this cell in place. If f and/or sources
  is not spcified they are set to nil."
  ([this]
   (destroy-cell! this true)
   (set-formula!* this nil nil nil))
  ([this f]
   (destroy-cell! this true)
   (set-formula!* this f [] nil))
  ([this f sources]
   (destroy-cell! this true)
   (set-formula!* this f sources nil))
  ([this f sources updatefn]
   (destroy-cell! this true)
   (set-formula!* this f sources updatefn)))

(defn cell?
  "Returns c if c is a Cell, nil otherwise."
  [c]
  (when (rank c) c))

(defn formula?
  "Returns c if c is a formula cell, nil otherwise."
  [c]
  (when (thunk c) c))

(defn lens?
  "Returns c if c is a lens, nil otherwise."
  [c]
  (when (and (cell? c) (update c)) c))

(defn input?
  "Returns c if c is an input cell, nil otherwise."
  [c]
  (when (and (cell? c) (not (formula? c))) c))

(defn constant?
  "Returns c if c is a constant formula cell, nil otherwise."
  [c]
  (when (and (cell? c) (constant c)) c))

(defn set-cell!
  "Converts c to an input cell in place, setting its contents to x. It's okay
  if c was already an input cell. Changes will be propagated to any cells that
  depend on c."
  [c x]
  ;; TODO: this will trigger watches on reset!... Switch to non-ref cells
  (doto c (remove-watch ::cell) (reset! x) (set-formula!)))

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
  ;; TODO: calling set-formula! won't be necessary once we move to non-ref cells
  ;; currently `set-formula!` is needed to trigger the initial ::cell watch code.
  ([x] (cell x :meta {}))
  ([x & {:keys [meta]}]
   (doto (make-cell meta x (next-rank) x [] #{} nil nil false)
     (set-formula!))))

(defn dosync* [thunk]
  (thunk))

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
