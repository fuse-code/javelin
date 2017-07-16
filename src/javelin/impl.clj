(ns javelin.impl
  (:refer-clojure :exclude [accessor update])
  (:require [clojure.core :as clj]
            [clojure.data.priority-map :refer [priority-map]]))

(declare cell? lens? input? cell propagate! set-formula! -graph)

;; Graph
(defn graph [name]
  {::name  name
   ::cells (ref #{})})

(defn assign-to-graph [cell graph]
  (assert graph "graph must be specified")
  (clj/update graph ::cells #(alter % conj cell)))

(defn remove-from-graph [cell graph]
  (assert graph "graph must be specified")
  (clj/update graph ::cells #(alter % disj cell)))

(defn graph-of [cell]
  (-graph cell))

(def           default-graph (graph 'javelin.graph/default))
(def ^:dynamic *graph*       default-graph)

(def ^:dynamic *tx*          nil)
(def ^:private last-rank     (ref 0))
(defn-         next-rank [ ] (commute last-rank inc))
(defn          deref*    [x] (if (cell? x) @x x))

(defprotocol IAccessCell
  (-graph    [this])
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
  (-destroy [this keep-watches? keep-in-graph?])
  (-set-cell [this v])
  (-set-value [this v])
  (-set-formula [this f srcs updatefn])
  (-notify-watches [this old new])
  (-reset [this f]))

(defn- bf-seq [branch? children root]
  (let [walk (fn walk [queue]
               (when-let [node (peek queue)]
                 (lazy-seq
                  (cons node (walk (into (pop queue)
                                         (when (branch? node)
                                           (children node))))))))]
    (walk (conj clojure.lang.PersistentQueue/EMPTY root))))

(deftype Cell [meta graph state rank prev sources sinks thunk watches update constant]
  ;; internal accessors - run in an enclosing clj/dosync
  IAccessCell
  (-graph    [this] graph)
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

  (-destroy [this keep-watches? keep-in-graph?]
    (let [srcs @sources]
      (ref-set sources [])
      (ref-set update nil)
      (ref-set thunk nil)
      (when-not keep-in-graph?
        ;; TODO: this is an orphaned cell - what should we do with it?
        (remove-from-graph this graph))
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

  (-reset [this f]
    (locking graph
      (cond
        (lens? this)  (@update (f))
        (input? this) (clj/dosync
                        (ref-set state (f))
                        (propagate! this))
        :else         (throw (ex-info "can't swap! or reset! formula cell"
                                      {:cell this}))))
    @state)

  Object
  (toString [this] (pr-str @state))

  clojure.lang.IObj
  (withMeta [this meta]
    (clj/dosync
      (doto (Cell. meta graph state rank prev sources sinks thunk watches update constant)
        (assign-to-graph graph))))
  (meta [this] meta)

  clojure.lang.IRef
  ;; TODO: should we `locking graph` when derefing?
  (deref [this] @state)
  (getWatches [this] @watches)
  (addWatch [this k f] (clj/dosync (alter watches assoc k f)) this)
  (removeWatch [this k] (clj/dosync (alter watches dissoc k)) this)

  clojure.lang.IAtom
  (reset [this x]           (-reset this (constantly x)))
  (swap  [this f]           (-reset this #(f @state)))
  (swap  [this f arg]       (-reset this #(f @state arg)))
  (swap  [this f arg1 arg2] (-reset this #(f @state arg1 arg2)))
  (swap  [this f x y args]  (-reset this #(apply f @state x y args))))

(defmethod clojure.core/print-method Cell
  [c ^java.io.Writer writer]
  (.write writer (str "#object [javelin.core.Cell " c "]")))

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
  ([this keep-watches?] (destroy-cell! this keep-watches? false))
  ([this keep-watches? keep-in-graph?]
   (clj/dosync (-destroy this keep-watches? keep-in-graph?))))

(defn- set-formula!* [this f srcs updatefn]
  (clj/dosync
    (-set-formula this f srcs updatefn)))

(defn set-formula!
  ([this]
   (clj/dosync
     (destroy-cell! this true true)
     (set-formula!* this nil nil nil)))
  ([this f]
   (clj/dosync
     (destroy-cell! this true true)
     (set-formula!* this f [] nil)))
  ([this f sources]
   (clj/dosync
     (destroy-cell! this true true)
     (set-formula!* this f sources nil)))
  ([this f sources updatefn]
   (clj/dosync
     (destroy-cell! this true true)
     (set-formula!* this f sources updatefn))))

(defn cell? [c]
  (when (instance? Cell c) c))

(defn formula? [c]
  (when (-thunk c) c))

(defn lens? [c]
  (when (and (cell? c) (-update c)) c))

(defn input? [c]
  (when (and (cell? c) (not (formula? c))) c))

(defn constant? [c]
  (when (and (cell? c) (-constant c)) c))

(defn set-cell! [c x]
  (clj/dosync (-set-cell c x)))

(defn cell
  ([x] (cell x :meta nil))
  ([x & {:keys [meta graph]}]
   (let [graph (or graph *graph*)]
     (clj/dosync
       (doto
         (Cell. meta
                graph
                (ref x)                        ;; state
                (ref (next-rank))              ;; rank
                (ref x)                        ;; prev
                (ref [])                       ;; sources
                (ref #{})                      ;; sinks
                (ref nil)                      ;; thunk
                (ref {})                       ;; watches
                (ref nil)                      ;; update
                (ref false))                   ;; constant
         (assign-to-graph graph))))))

(defn- validate-single-graph! [graphs]
  (when-not (= (count graphs) 1)
    (throw (ex-info "Cannot create a formula from cells belonging to multiple graphs!"
                    {:graphs graphs})))
  graphs)

(defn- discover-graph [sources]
  (some->> (filter cell? sources)
           (map -graph)
           (seq)
           (set)
           (validate-single-graph!)
           (first)))

(defn formula
  [f updatefn]
  (fn [& sources]
    (let [graph (or (discover-graph sources) *graph*)]
      (set-formula!* (cell ::none :graph graph) f sources updatefn))))
