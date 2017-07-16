;; Copyright (c) Alan Dipert and Micha Niskin. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns javelin.impl
  (:require [goog.array :as garray]
            [goog.object :as gobj]))

;; helpers ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declare cell? cell input? lens? cmp-rank)

;; Graph
(defn graph [name]
  {::name  name
   ::cells (atom #{})})

(defn assign-to-graph [cell graph]
  (assert graph "graph must be specified")
  (update graph ::cells #(swap! % conj cell)))

(defn remove-from-graph [cell graph]
  (assert graph "graph must be specified")
  (update graph ::cells #(swap! % disj cell)))

(defn graph-of [cell]
  (.-graph cell))

(def           default-graph (graph 'javelin.graph/default))
(def ^:dynamic *graph*       default-graph)

(def ^:private ^:dynamic *tx* nil)
(def ^:private last-rank (atom 0))

(defn- propagate* [pri-map]
  (when-let [next (.shift pri-map)]
    (let [old (.-prev next)
          new (if-let [f (.-thunk next)] (f) (.-state next))]
      (when (not= new old)
        (set! (.-prev next) new)
        (-notify-watches next old new)
        (let [sinks (.-sinks next)]
          (dotimes [i (alength sinks)]
            (garray/binaryInsert pri-map (aget sinks i) cmp-rank))))
      (recur pri-map))))

(defn deref*      [x]   (if (cell? x) @x x))

(defn- next-rank  [ ]   (swap! last-rank inc))
(defn- cmp-rank   [a b] (let [a (.-rank a) b (.-rank b)]
                          (if (= a b) 0 (- a b))))
(defn- add-sync!  [c]   (garray/binaryInsert *tx* c cmp-rank))
(defn- propagate! [c]   (if *tx* (doto c add-sync!) (doto c (-> array propagate*))))

;; javelin ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn destroy-cell!
  ([this] (destroy-cell! this nil))
  ([this keep-watches?] (destroy-cell! this keep-watches? false))
  ([this keep-watches? keep-in-graph?]
   (let [srcs (.-sources this)]
     (set! (.-sources this) (array))
     (set! (.-update this) nil)
     (set! (.-thunk this) nil)
     (when-not keep-watches?
       (set! (.-watches this) {})
       (set! (.-numwatches this) 0))
     (when-not keep-in-graph?
       (remove-from-graph this (.-graph this))
        ;; TODO: this is an orphaned cell - what should we do with it?
       (set! (.-graph this) nil))
     (dotimes [i (alength srcs)]
       (when-let [c (cell? (aget srcs i))]
         (garray/removeIf (.-sinks c) #(= % this)))))))

(defn- set-formula!* [this f sources updatefn]
  (when f
    (set! (.-constant this) true)
    (set! (.-sources this) (doto sources (.push f)))
    (dotimes [i (alength (.-sources this))]
      (let [source (aget (.-sources this) i)]
        (when (cell? source)
          (when (and (.-constant this) (not (.-constant source)))
            (set! (.-constant this) false))
          (.push (.-sinks source) this)
          (if (> (.-rank source) (.-rank this))
            (loop [q (array source)]
              (when-let [dep (.shift q)]
                (set! (.-rank dep) (next-rank))
                (recur (.concat q (.-sinks dep)))))))))
    (set! (.-thunk this) #(let [argv (.slice (.-sources this))
                                f    (deref* (.pop argv))]
                            (dotimes [i (alength argv)]
                              (aset argv i (deref* (aget argv i))))
                            (set! (.-state this) (.apply f nil argv))))
    (set! (.-update this) updatefn))
  (propagate! this))

(defn set-formula!
  ([this]
   (destroy-cell! this true true)
   (set-formula!* this nil nil nil))
  ([this f]
   (destroy-cell! this true true)
   (set-formula!* this f (array) nil))
  ([this f sources]
   (destroy-cell! this true true)
   (set-formula!* this f (into-array sources) nil))
  ([this f sources updatefn]
   (destroy-cell! this true true)
   (set-formula!* this f (into-array sources) updatefn)))

(deftype Cell [meta graph state rank prev sources sinks thunk watches update constant numwatches]
  cljs.core/IPrintWithWriter
  (-pr-writer [this w _]
    (write-all w "#object [javelin.core.Cell " (pr-str state) "]"))

  cljs.core/IWithMeta
  (-with-meta [this meta]
    (doto (Cell. meta graph state rank prev sources sinks thunk watches update constant numwatches)
      (assign-to-graph graph)))

  cljs.core/IMeta
  (-meta [this] meta)

  cljs.core/IDeref
  (-deref [this] (.-state this))

  cljs.core/IReset
  (-reset! [this x]
    (cond (lens? this)  ((.-update this) x)
          (input? this) (do (set! (.-state this) x) (propagate! this))
          :else         (throw (js/Error. "can't swap! or reset! formula cell")))
    (.-state this))

  cljs.core/ISwap
  (-swap! [this f]        (reset! this (f (.-state this))))
  (-swap! [this f a]      (reset! this (f (.-state this) a)))
  (-swap! [this f a b]    (reset! this (f (.-state this) a b)))
  (-swap! [this f a b xs] (reset! this (apply f (.-state this) a b xs)))

  cljs.core/IWatchable
  (-notify-watches [this o n]
    (when (< 0 (.-numwatches this))
      (doseq [[key f] watches] (f key this o n))))
  (-add-watch [this k f]
    (when-not (contains? (.-watches this) k)
      (set! (.-numwatches this) (inc (.-numwatches this))))
    (set! (.-watches this) (assoc watches k f)))
  (-remove-watch [this k]
    (when (contains? (.-watches this) k)
      (set! (.-numwatches this) (dec (.-numwatches this)))
      (set! (.-watches this) (dissoc watches k)))))

(defn cell? [c]
  (when (= (type c) Cell) c))

(defn formula? [c]
  (when (and (cell? c) (.-thunk c)) c))

(defn lens? [c]
  (when (and (cell? c) (.-update c)) c))

(defn input? [c]
  (when (and (cell? c) (not (formula? c))) c))

(defn constant? [c]
  (when (and (cell? c) (.-constant c)) c))

(defn set-cell! [c x]
  (set! (.-state c) x) (set-formula! c))

(defn cell
  ([x] (cell x :meta nil))
  ([x & {:keys [meta graph]}]
   (let [graph (or graph *graph*)]
     (doto (Cell. meta graph x (next-rank) x (array) (array) nil {} nil false 0)
       (assign-to-graph graph)))))

(defn- validate-single-graph! [graphs]
  (when-not (= (count graphs) 1)
    (throw (ex-info "Cannot create a formula from cells belonging to multiple graphs!"
                    {:graphs graphs})))
  graphs)

(defn- discover-graph [sources]
  (some->> (filter cell? sources)
           (map #(.-graph %))
           (seq)
           (set)
           (validate-single-graph!)
           (first)))

(defn formula
  [f updatefn]
  #(let [sources (.. js/Array -prototype -slice (call (js-arguments)))
         graph (or (discover-graph sources) *graph*)]
     (set-formula!* (cell ::none :graph graph) f sources updatefn)))

;; javelin util ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn dosync*
  [thunk]
  (if *tx*
    (thunk)
    (binding [*tx* (array)]
      (thunk)
      (let [tx *tx*]
        (binding [*tx* nil]
          (propagate* tx))))))
