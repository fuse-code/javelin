;; Copyright (c) Alan Dipert and Micha Niskin. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns javelin.macros.js
  (:require
    [clojure.walk    :refer [prewalk]]
    [clojure.pprint  :as p]
    [clojure.string  :as s]
    [cljs.core]
    [cljs.analyzer   :as a]
    #? (:clj [net.cgrand.macrovich :as macro]))
  #? (:cljs (:require-macros [net.cgrand.macrovich :as macro])))

;; util ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn macroexpand*
  "Expand form if it is a CLJS macro, otherwise just return form."
  [env form]
  (if (seq? form)
    (let [ex (a/macroexpand-1 env form)]
      (if (identical? ex form)
        form
        (macroexpand* env ex)))
    form))

(defn macroexpand-all*
  "Fully expand all CLJS macros contained in form."
  [env form]
  (prewalk (partial macroexpand* env) form))

(macro/deftime

(defmacro macroexpand-all
  "Fully expand all CLJS macros contained in form."
  [form]
  (macroexpand-all* &env form))

(defmacro mx
  "Expand all macros in form and pretty-print them (as code)."
  [form]
  `(println
     ~(with-out-str
        (p/write (macroexpand-all* &env form) :dispatch p/code-dispatch))))

(defmacro mx2
  "Expand all macros in form and pretty-print them (as data)."
  [form]
  `(println
     ~(with-out-str
        (p/write (macroexpand-all* &env form)))))

)

(declare walk)

;; javelin cells ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#? (:clj (create-ns 'js))

(defn- ex [msg]
  #? (:cljs (js/Error. msg)
      :clj  (Exception. msg)))

(def ^:dynamic *env*    nil)
(def ^:dynamic *hoist*  nil)
(def ^:dynamic *pass*   nil)

(def ^:private to-list   #(into '() (reverse %)))
(def ^:private special   a/specials)
(def ^:private special?  #(contains? special %))
(def ^:private unsupp?*  #(contains? '#{def ns deftype* defrecord*} %))
(def ^:private core?     #(contains? #{"clojure.core" "cljs.core" "js"} (namespace %)))
(def ^:private empty?*   #(= 0 (count %)))
(def ^:private dot?      #(= '. (first %)))
(def ^:private try?      #(= 'try (first %)))
(def ^:private finally?  #(= 'finally (first %)))
(def ^:private binding1? #(contains? '#{let* loop*} (first %)))
(def ^:private binding2? #(= 'letfn* (first %)))
(def ^:private binding3? #(= 'fn* (first %)))
(def ^:private catch?    #(= 'catch (first %)))
(def ^:private quoted?   #(= 'quote (first %)))
(def ^:private unwrap1?  #(= 'clojure.core/unquote (first %)))
(def ^:private unwrap2?  #(= 'clojure.core/unquote-splicing (first %)))
(def ^:private err1      #(str "formula expansion contains unsupported form: " %))

(defn unsupp? [x local]
  (let [op (first x)]
    (and (not (*env* op)) (not (local op)) (unsupp?* op))))

(defn hoist? [x local]
  (and (not (or (local x) (core? x))) (or (*env* x) (not (special? x)))))

(defn walk-sym [x local]
  (if-not (hoist? x local)
    x
    (let [h (@*hoist* x)]
      (when-not h (swap! *hoist* conj (with-meta x {::h (gensym)})))
      (::h (meta (@*hoist* x))))))

(defn walk-map [x local]
  (into (empty x) (map #(mapv (fn [x] (walk x local)) %) x)))

(defn walk-seq [x local]
  (into (empty x) (map #(walk % local) x)))

(defn walk-bind1 [[sym bindings & body] local]
  (let [local     (atom local)
        bind1     (fn [[k v]]
                    (let [x [k (walk v @local)]]
                      (swap! local conj k) x))
        bindings  (mapcat bind1 (partition 2 bindings))]
    (to-list `(~sym [~@bindings] ~@(map #(walk % @local) body)))))

(defn walk-catch [[sym etype bind & body] local]
  (to-list `(~sym ~etype ~bind ~@(map #(walk % (conj local bind)) body))))

(defn walk-finally [[sym & body] local]
  (to-list `(~sym ~@(map #(walk % local) body))))

(defn walk-try [[sym & body] local]
  (to-list `(~sym ~@(map #((cond (not (seq? %)) walk
                                 (catch?   %)   walk-catch
                                 (finally? %)   walk-finally
                                 :else          walk)
                           % local)
                         body))))

(defn walk-bind2 [[sym bindings & body] local]
  (let [local     (reduce conj local (map first (partition 2 bindings)))
        bindings  (map #(%1 %2) (cycle [identity #(walk % local)]) bindings)]
    (to-list `(~sym [~@bindings] ~@(map #(walk % local) body)))))

(defn walk-bind3 [[sym & arities] local]
  (let [fname   (when (symbol? (first arities)) [(first arities)])
        arities (if fname (rest arities) arities)
        arities (if (vector? (first arities)) [arities] arities)
        local   (if fname (conj local (first fname)) local)]
    (let [mkarity (fn [[bindings & body]]
                    (let [local (into local (remove #(= '& %) bindings))]
                      (to-list `([~@bindings] ~@(map #(walk % local) body)))))
          arities (map mkarity arities)]
      (to-list `(~sym ~@fname ~@arities)))))

(defn walk-passthru [x local]
  (let [s (gensym)] (swap! *pass* assoc s x) s))

(defn walk-dot [[sym obj meth & more] local]
  (let [obj       (walk obj local)
        more      (map #(walk % local) more)
        walk-meth (fn [m] (list (first m) (map #(walk % local) (rest m))))]
    (to-list `(~sym ~obj ~@(if-not (seq? meth) `[~meth ~@more] [(walk-meth meth)])))))

(defn walk-list [x local]
  (let [unsupp? #(unsupp? % local)]
    (cond (empty?*   x) x
          (dot?      x) (walk-dot x local)
          (try?      x) (walk-try x local)
          (binding1? x) (walk-bind1 x local)
          (binding2? x) (walk-bind2 x local)
          (binding3? x) (walk-bind3 x local)
          (quoted?   x) (walk-passthru x local)
          (unwrap1?  x) (walk-passthru (second x) local)
          (unwrap2?  x) (walk-passthru (list 'deref (second x)) local)
          (unsupp?   x) (throw (ex (err1 (first x))))
          :else         (to-list (map #(walk % local) x)))))

(defn walk [x local]
  (cond (symbol? x) (walk-sym x local)
        (map?    x) (walk-map x local)
        (set?    x) (walk-seq x local)
        (vector? x) (walk-seq x local)
        (seq?    x) (walk-list x local)
        :else       x))

(defn hoist [x env]
  (binding [*env* env, *hoist* (atom #{}), *pass* (atom {})]
    (let [body          (walk (macroexpand-all* env x) #{})
          [params args] (if (empty? @*pass*) [[] []] (apply map vector @*pass*))
          params        (into params (map #(::h (meta %)) @*hoist*))
          args          (into args @*hoist*)]
      [(list 'fn params body) args])))
