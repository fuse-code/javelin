(ns javelin.macros.clj
  (:require [riddley.compiler :as compiler]
            [riddley.walk     :as walk]))

(def specials (into #{} (keys (. clojure.lang.Compiler specials))))

(defn hoist [x env]
  (let [hoist    (atom [])
        pass     (atom {})
        local    #(symbol (name %))
        core?    #(= "clojure.core" (namespace %))
        skip?    #(or
                    (= 'quote %)
                    (not (contains? env %))
                    (contains? specials %)
                    (core? %))
        find-sym #(fn [[s l]] (when (= l %) s))
        walk!    (fn [x]
                   (if (skip? x)
                     x
                     (if-let [s (some (find-sym x) @hoist)]
                       s
                       (let [s (gensym "clj")] (swap! hoist conj [s x]) s))))

        ;; unquoted/quoted expressions are preserved as is
        unquote?  #{'clojure.core/unquote 'clojure.core/unquote-splicing}
        unwrap?   #(and (seq? %) (or (= 'quote (first %)) (unquote? (first %))))
        unwrapped #(condp = (first %)
                     'quote                         %
                     'clojure.core/unquote          (second %)
                     'clojure.core/unquote-splicing (list 'deref (second %)))
        walk-unwrap! #(let [s (gensym "clj")] (swap! pass assoc s (unwrapped %)) s)

        walked   (->> (walk/walk-exprs unwrap? walk-unwrap! x)
                      (walk/walk-exprs symbol? walk!))

        [params args] (if (empty? @pass) [[] []] (apply map vector @pass))
        params   (into params (map first @hoist))
        args     (into args (map second @hoist))]
    [`(fn ~params ~walked) args]))
