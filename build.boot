(set-env!
  :dependencies   '[[org.clojure/clojure             "1.8.0"      :scope "provided"]
                    [org.clojure/clojurescript       "1.9.293"    :scope "provided"]
                    [adzerk/bootlaces                "0.1.10"     :scope "test"]
                    [adzerk/boot-cljs                "2.0.0"      :scope "test"
                     :exclusions [org.clojure/clojurescript]]
                    [crisptrutski/boot-cljs-test     "0.3.0"      :scope "test"]
                    [adzerk/boot-test                "1.2.0"      :scope "test"]
                    [net.cgrand/macrovich            "0.2.0"]
                    [org.clojure/data.priority-map   "0.0.7"]
                    [riddley                         "0.1.14"]]
  :resource-paths #{"src"})

(require
  '[clojure.java.io  :as io]
  '[adzerk.bootlaces :refer :all]
  '[adzerk.boot-cljs :refer :all]
  '[adzerk.boot-test :as bt]
  '[crisptrutski.boot-cljs-test :as bct])

(def +version+ "3.9.0")

(bootlaces! +version+)

(task-options!
  pom  {:project     'hoplon/javelin
        :version     +version+
        :description "Spreadsheet-like dataflow programming in ClojureScript"
        :url         "https://github.com/hoplon/javelin"
        :scm         {:url "https://github.com/hoplon/javelin"}
        :license     {"Eclipse Public License" "http://www.eclipse.org/legal/epl-v10.html"}})

(deftask test-runner
  []
  (let [tmp (tmp-dir!)]
    (with-pre-wrap fileset
      (with-let [fs fileset]
        (empty-dir! tmp)
        (doseq [[path in-file] (map (juxt tmp-path tmp-file) (output-files fs))]
          (let [out-file (doto (io/file tmp path) io/make-parents)]
            (io/copy in-file out-file)))
        (binding [*sh-dir* (.getPath tmp)] (dosh "bash" "run.sh"))))))

(deftask test-javelin
  "Run Javelin tests"
  [a advanced bool "Test with :advanced optimizations."]
  (merge-env! :resource-paths #{"test"})
  (when advanced (task-options! cljs {:optimizations :advanced}))
  (comp (cljs)
        (test-runner)
        (test-runner)
        (test-runner)
        (test-runner)
        (test-runner)
        (test-runner)
        (test-runner)
        (test-runner)
        (test-runner)
        (test-runner)))

(deftask test-cljs []
  (merge-env! :resource-paths #{"test"})
  (bct/test-cljs :js-env :phantom))

(deftask test-clj []
  (merge-env! :source-paths #{"test"})
  (bt/test))

(deftask repl-test []
  (merge-env! :source-paths #{"test"})
  (repl))
