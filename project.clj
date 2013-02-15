(defproject rosalind-clj "0.1.0-SNAPSHOT"
  :description "Phillip Garland's code for Rosalind problems"
  :url "http://github.com/pgarland/rosalind-clj"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.4.0"]
                 [ring "1.1.8"]
                 [compojure "1.1.5"]
                 [hiccup "1.0.2"]]
    :plugins [[lein-ring "0.8.2" :exclusions [org.clojure/clojure]]]
    :ring {:handler hello-world.handler/app}
    :profiles
    {:dev {:dependencies [[ring-mock "0.1.3"]
                          [ring-server "0.2.7"]]}})
