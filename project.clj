(defproject clj-id3 "0.0.1-SNAPSHOT"
  :descpription "ID3v2 parser written in Clojure."
  :namespaces [funkfest.common
               funkfest.utils
               funkfest.tags
               funkfest.writer
               funkfest.str
               funkfest.helpers]
  :dependencies [[org.clojure/clojure "1.1.0"]
                 [org.clojure/clojure-contrib "1.1.0"]]
  :dev-dependencies [[org.clojars.gilbertl/vimclojure "2.1.2"]
                     [jline "0.9.94"]]
  :source-path "src/main/clojure"
  :resources-path "src/main/resources"
  :test-path "src/test/clojure")

