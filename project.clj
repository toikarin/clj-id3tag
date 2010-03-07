(defproject clj-id3tag "0.0.1-SNAPSHOT"
  :descpription "ID3v2 parser written in Clojure."
  :namespaces [clj-id3tag.common
               clj-id3tag.utils
               clj-id3tag.tags
               clj-id3tag.writer
               clj-id3tag.str
               clj-id3tag.helpers]
  :dependencies [[org.clojure/clojure "1.1.0"]
                 [org.clojure/clojure-contrib "1.1.0"]]
  :dev-dependencies [[org.clojars.gilbertl/vimclojure "2.1.2"]
                     [jline "0.9.94"]]
  :source-path "src/main/clojure"
  :resources-path "src/main/resources"
  :test-path "src/test/clojure")

