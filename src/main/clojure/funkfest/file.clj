(ns funkfest.file
  (:use
     [clojure.contrib.duck-streams :only (to-byte-array)]
     [funkfest.tags :only (log-error log-debug parse-id3v2 tag-to-string)])
  (:import
     (java.io File)))

;; Remove this

(defn mp3 
  []
  (File. "/home/oikku/mp3/iron_maiden/iron_maiden/01_-_prowler.mp3"))

(defn read-file
  [file]
  (to-byte-array file))

(defn
  is-mp3?
  [file]
  (and
    (.. file (toString) (endsWith ".mp3"))
    (. file isFile)))

(defn mp3-list
  [root-dir]
  (filter is-mp3? (file-seq (File. root-dir))))

(defn read-directory
  [root-dir]
  (let [filelist (mp3-list root-dir)]
    (doseq [file filelist]
      (log-debug (. file toString))
      (log-debug (tag-to-string (parse-id3v2 (read-file file)))))))

