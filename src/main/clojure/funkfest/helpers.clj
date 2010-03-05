(ns funkfest.helpers
  (:use [funkfest.common :as common]))

;;
;; User-friendly functions
;;

;
; Tags
;

(defn get-tag-version
  [tag]
  (let [hdr (:header tag)]
    (apply str (interpose "." [2 (:major-version hdr) (:minor-version hdr)]))))

;
; Frames
;

(defn get-album
  [tag]
  (common/get-frame-data-by-id tag :album))

(defn get-lead-performer
  [tag]
  (common/get-frame-data-by-id tag :lead-performer))

(defn get-title
  [tag]
  (common/get-frame-data-by-id tag :title))

(defn get-year
  [tag]
  (common/get-frame-data-by-id tag :year))

