(ns clj-id3tag.writer
  (:require [[clj-id3tag.utils :as utils]
             [clj-id3tag.common :as common]]))

;;
;; This file contains functions to write ID3v2 data structures to files.
;;

;;
;; Helpers
;;

(defn length-to-bytes
  [len]
  [(bit-and 127 (bit-shift-right len 21))
   (bit-and 127 (bit-shift-right len 14))
   (bit-and 127 (bit-shift-right len 7))
   (bit-and 127 len)])

(defn create-padding
  [n-bytes]
  (replicate n-bytes (byte 0)))

;;
;; Functions
;;

(defn header->bytes
  "Convert a header to bytes."
  [header]
  (utils/to-bytes
    "ID3"
    (:major-version header)
    (:minor-version header)
    (:flags header)
    (length-to-bytes (:tag-length header))))

(defn extended-header->bytes
  "Convert an extended header to bytes."
  [eheader]
  (utils/to-bytes
    (length-to-bytes (:length eheader))
    (:flags eheader)))

(defn frame-header->bytes
  "Convert a frame header to bytes."
  [fheader]
  (utils/to-bytes
    (:id fheader)
    (length-to-bytes (:frame-length fheader))
    (:flags-1 fheader)
    (:flags-2 fheader)))

(defn frame->bytes
  "Convert a frame to bytes. Includes frame header."
  [frame]
  (conj (frame-header->bytes (:header frame))
        (utils/to-bytes (:data frame))))

(defn tag->bytes
  "Convert a tag to bytes.
  
   Tag consists of header, optional extended header,
   and all the frames.
  "
  [tags]
  (let [bytes (transient [])
        header (:header tags)
        eheader (:extended-header tags)]
  (do
    (conj! bytes (header->bytes header))
    (if eheader (conj! bytes (extended-header->bytes eheader)))
    (persistent! bytes))))


