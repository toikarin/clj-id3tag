(ns clj-id3tag.str
  (:use [clj-id3tag.common]))

;;
;; This file contains functions to convert parsed ID3v2 data to string.
;;


;;
;; Convert to string functions
;;

;
; Header
;

(defn header-to-string
  [hdr]
  (str
    "Header:" \newline
    " - Major version: " (:major-version hdr) \newline
    " - Minor version: " (:minor-version hdr) \newline
    " - Flags: " \newline
    "  - Has footer: " (header-flag-set? :footer hdr) \newline
    "  - Is experimental: " (header-flag-set? :experimental hdr) \newline
    "  - Contains extended header: " (header-flag-set? :extended hdr) \newline
    "  - Unsynchronization: " (header-flag-set? :unsynchronized hdr) \newline
    " - Tag Length: " (:tag-length hdr) \newline))

;
; Extended header
;

(defn extended-header-to-string
  [extended-hdr]
  (str
    "Extended header:" \newline
    " - Length: " (:length extended-hdr) \newline
    " - Flags: " (:flags extended-hdr) \newline))

;
; Frame header
;

(defn frame-header-to-string
  [frm-hdr]
  (str 
    "Frame header:" \newline
    " - ID: " (:id frm-hdr) \newline
    " - Frame length: " (:frame-length frm-hdr) \newline
    " - Flags:" \newline
    "  - Tag alter preservation: " (frame-flag-set? :tag-alter-preservation frm-hdr) \newline
    "  - File alter preservation: " (frame-flag-set? :file-alter-preservation frm-hdr) \newline
    "  - Read only: " (frame-flag-set? :read-only frm-hdr) \newline
    "  - Grouping identity: " (frame-flag-set? :grouping-identity frm-hdr) \newline
    "  - Compression: " (frame-flag-set? :compression frm-hdr) \newline
    "  - Encryption: " (frame-flag-set? :encryption frm-hdr) \newline
    "  - Unsynchronisation: " (frame-flag-set? :unsynchronisation frm-hdr) \newline
    "  - Data length indicator: " (frame-flag-set? :data-length-indicator frm-hdr) \newline))

;
; Frames
;

(defmulti frame-to-string :type)
(defmethod frame-to-string :text-frame
  [frame]
  (str
    (frame-header-to-string (:header frame))
    "Frame:" \newline
    " - Text encoding: " (:encoding (:data frame)) \newline
    " - Info: " (:data (:data frame)) \newline))

(defmethod frame-to-string :comments-frame
  [frame]
  (str
    (frame-header-to-string (:header frame))
    "Frame:" \newline
    " - Text encoding: " (:encoding (:data frame)) \newline
    " - Language: " (:language (:data frame)) \newline
    " - Info: " (:data (:data frame)) \newline))

(defmethod frame-to-string :url-frame
  [frame]
  (str
    (frame-header-to-string (:header frame))
    "Frame:" \newline
  " - URL: " (:url (:data frame)) \newline))

(defmethod frame-to-string :user-url-frame
  [frame]
  (str
    (frame-header-to-string (:header frame))
    "Frame:" \newline
    " - Text encoding: " (:encoding (:data frame)) \newline
    " - Description: " (:description (:data frame)) \newline
    " - URL: " (:url (:data frame)) \newline))

(defmethod frame-to-string :user-text-frame [frame]
  (str
    (frame-header-to-string (:header frame))
    "Frame:" \newline
    " - Text encoding: " (:encoding (:data frame)) \newline
    " - Description: " (:description (:data frame)) \newline
    " - Value: " (:value (:data frame)) \newline))

(defmethod frame-to-string :default [frame]
  (str
    (frame-header-to-string (:header frame))
    "Frame:" \newline
    " - Data: " (:data frame) \newline))

;
; Tag
;

(defn tag-to-string
  [id3v2-tag]
  (apply str
         (header-to-string (:header id3v2-tag))
         (if (:extended-header id3v2-tag) (extended-header-to-string (:extended-header id3v2-tag)))
         "Padding: " (:padding id3v2-tag) " bytes." \newline
         (map frame-to-string (:frames id3v2-tag))))

