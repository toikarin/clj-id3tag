(ns funkfest.common
  (:use [clojure.template :as tmpl]))

;;
;; Types
;;

(defstruct id3v2-tag :header :extended-header :padding :frames :footer :total-length)
(defstruct id3v2-header :major-version :minor-version :flags :tag-length)
(defstruct id3v2-extended-header :length :flags)
(defstruct id3v2-frame-header :id :frame-length :flags-1 :flags-2)
(defstruct id3v2-frame :type :header :data)

;;
;; Definitions
;;

(def HEADER_LENGTH 10)
(def FRAME_HEADER_LENGTH 10)
(def HEADER_SIZE_LENGTH 4)

;;
;; Functions
;;

(defmacro
  #^{:private true}
  create-flag-set-method
  [method-name method-key flag-key bit-op bit]
  `(defmethod ~method-name ~method-key
     [_# m#]
     (assoc m# ~flag-key (~bit-op (~flag-key m#) ~bit))))

(defmacro
  #^{:private true}
  create-flag-get-method
  [method-name method-key flag-key bit]
  `(defmethod ~method-name ~method-key
     [_# m#]
     (bit-test (~flag-key m#) ~bit)))

;
;; Header flag handlers
;

(defmulti header-flag-set? (fn [k header] k))
(defmulti header-set-flag (fn [k header] k))
(defmulti header-clear-flag (fn [k header] k))

(tmpl/do-template [method-key flag-holder bit]
  (do
    (create-flag-get-method header-flag-set? method-key flag-holder bit)
    (create-flag-set-method header-set-flag method-key flag-holder bit-set bit)
    (create-flag-set-method header-clear-flag method-key flag-holder bit-clear bit))

:footer          :flags 4
:experimental    :flags 5
:extended        :flags 6
:unsynchronized  :flags 7)


;
; Frame flag handlers
;

(defmulti frame-flag-set? (fn [k frame] k))
(defmulti frame-set-flag (fn [k frame] k))
(defmulti frame-clear-flag (fn [k frame] k))

(tmpl/do-template [method-key flag-holder bit]
  (do
    (create-flag-get-method frame-flag-set? method-key flag-holder bit)
    (create-flag-set-method frame-set-flag method-key flag-holder bit-set bit)
    (create-flag-set-method frame-clear-flag method-key flag-holder bit-clear bit))

:tag-alter-preservation  :flags-1 6
:file-alter-preservation :flags-1 5
:read-only               :flags-1 4
:grouping-identity       :flags-2 6
:compression             :flags-2 3
:encryption              :flags-2 2
:unsynchronisation       :flags-2 1
:data-length-indicator   :flags-2 0)


;
;
;

(defn massoc
  [m k f]
  (assoc m k (f (k m))))

;(defn add-frame
;  [tag frame]
;  (let [cur-frames (:frames tag)
;        cur-header (:header tag)
;        mod-frames (conj cur-frames frame)
;        mod-header (massoc cur-header :tag-length #(+ (:length (:header frame)) %)
;        mod-header (assoc cur-header :tag-length (+ (:tag-length cur-head))))
;        ]
;  (do
;    (assoc tag :frames (conj cur-frames frame))
;    )))
    ;(assoc tag :
    ;       (conj (:

(defn create-header
  []
  (struct-map id3v2-header
              :major-version 3
              :minor-version 2
              :flags 0
              :tag-length 0)) 

(defn create-empty-tag
  []
  (struct-map id3v2-tag
              :header (create-header)
              :extended-header nil
              :padding 0
              :frames []
              :footer nil
              :total-length HEADER_LENGTH))

