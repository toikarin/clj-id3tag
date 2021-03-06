(ns clj-id3tag.parser
  (:use
     [clj-id3tag.common :as common]
     [clj-id3tag.utils :as utils]
     [clojure.contrib.seq-utils :only (flatten)]))

;;
;; This file contains functions to
;; parse ID3v2 data.
;;

(def frame-types #{:unique-file-frame
                   :text-frame
                   :user-text-frame
                   :url-frame
                   :user-url-frame
                   :involved-people-frame
                   :music-cd-id-frame
                   :event-timing-frame
                   :mpeg-location-frame
                   :synchronised-tempo-frame
                   :unsynchronised-lyrics-frame
                   :synchronised-lyrics-frame
                   :comments-frame})

(def frame-ids
  {:audio-encryption "AENC"
   :attached-pic "APIC"

   :comments "COMM"
   :commercial-frame "COMR"

   :encryption-method "ENCR"
   :equalization "EQUA"
   :event-timing-codes "ETCO"

   :general-encapsulated-object "GEOB"
   :group-id-registration "GRID"

   :involved-people-list "IPSL"

   :linked-info "IPLS"

   :music-cd-id "MCDI"
   :mpeg-loc-lookup-table "MLLT"

   :ownership "OWNE"

   :private "PRIV"
   :play-counter "PCNT"
   :popularimeter "POPM"
   :position-synch "POSS"

   :recommended-buffer-size "RBUF"
   :relative-vol-adjustment "RVAD"
   :reverb "RVRB"

   :synchronized-lyric "SYLT"
   :synchronized-tempo "SYTC"
   
   :album "TALB"
   :bpm "TBPM"
   :composer "TCOM"
   :content-type "TCON"
   :copyright "TCOP"
   :date "TDAT"
   :playlist-delay "TDLY"
   :encoded "TENC"
   :lyricist "TEXT"
   :file-type "TFLT"
   :time "TIME"
   :content-group-desc "TIT1"
   :title "TIT2"
   :subtitle-refinement "TIT3"
   :initial-key "TKEY"
   :language "TLAN"
   :length "TLEN"
   :media-type "TMED"
   :original-album "TOAL"
   :original-filename "TOFN"
   :original-lyricist "TOLY"
   :original-artist "TOPE"
   :original-year "TORY"
   :file-owner "TOWN"
   :lead-performer "TPE1"
   :band "TPE2"
   :conductor "TPE3"
   :interpreted "TPE4"
   :part "TPOS"
   :publisher "TPUB"
   :track-number "TRCK"
   :recording-dates "TRDA"
   :internet-radio-station-name "TRSN"
   :internet-radio-station-owner "TRSO"
   :size "TSIZ"
   :isrc "TSRC"
   :encoding-software "TSSE"
   :year "TYER"
   :user-defined-text "TXXX"
   
   :uniq-file-id "UFID"
   :terms-of-use "USER"
   :unsynch-lyric "USLT"

   :commercial-info "WCOM"
   :copyright-info "WCOP"
   :official-audio-file-url "WOAF"
   :official-artist-url "WOAR"
   :official-audio-source-url "WOAS"
   :official-internet-radio-station-url "WORS"
   :payment "WPAY"
   :official-publisher-url "WPUB"
   :user-defined-urls "WXXX"
   })

;;
;; Logging (remove this?)
;;

(defn log-debug
  [& more]
  (apply println "DBG: " more))

(defn log-error
  [& more]
  (apply println "ERR: " more))

;;
;; Misc functions

(defn starts-with-header?
  "Checks if the given data starts with a ID3v2 header."
  [data]
  {:pre [(>= (count data) HEADER_LENGTH)]}
  (let [[id versions flags length] (utils/split-at-pos [3 2 1 4] data)]
    (and
      (= (utils/str-to-bytes "ID3") id)
      (every? #(< % 255) versions)
      ; XXX: strict check for flags?
      (every? #(< % 128) length))))

(defn calculate-padding-length
  "Calculate padding length from given data.

  Checks how many nil bytes are in the front of the given sequence.

  Returns a map containing :length and :rest keys.
  Length is the number of nil bytes and rest is the data after
  the padding."
  [data]
  (loop [cur-data data
        len 0]
    (if (or
          (empty? cur-data)
          (not= 0 (first cur-data)))
      {:length len :rest cur-data}
      (recur (rest cur-data) (inc len)))))

(defn get-encoding
  "Get text encoding by byte value"
  [text-encoding]
  (cond
    (= 0 text-encoding) "ISO-8859-1"
    (= 1 text-encoding) "UTF-16"
    (= 2 text-encoding) "UTF-16"
    (= 3 text-encoding) "UTF-8"
    :else (do
            (log-error "Invalid encoding: " text-encoding ", using default instead.")
            (get-encoding 0))))

(defn calculate-length
  "Calculates header length from header's length data."
  [len-data]
  {:pre [(>= (count len-data) HEADER_SIZE_LENGTH)
         (not-any? neg? (take HEADER_SIZE_LENGTH len-data))]}
  (let [raw-bytes (map #(bit-and (short 255) %) (take 4 len-data))
        byte-values [(nth raw-bytes 3)
                     (bit-shift-left (nth raw-bytes 2) 7)
                     (bit-shift-left (nth raw-bytes 1) 14)
                     (bit-shift-left (first raw-bytes) 21)]]
    (reduce bit-or byte-values)))

(defn frame-id-to-type
  "Converts the frame's id to internal type."
  [id]
  (cond
    (and
      (= \T (first id))
      (not (= "TXXX" id))) :text-frame
    (= "TXXX" id) :user-text-frame
    (= "COMM" id) :comments-frame
    ;(= "UFID" id) :unique-file-frame
    (and
      (= \W (first id))
      (not (= "WXXX" id))) :url-frame
    (= "WXXX" id) :user-url-frame
    ;(= "IPLS" id) :involved-people-frame
    ;(= "MCDI" id) :music-cd-id-frame
    ;(= "ETCO" id) :event-timing-frame
    ;(= "MPEG" id) :mpeg-location-frame
    ;(= "SYTC" id) :synchronised-tempo-frame
    ;(= "USLT" id) :unsynchronised-lyrics-frame
    ;(= "SYLT" id) :synchronised-lyrics-frame
    ;(= "RVAD" id) :relative-vol-adjustment-frame
    ;(= "EQUA" id) :equalisation-frame
    ;(= "RVRB" id) :reverb-frame
    ;(= "APIC" id) :general-encapsulated-object-frame
    :else :unknown-frame))

;;
;; Helper functions
;;

(defn to-text-info
  ([data]
     (to-text-info data 0))
  ([data file-enc]
    (String. (byte-array data) (get-encoding file-enc))))

(defn split-by-nil
  [data]
  (common/split-drop-separator #(not= 0 %) data))

;;
;; Info about the ID3v2 tags...
;;
;; +------------------------------------------------------------------------------+
;; | Header (10 bytes)                                                            |
;; +------------------------------------------------------------------------------+
;; | - 3 bytes: ID3                                                               |
;; | - 1 byte: Major version                                                      |
;; | - 1 byte: Minor version                                                      |
;; | - 1 byte: flags                                                              | 
;; |   - 4th bit: Indicates if the footer is present                              |
;; |   - 5th bit: Indicates if the tag is experimental                            |
;; |   - 6th bit: Indicates if the extended header follows the header             |
;; |   - 7th bit: Indicates if the unsynchronisation is applied to all frames     |
;; | - 4 bytes: The sum of the byte length of the extended header, the padding,   |
;; |            and the frames after unsynchronisation. Does not include footer   |
;; +------------------------------------------------------------------------------+
;; | Extended header (variable length, optional)                                  |
;; +------------------------------------------------------------------------------+
;; | - 4 bytes: Size of the whole extended header                                 |
;; | - 1 byte: Number of flag bytes                                               |
;; | - 1 byte: extended flags                                                     |
;; +------------------------------------------------------------------------------+
;; | Frames (variable length)                                                     |
;; +------------------------------------------------------------------------------+
;; | - 10 bytes: Frame header                                                     |
;; |  - 4 bytes: Frame ID                                                         |
;; |  - 4 bytes: Frame size (excluding frame header)                              |
;; |  - 2 bytes: Flags                                                            |
;; | - Frame (size and data depends on frame id and size)                         |
;; +------------------------------------------------------------------------------+
;; | Padding (variable length, optional)                                          |
;; +---------------------------------------------------------------------------=--+
;; | Footer (10 bytes, identical to header except the 3 first bytes equal to 3DI) |
;; +-----------------------------------------------------------------------------=+
;;
;;

;;
;; Functions
;;

(defn parse-id3v2-header
  "
  Parses header from the given data and returns the parsed header.
  "
  [data]
  {:pre [(>= (count data) HEADER_LENGTH)
         (starts-with-header? data)]}
  (let [[_ major-version minor-version flags raw-len] (utils/split-at-pos [3 1 1 1 4] data)]
    (struct-map id3v2-header
                :major-version major-version
                :minor-version minor-version
                :flags flags
                :tag-length (calculate-length raw-len))))

(defn parse-extended-id3v2-header
  [data]
  {:pre [(>= (count data) HEADER_LENGTH)]}
  (let [[raw-len flag-byte-count flags] (utils/split-at-pos [4 1 1] data)]
    (struct-map id3v2-extended-header :length (calculate-length raw-len) :flags flags)))

(defn parse-frame-header
  [data]
  {:pre [(>= (count data) FRAME_HEADER_LENGTH)]}
  (let [[raw-id raw-length flags-1 flags-2] (utils/split-at-pos [4 4 1 1] data)]
    (struct-map id3v2-frame-header
                :id (String. (byte-array raw-id))
                :frame-length (calculate-length raw-length)
                :flags-1 flags-1
                :flags-2 flags-2)))

;;
;; Frame parsers
;;

(defmulti parse-frame-mm (fn [frame-type header frame-data] frame-type))

(defmacro defframe-data-parser
  [frame-type fdata]
  `(defmethod parse-frame-mm ~frame-type [frame-type# header# frame-data#]
    (let [parsed-data# (~fdata frame-data#)]
      (struct-map id3v2-frame
                  :type ~frame-type
                  :header header#
                  :data parsed-data#))))

;
; Text frame
;
; <Header, ID: T[A-Z]{3}, excluding TXXX>
; <Text encoding>                         (1 byte)
; <Text information>                      (text data according the encoding)
(defframe-data-parser
  :text-frame #(let [enc (first %)]
                 {:encoding enc
                  :data (to-text-info (rest %) enc)}))

;
; User defined text frame
;
; <Header, ID: TXXX>
; <Text encoding>                         (1 byte)
; <Description>                           (text data according the encoding)
; <Nil byte>
; <Value>                                 (text data according the encoding)
(defframe-data-parser
  :user-text-frame #(let [enc (first %)
                          splitted-data (split-by-nil (rest %))]
                      {:encoding enc
                       :description (to-text-info (first splitted-data) enc)
                       :value (to-text-info (second splitted-data) enc)}))

;
; Comments frame
;
; <Header, ID: COMM>
; <Text encoding>    (1 byte)
; <Language>         (text data)
; <Two nil bytes>
; <Text information> (text data according the encoding)
(defframe-data-parser :comments-frame
  #(let [splitted (utils/split-at-pos [1 3 :rest] %)]
     (apply hash-map [:encoding :language :fixme] splitted)))

;
; URL link frame
; <Header, ID: W[A-Z]{3}, excluding WXXX>
; <URL> (text data)
(defframe-data-parser :url-frame #({:url (to-text-info %)}))

;
; User-defined URL frame
;
; <Header, ID: WXXX>
; <Text encoding>    (1 byte)
; <Description>      (text data according the encoding)
; <Nil byte>
; <URL>              (text data)
(defframe-data-parser :user-url-frame
  #(let [enc (first %)
         splitted-data (split-by-nil (rest %))]
     {:encoding enc
      :description (to-text-info (first splitted-data) enc)
      :url (to-text-info (second splitted-data))}))

;
; Default frame parser
;
; Just add the pure data, don't parse it
;
(defframe-data-parser :default identity)

(defn parse-frame
  "Parse a single frame."
  [data]
  (let [header (parse-frame-header data)
        frame-type (frame-id-to-type (:id header))
        frame (drop FRAME_HEADER_LENGTH (take (+ FRAME_HEADER_LENGTH (:frame-length header)) data))]
    (if (empty? frame)
      (struct-map id3v2-frame :type frame-type :header header :data nil)
      (parse-frame-mm frame-type header frame))))

;; REMOVE
(defn parse-frame-data
  [id encoding data]
  (let [conv (fn [data] (String. (byte-array data) (get-encoding encoding)))]
    (cond
      ;(is-text-frame? id) (conv data)
      ;(is-user-defined-text-frame? id) (let
      ;                                   [split-pos (split-by-two-nils data)]
      ;                                   (if (>= 0 split-pos)
      ;                                     [(conv (take split-pos data)) (conv (drop (+ split-pos 2) data))]))
      ;(is-unique-file-id-frame? id) (let
      ;                                [split-pos (split-by-two-nils data)]
      ;                                (if (>= 0 split-pos)
      ;                                  [(conv (take split-pos data)) (drop (+ split-pos 2) data)]))
      ;(is-url-link-frame? id) nil
      
      ;(let
      ;                          [split-pos (split-by-two-nils data)]
      ;                         ; if the textstring is followed by a termination (00 00) all the
      ;                         ; following information should be ignored and not be displayed
      ;                           (if (>= split-pos 0)
      ;                             (String. (byte-array (take split-pos data)))
      ;                             (String. (byte-array data))))
      ; User defined URL link frame
      ;
      ; <Header, ID: WXXX>
      ; <Text encoding>  (1 byte)
      ; <Description>    (text string according to encoding
      ; <Text separator> (two nil bytes)
      ; <URL>            (text string)
      ;(is-user-defined-url-link-frame? id) nil
      ; Involved people list frame
      ;
      ; <Header, ID: IPLS>
      ; <Text encoding> (1 byte)
      ; <String>        (text string according to encoding)
      ;
      ; The body simply contains a terminated string with the involvement directly followed
      ; by a terminated string with the involvee followed by a new involvement...
      ;(is-involved-people-list-frame? id) nil
      ; Music CD identifier
      ;
      ; <Header, ID: MCDI>
      ; <CD TOC> (binary data)
      ;(is-music-cd-identifier-frame? id) nil
      ; Event timing codes
      ;
      ; <Header, ID: ETCO>
      ; <Time stamp format> (1 byte)
      ; <Event type>        (1 byte)
      ; <Timestamp>         (1 byte)
      ;(is-event-timing-code-frame? id) nil
      ;(is-comment-frame? id) nil)))
      )))

;; Main parser

(defn parse-id3v2
  [data]
  (if-not (starts-with-header? data)
    nil
    (let [header (parse-id3v2-header (take HEADER_LENGTH data))
          extended-header (if (:extended-header header) (parse-extended-id3v2-header))
          headers-length (if extended-header
                           (+ HEADER_LENGTH (:length extended-header))
                           HEADER_LENGTH)
          data-left-without-headers (-
                                      (:tag-length header)
                                      headers-length
                                      (if (header-flag-set? :footer header) (HEADER_LENGTH) 0))]
      ; Parse frames
      (loop [data-left data-left-without-headers
             cur-data (drop headers-length data)
             frames (transient [])
             padding-length 0]
        (cond
          ; Finally return the whole tag struct
          (not (pos? data-left)) (struct-map id3v2-tag
                                             :header header
                                             :extended-header extended-header
                                             :padding padding-length
                                             :frames (persistent! frames)
                                             :footer nil
                                             :total-length nil)
          ; Skip padding
          (= 0 (first cur-data)) (let [padding (calculate-padding-length cur-data)]
                                  (recur (- data-left (:length padding)) (:rest padding) frames (:length padding)))
          ; Parse single frame
          :else (let [parsed-frame (parse-frame cur-data)
                      frame-len (+ FRAME_HEADER_LENGTH (:frame-length (:header parsed-frame)))]
                  (conj! frames parsed-frame)
                  (recur (- data-left frame-len) (drop frame-len cur-data) frames padding-length)))))))

