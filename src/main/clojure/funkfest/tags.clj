(ns funkfest.tags
  (:use [funkfest.utils :as utils]
        [clojure.contrib.seq-utils :only (flatten)]))
;;
;; Types
;;

(defstruct id3v2-tag :header :extended-header :frames :footer :total-length)
(defstruct id3v2-header :major-version :minor-version :flags :tag-length)
(defstruct id3v2-extended-header :length :flags)
(defstruct id3v2-frame-header :id :frame-length :tag-alter-preservation :file-alter-preservation :read-only
           :grouping-identity :compression :encryption :unsynchronisation :data-length-indicator)
(defstruct id3v2-frame :type :header :data)

;;
;; Definitions
;;

(def HEADER_LENGTH 10)
(def FRAME_HEADER_LENGTH 10)
(def HEADER_SIZE_LENGTH 4)

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
;; Helper functions
;;

(defn starts-with-header?
  "Checks if the given data starts with a ID3v2 header."
  [data]
  {:pre [(>= (count data) HEADER_LENGTH)]}
  (let [[id versions flags length] (utils/split-at-pos [3 2 1 4] data)]
    (and
      (= (seq (. "ID3" getBytes)) id)
      (every? #(< % 255) versions)
      ; XXX: strict check for flags?
      (every? #(< % 128) length))))

(defn is-user-defined-text-frame?
  "Checks if the given id is a user defined text frame id"
  [id]
  (= id (:user-defined-text frame-ids))) 

(defn is-unique-file-id-frame?
  [id]
  (= id (:uniq-file-id frame-ids)))

(defn is-padded?
  [data]
  (every? zero? (take FRAME_HEADER_LENGTH data)))

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

(defn to-text-info
  ([data]
     (to-text-info data 0))
  ([data file-enc]
    (String. (byte-array data) (get-encoding file-enc))))

(defn length-to-bytes
  [len]
  [(bit-and 127 (bit-shift-right len 21))
   (bit-and 127 (bit-shift-right len 14))
   (bit-and 127 (bit-shift-right len 7))
   (bit-and 127 len)])

(defn calculate-length
  "Calculates header length from given data."
  [len-data]
  {:pre [(>= (count len-data) HEADER_SIZE_LENGTH)
         (not-any? neg? (take HEADER_SIZE_LENGTH len-data))]}
  (let [raw-bytes (map #(bit-and (short 255) %) (take 4 len-data))
        byte-values [(nth raw-bytes 3)
                     (bit-shift-left (nth raw-bytes 2) 7)
                     (bit-shift-left (nth raw-bytes 1) 14)
                     (bit-shift-left (first raw-bytes) 21)]]
    (reduce bit-or byte-values)))

(defn get-frames-by-id
  [id3v2-tag id]
  (filter #(= (id frame-ids) (:id (:header %))) (seq (:frames id3v2-tag))))

(defn get-frame-data-by-id
  [id3v2-tag id]
  (map #(:data %) (get-frames-by-id id3v2-tag id)))

(defn find-two-nil-bytes
  [data]
  (loop [cur-data data index 0]
    (cond
      (< (count cur-data) 2) -1
      (= [0 0] (take 2 cur-data)) index
      :else (recur (rest cur-data) (inc index)))))

(defn find-nil-byte
  [data]
  (loop [cur-data data index 0]
    (cond
      (< (count cur-data) 1) -1
      (= [0] (first cur-data)) index
      :else (recur (rest cur-data) (inc index)))))

(defn split-by-two-nils
  [data]
  (let [split-pos (find-two-nil-bytes data)]
    (if (>= split-pos 0)
      [(take split-pos data) (drop (+ 2 split-pos) data)]
      [data nil])))

(defn split-by-nil
  [data]
  (let [split-pos (find-nil-byte data)]
    (if (>= split-pos 0)
      [(take split-pos data) (drop (inc split-pos) data)]
      [data nil])))

(defn frame-id-to-type
  [id]
  (cond
    (and
      (= \T (first id))
      (not (= "TXXX" id))) :text-frame
    ;(= "TXXX" id) :user-text-frame
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
    :else nil))

;;
;; User-friendly functions
;;

(defn get-album
  [id3v2-tag]
  (get-frame-data-by-id id3v2-tag :album))

(defn get-lead-performer
  [id3v2-tag]
  (get-frame-data-by-id id3v2-tag :lead-performer))

(defn get-title
  [id3v2-tag]
  (get-frame-data-by-id id3v2-tag :title))

(defn get-year
  [id3v2-tag]
  (get-frame-data-by-id id3v2-tag :year))

(defn get-tag-version
  [id3v2-tag]
  (let [hdr (:header id3v2-tag)]
    (apply str (interpose "." [2 (:major-version hdr) (:minor-version hdr)]))))

;;
;; Convert to string functions
;;

(defn header-to-string
  [hdr]
  (str
    "Header:" \newline
    " - Major version: " (:major-version hdr) \newline
    " - Minor version: " (:minor-version hdr) \newline
    " - Flags: " \newline
    "  - Has footer: " (:footer hdr) \newline
    "  - Is experimental: " (:experimental hdr) \newline
    "  - Contains extended header: " (:extended hdr) \newline
    "  - Unsynchronization: " (:unsynch hdr) \newline
    " - Tag Length: " (:tag-length hdr) \newline))

(defn extended-header-to-string
  [extended-hdr]
  (str
    "Extended header:" \newline
    " - Length: " (:length extended-hdr) \newline
    " - Flags: " (:flags extended-hdr) \newline))

(defn frame-header-to-string
  [frm-hdr]
  (str 
    "Frame header:" \newline
    " - ID: " (:id frm-hdr) \newline
    " - Frame length: " (:frame-length frm-hdr) \newline
    " - Flags:" \newline
    "  - Tag alter preservation: " (:tag-alter-preservation frm-hdr) \newline
    "  - File alter preservation: " (:file-alter-preservation frm-hdr) \newline
    "  - Read only: " (:read-only frm-hdr) \newline
    "  - Grouping identity: " (:grouping-identity frm-hdr) \newline
    "  - Compression: " (:compression frm-hdr) \newline
    "  - Encryption: " (:encryption frm-hdr) \newline
    "  - Unsynchronisation: " (:unsynchronisation frm-hdr) \newline
    "  - Data length indicator: " (:data-length-indicator frm-hdr) \newline))

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

(defmethod frame-to-string nil [frame]
  (str
    (frame-header-to-string (:header frame))
    "Frame:" \newline
    " - Data: " (:data frame) \newline))

(defn tag-to-string
  [id3v2-tag]
  (apply str
    (header-to-string (:header id3v2-tag))
    (if (:extended-header id3v2-tag) (extended-header-to-string (:extended-header id3v2-tag)))
    (map #(frame-to-string %) (:frames id3v2-tag))))

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
(defn bit-set-to [x n pred]
  (if pred
    (bit-set x n)
    (bit-clear x n)))

(defn id3v2-header-to-bytes
  [id3v2-header]
  (flatten
    (seq (. "ID3" getBytes))
    (:major-version id3v2-header)
    (:minor-version id3v2-header)
    (:flags id3v2-header)
    (:tag-length id3v2-header)))

;;
;; Functions
;;

(defn header-has-footer?
  [id3v2-header]
  (bit-test (:flags id3v2-header) 4))

(defn header-is-experimental-tag?
  [id3v2-header]
  (bit-test (:flags id3v2-header) 5))

(defn header-contains-extended-header?
  [id3v2-header]
  (bit-test (:flags id3v2-header) 6))

(defn header-is-unsynchronized?
  [id3v2-header]
  (bit-test (:flags id3v2-header) 7))

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
;                :footer (bit-test flags 4)
;                :experimental (bit-test flags 5)
;                :extended (bit-test flags 6)
;                :unsynch (bit-test flags 7)
                :tag-length (calculate-length raw-len))))

(defn parse-extended-id3v2-header
  [data]
  {:pre [(>= (count data) HEADER_LENGTH)]}
  (let [[raw-len flag-byte-count flags] (utils/split-at-pos [4 1 1] data)]
    (struct-map id3v2-extended-header :length (calculate-length raw-len) :flags flags)))

(defn parse-frame-header
  [data]
  {:pre [(>= (count data) FRAME_HEADER_LENGTH)]}
  (let
    [[raw-id raw-length flags-1 flags-2] (utils/split-at-pos [4 4 1 1] data)]

    (struct-map id3v2-frame-header
                :id (String. (byte-array raw-id))
                :frame-length (calculate-length raw-length)
                :tag-alter-preservation (bit-test flags-1 6)
                :file-alter-preservation (bit-test flags-1 5)
                :read-only (bit-test flags-1 4)
                :grouping-identity (bit-test flags-2 6)
                :compression (bit-test flags-2 3)
                :encrypstion (bit-test flags-2 2)
                :unsynch (bit-test flags-2 1)
                :data-len-indicator (bit-test flags-2 0))))

;;
;; Frame parsers
;;

(defmulti parse-frame-mm (fn [frame-type header frame-data] frame-type))

;
; Text frame
;
; <Header, ID: T[A-Z]{3}, excluding TXXX>
; <Text encoding>                         (1 byte)
; <Text information>                      (text data according the encoding)
(defmethod parse-frame-mm :text-frame [frame-type header frame-data]
  (let [frame-encoding (first frame-data)
        frame-info (to-text-info (rest frame-data) frame-encoding)]
    (struct-map id3v2-frame
            :type frame-type
            :header header
            :data {:encoding frame-encoding :data frame-info})))

;
; Comments frame
;
; <Header, ID: COMM>
; <Text encoding>    (1 byte)
; <Language>         (text data)
; <Two nil bytes>
; <Text information> (text data according the encoding)
(defmethod parse-frame-mm :comments-frame [frame-type header frame-data]
  (let
    [[frame-encoding raw-lang raw-data] (utils/split-at-pos [1 3 :rest])]
    (let [splitted-data (split-by-nil raw-data)]
      (struct-map id3v2-frame
                  :type frame-type
                  :header header
                  :data {:encoding frame-encoding
                         :language (to-text-info raw-lang)
                         :description (first splitted-data)
                         :text (rest splitted-data)}))))

;
; URL link frame
; <Header, ID: W[A-Z]{3}, excluding WXXX>
; <URL> (text data)
(defmethod parse-frame-mm :url-frame [frame-type header frame-data]
  (struct-map id3v2-frame
              :type frame-type
              :header header
              :data {:url (to-text-info (frame-data))}))
;
; User-defined URL frame
;
; <Header, ID: WXXX>
; <Text encoding>    (1 byte)
; <Description>      (text data according the encoding)
; <Two nil bytes>
; <URL>              (text data)
(defmethod parse-frame-mm :user-url-frame [frame-type header frame-data]
  (let [frame-encoding (first frame-data)
        splitted-data (split-by-two-nils (rest frame-data))]
    (struct-map id3v2-frame
            :type frame-type
            :header header
            :data {:encoding frame-encoding :description (first splitted-data)
             :url (rest splitted-data)})))

;
; Default frame parser
;
; Just add the pure data, don't parse it
;
(defmethod parse-frame-mm nil [frame-type header frame-data]
  (struct-map id3v2-frame :type frame-type :header header :data frame-data))

(defn parse-frame
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
  (if (not (starts-with-header? data))
    nil
    (let [header (parse-id3v2-header (take HEADER_LENGTH data))
          extended-header (if (:extended-header header) (parse-extended-id3v2-header))
          headers-length (if extended-header
                           (+ HEADER_LENGTH (:length extended-header))
                           HEADER_LENGTH)
          data-left-without-headers (-
                                      (:tag-length header)
                                      headers-length
                                      (if (header-has-footer? header) (HEADER_LENGTH) 0))]
      ; Parse frames
      (loop [data-left data-left-without-headers
             cur-data (drop headers-length data)
             frames (transient [])]
        (if (and
              (not (is-padded? cur-data))
              (pos? data-left))
          ; Parse single frame
          (let [parsed-frame (parse-frame cur-data)
                frame-len (+ FRAME_HEADER_LENGTH (:frame-length (:header parsed-frame)))]
            (conj! frames parsed-frame)
            (recur (- data-left frame-len) (drop frame-len cur-data) frames))
          ; Finally return the whole tag struct
          (struct-map id3v2-tag
                  :header header
                  :extended-header extended-header
                  :frames (persistent! frames)
                  :footer nil
                  :total-length nil))))))

