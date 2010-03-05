(ns funkfest.parser-tests
  (:require [funkfest.parser :as p])
  (:use clojure.test
        [funkfest.common :as common]
        [funkfest.utils :as utils]
        [clojure.contrib.seq-utils :only (flatten)]))

;(deftest test-length-to-bytes
;  (is (= (p/length-to-bytes 0)) (utils/to-bytes 0 0 0 0))
;  (is (= (p/length-to-bytes 1)) (utils/to-bytes 0 0 0 1))
;  (is (= (p/length-to-bytes 128)) (utils/to-bytes 0 0 1 0))
;  (is (= (p/length-to-bytes 16384)) (utils/to-bytes 0 1 0 0))
;  (is (= (p/length-to-bytes 2097152)) (utils/to-bytes 1 0 0 0))
;  (is (= (p/length-to-bytes 2113665)) (utils/to-bytes 1 1 1 1))
;  (is (= (p/length-to-bytes 268435455)) (utils/to-bytes 127 127 127 127)))

(deftest test-calculate-length
  (testing "basic usage"
    (is (= (p/calculate-length (utils/to-bytes 0 0 0 0)) 0))
    (is (= (p/calculate-length (utils/to-bytes 0 0 0 1)) 1))
    (is (= (p/calculate-length (utils/to-bytes 0 0 1 0)) 128))
    (is (= (p/calculate-length (utils/to-bytes 0 1 0 0)) 16384))
    (is (= (p/calculate-length (utils/to-bytes 1 0 0 0)) 2097152))
    (is (= (p/calculate-length (utils/to-bytes 1 1 1 1)) 2113665))
    (is (= (p/calculate-length (utils/to-bytes 127 127 127 127)) 268435455))))
;  (testing "circular"
;    (let [test-circular (fn [len] (is (= (p/calculate-length (p/length-to-bytes len)) len)))]
;      (test-circular 0)
;      (test-circular 1)
;      (test-circular 128)
;      (test-circular 16384)
;      (test-circular 2097152)
;      (test-circular 2113665)
;      (test-circular 268435455))))

(deftest test-calculate-padding-length
  (let [test-fn (fn [len v]
                  (is (= {:length len :rest (drop len v)} (p/calculate-padding-length v))))]
    (test-fn 0 [1 2 3])
    (test-fn 0 [1 0 2])
    (test-fn 1 [0 1 2])
    (test-fn 1 [0 1 0])
    (test-fn 3 [0 0 0])))

(deftest test-parse-header
  (let [empty-header (common/create-header)
        ; Create flags
        flags (->> empty-header
                (header-set-flag :footer)
                (header-set-flag :experimental)
                (header-set-flag :extended)
                (header-set-flag :unsynchronized)
                (:flags))
        ; Parse header
        parsed-header (p/parse-id3v2-header (seq (utils/to-bytes "ID3" 1 2 flags 0 0 0 1)))]
    (is (= 1 (:major-version parsed-header)))
    (is (= 2 (:minor-version parsed-header)))
    (is (true? (header-flag-set? :footer parsed-header)))
    (is (true? (header-flag-set? :experimental parsed-header)))
    (is (true? (header-flag-set? :extended parsed-header)))
    (is (true? (header-flag-set? :unsynchronized parsed-header)))
    (is (= 1 (:tag-length parsed-header)))))

