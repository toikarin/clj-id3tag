(ns funkfest.tags-tests
  (:require [funkfest.tags :as t])
  (:use clojure.test))

(deftest test-length-to-bytes
  (let [to-bytes (fn [& bytes] (byte-array (map byte bytes)))]
    (is (= (t/length-to-bytes 0)) (to-bytes 0 0 0 0))
    (is (= (t/length-to-bytes 1)) (to-bytes 0 0 0 1))
    (is (= (t/length-to-bytes 128)) (to-bytes 0 0 1 0))
    (is (= (t/length-to-bytes 16384)) (to-bytes 0 1 0 0))
    (is (= (t/length-to-bytes 2097152)) (to-bytes 1 0 0 0))
    (is (= (t/length-to-bytes 2113665)) (to-bytes 1 1 1 1))
    (is (= (t/length-to-bytes 268435455)) (to-bytes 127 127 127 127))))

(deftest test-calculate-length
  (testing "basic usage"
    (let [to-bytes (fn [& bytes] (byte-array (map byte bytes)))]
      (is (= (t/calculate-length (to-bytes 0 0 0 0)) 0))
      (is (= (t/calculate-length (to-bytes 0 0 0 1)) 1))
      (is (= (t/calculate-length (to-bytes 0 0 1 0)) 128))
      (is (= (t/calculate-length (to-bytes 0 1 0 0)) 16384))
      (is (= (t/calculate-length (to-bytes 1 0 0 0)) 2097152))
      (is (= (t/calculate-length (to-bytes 1 1 1 1)) 2113665))
      (is (= (t/calculate-length (to-bytes 127 127 127 127)) 268435455))))
  (testing "circular"
    (let [test-circular (fn [len] (is (= (t/calculate-length (t/length-to-bytes len)) len)))]
      (test-circular 0)
      (test-circular 1)
      (test-circular 128)
      (test-circular 16384)
      (test-circular 2097152)
      (test-circular 2113665)
      (test-circular 268435455))))

