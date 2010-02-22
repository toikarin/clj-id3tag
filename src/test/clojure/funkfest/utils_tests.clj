(ns funkfest.utils-tests
  (:require [funkfest.utils :as u])
  (:use clojure.test))

(deftest test-take-min
  (is (= [1 2 3] (u/take-min 3 [1 2 3])))
  (is (= [1 2] (u/take-min 2 [1 2 3])))
  (is (= [1 2 3 nil nil] (u/take-min 5 [1 2 3]))))

(deftest test-split-at-pos
         (testing "basic cases"
                  (is (= [1 2 3] (u/split-at-pos [1 1 1] [1 2 3])))
                  (is (= [1 [1 2] [1 2 3]] (u/split-at-pos [1 2 3] [1 1 2 1 2 3])))
                  (is (= [1 [1 2] [1 2 3]] (u/split-at-pos [1 2 3] [1 1 2 1 2 3])))
                  (is (= [[1 2] [1 2 3]] (u/split-at-pos [2 :rest] [1 2 1 2 3]))))
         (testing "take more than coll contains"
                  (is (= [1 1 nil] (u/split-at-pos [1 1 1] [1 1])))
                  (is (= [1 [1 2] [1 2 nil]] (u/split-at-pos [1 2 3] [1 1 2 1 2]))))
         (testing "nil coll"
                  (is (= [nil] (u/split-at-pos [1] nil)))
                  (is (= [nil [nil nil] [nil nil nil]] (u/split-at-pos [1 2 3] nil))))
         (testing "pos = 0"
                  (is (= [nil] (u/split-at-pos [0] [1])))
                  (is (= [nil nil nil] (u/split-at-pos [0 0 0] [1 1 1])))
                  (is (= [nil 1 nil] (u/split-at-pos [0 1 0] [1 2 3]))))
         (testing "pos = neg"
                  (is (= [nil] (u/split-at-pos [-1] [1]))))
         (testing "args after :rest"
                  (is (= [[1 2] [1 2 3] nil] (u/split-at-pos [2 :rest 1] [1 2 1 2 3])))
                  (is (= [[1 2] [1 2 3] nil nil] (u/split-at-pos [2 :rest :rest 1] [1 2 1 2 3])))))
