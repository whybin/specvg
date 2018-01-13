(ns specvg.utils-test
  (:require [clojure.test :refer :all]
            [specvg.utils :refer :all]
            [same :refer [ish?]]))

; Random Bias {{{
(deftest pick-item-test
  (testing "Will always pick a bias of 1"
    (is (=
         (#'specvg.utils/pick-item (list :will-be-picked) (list 1) 0.5)
         :will-be-picked))
    (is (=
         (#'specvg.utils/pick-item (list :not :will-be-picked) (list 0 1) 0.5)
         :will-be-picked)))
  (testing "Will never pick a bias of 0"
    (is (not (= (#'specvg.utils/pick-item (list :never-picked :a :b)
                                         (list 0 0.5 0.5) 0.3)
                :never-picked)))
    (is (not (= (#'specvg.utils/pick-item (list :never-picked :a :b)
                                         (list 0 0.5 0.5) 0.7)
                :never-picked))))
  (testing "Correctly picks based on float"
    (is (#'specvg.utils/pick-item (list :z :a :b) (list 0.2 0.3 0.5) 0.1) :z)
    (is (#'specvg.utils/pick-item (list :z :a :b) (list 0.2 0.3 0.5) 0.2) :a)
    (is (#'specvg.utils/pick-item (list :z :a :b) (list 0.2 0.3 0.5) 0.3) :b)))

(deftest normalize-bias-test
  (testing "Translates list to values out of 1"
    (is (= (#'specvg.utils/normalize-bias (list 1)) (list 1.0)))
    (is (= (#'specvg.utils/normalize-bias (list 42)) (list 1.0)))
    (is (= (#'specvg.utils/normalize-bias (list 1 2 3 4))
           (list 0.1 0.2 0.3 0.4)))))
; }}}

(deftest slope-test
  (testing "Correctly computes slope"
    (is (= (slope [0 0] [1 1]) 1))
    (is (= (slope [2 1] [-1 5]) -4/3))))

(deftest point-on-line-test
  (testing "Correctly finds endpoint"
    (is (= (point-on-line [0 0] [1 0] 10)
           [11.0 0.0]))
    (is (= (point-on-line [0 0] [0 1] 10)
           [0.0 11.0]))
    (is (= (point-on-line [0 0] [3 4] 10)
           [9.0 12.0]))))

(deftest polar-coordinate-test
  (testing "Correctly computes polar coords"
    (is (= (polar-coordinate [0 0] [1 0])
           0.0))
    (is (= (polar-coordinate [0 0] [0 1])
           (/ Math/PI 2)))
    (is (= (polar-coordinate [0 0] [0 -1])
           (/ Math/PI -2)))))

(deftest cartesian-test
  (testing "Correctly computes cartesian coords"
    (is (ish? (cartesian (* 2 Math/PI)) [1.0 0.0]))
    (is (ish? (cartesian Math/PI) [-1.0 0.0]))
    (is (ish? (cartesian (/ Math/PI 2)) [0.0 1.0]))
    (is (ish? (cartesian (/ Math/PI -2)) [0.0 -1.0]))))
