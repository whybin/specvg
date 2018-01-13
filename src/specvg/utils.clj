(ns specvg.utils
  (:gen-class))

; Random Bias {{{
(defn- pick-item
  "Pick an item based on the given float by checking
  if the float is less than the bias
  Params:
    coll : keyword list
    bias : float list
    flt : float
  Return: keyword"
  [coll bias flt]
  (if (< flt (first bias))
    (first coll)
    (recur (rest coll) (rest bias) (- flt (first bias)))))

(defn- normalize-bias
  "Translate each number by normalizing to the range [0, 1]
  Params:
    bias : number list
  Return: float list"
  [bias]
  (let [sum (float (reduce + bias))]  ; Ensure division returns float
    (map (fn [value] (/ value sum))
         bias)))

(defn rand-bias
  "Pseudorandomly picks a keyword from `coll` according to
  the associated bias
  Params:
    coll : keyword,number map
  Return: keyword"
  [coll]
  (pick-item (keys coll) (normalize-bias (vals coll)) (rand)))
; }}}

(defn slope
  "Params:
    [o p] : [number number]
    [x y] : [number number]
  Returns: ratio"
  [[o p] [x y]]
  (/ (- y p) (- x o)))

(defn point-sum
  [[o p] [x y]]
  [(+ o x) (+ p y)])

(defn point-on-line
  "Find endpoint (represented as 2-element vector) given two points.
  Params:
    [o p] : [number number]
    [x y] : [number number]
    dist : number, distance
  Returns: float vector"
  [[o p] [x y] dist]
  (let [[v w] [(- x o) (- y p)]
        k (/ dist (Math/sqrt (+ (* v v) (* w w))))]
    [(float (+ x (* v k)))
     (float (+ y (* w k)))]))

(defn polar-coordinate
  "Converts cartesian to polar.
  Params:
    [o p] : [number number]
    [x y] : [number number]
  Return: float"
  [[o p] [x y]]
  (Math/atan2 (- y p) (- x o)))

(defn cartesian
  "Converts polar to cartesian.
  Params:
    rad : number
  Return: float vector"
  [rad]
  [(Math/cos rad) (Math/sin rad)])
