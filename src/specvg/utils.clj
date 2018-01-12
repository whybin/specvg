(ns specvg.utils
  (:gen-class))

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
