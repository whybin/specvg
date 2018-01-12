(ns specvg.core
  (:gen-class)
  (:require [dali.io :as io]
            [specvg
             [utils :as utils]]))

(def unit 10)
(def choices {:terminate 4 :branch 3 :extend 5})

(defn- make-choice
  "Params:
    can-terminate : bool
  Return: keyword, describing the action to take.
    Only returns :terminate if `can-terminate` is true"
  [can-terminate]
  (utils/rand-bias (if can-terminate choices
                     (dissoc choices :terminate))))

(defn grow-branch
  "Generates a series of branched paths
  Params:
    path : vector
    continue-for : number, depth to disallow termination for
    branches : path vector, accumulation of paths
  Return: vector, of paths."
  [path continue-for branches]
  (let [can-terminate (= continue-for 0)
        continue-for (if (> continue-for 0) (- continue-for 1) 0)]
    (case (make-choice can-terminate)
      :terminate (conj branches path)
      :branch (recur (conj path :l [unit unit])
                     continue-for
                     (grow-branch (conj path :l [(* unit -1) unit])
                                  continue-for branches))
      :extend (recur (conj path :l [0 unit]) continue-for branches))))

(defn create-tree
  "Generates a branched tree SVG
  Params:
    attrs : map, SVG attributes
  Return: vector, a Dali document"
  [attrs]
  (into [:dali/page] (grow-branch [:path attrs :M [200 0]] 15 [])))

(defn -main
  [& args]
  (io/render-png (create-tree {:stroke :indigo
                               :stroke-width 4
                               :fill :none}) "out/test.png")
  (println "done!"))
