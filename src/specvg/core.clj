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

(defn grow-tree
  [origin path center continue-for]
  (letfn [(grow-branch
            ; Generates a series of branched paths
            ; Params:
            ;   path : vector
            ;   continue-for : number, depth to disallow termination for
            ;   branches : path vector, accumulation of paths
            ; Return: vector, of paths.
            [[x y] path continue-for branches]
            (let [can-terminate (= continue-for 0)
                  continue-for (if (> continue-for 0) (- continue-for 1) 0)
                  path (conj path :L [x y])]
              (case (make-choice can-terminate)
                :terminate (conj branches path)
                :branch (recur [(+ x unit) (+ y unit)] path continue-for
                               (grow-branch [(- x unit) (+ y unit)]
                                            path continue-for branches))
                :extend (recur [x (+ y unit)] path continue-for branches))))]
    (grow-branch origin path continue-for [])))

(defn create-tree
  "Generates a branched tree SVG
  Params:
    attrs : map, SVG attributes
  Return: vector, a Dali document"
  [attrs center-radius]
  (let [center [200 0]
        origin [200 center-radius]]
    (into [:dali/page]
          (grow-tree origin [:path attrs :M origin] center 15))))

(defn -main
  [& args]
  (io/render-png (create-tree {:stroke :indigo
                               :stroke-width 4
                               :fill :none} 12) "out/test.png")
  (println "done!"))
