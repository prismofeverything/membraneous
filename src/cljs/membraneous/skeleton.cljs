(ns membraneous.skeleton
  (:require [clojure.set :as set]
            [membraneous.geometry :as geometry]))

(def skeletons (atom {}))

(defn make-skeleton-molecule
  [color joints]
  (reduce
   (fn [[obj molecule] [joint at]] 
     (let [sphere (geometry/make-sphere at color 5)
           molecule (assoc molecule joint sphere)]
       (.add obj sphere)
       [obj molecule]))
   [(js/THREE.Object3D.) {}] joints))

(defn update-skeleton-molecule
  [molecule joints]
  (doseq [[joint at] joints]
    (let [sphere (get molecule joint)]
      (geometry/set-sphere-at sphere at)))
  molecule)

(defn receive-skeletons
  [data scene]
  (let [recent-skeletons (:skeletons data)
        missing-skeletons (set/difference (set (keys @skeletons)) (set (keys recent-skeletons)))]
    (doseq [gone missing-skeletons]
      (let [missing (get @skeletons gone)]
        (.remove scene (:obj missing))
        (swap! skeletons dissoc gone)))
    (doseq [[id joints] recent-skeletons]
      (if-let [skeleton (get @skeletons id)]
        (do
          (swap! skeletons update-in [id :molecule] update-skeleton-molecule joints)
          (swap! skeletons update-in [id :joints] (constantly joints)))
        (let [color (geometry/random-color)
              [obj molecule] (make-skeleton-molecule color joints)]
          (.add scene obj)
          (swap! skeletons assoc id {:joints joints :molecule molecule :obj obj}))))))
