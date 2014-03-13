(ns membraneous.geometry
  (:require [clojure.set :as set]))

(def sqrt-three-over-two (/ (Math/sqrt 3.0) 2))

(defn object
  [m]
  (let [out (js-obj)]
    (doall (map #(aset out (name (first %)) (second %)) m))
    out))

(defn random-color
  []
  (let [base (str (.toString (js/Math.random) 16) "000000")]
    (str "#" (.slice base 2 8))))

(defn set-sphere-at
  [sphere [x y z]]
  (.set (.-position sphere) (* 100 x) (* 100 y) (* 100 z)))

(defn make-sphere
  ([at color radius] (make-sphere at color radius (js/THREE.SphereGeometry. radius 16 16)))
  ([at color radius geometry]
     (let [material (js/THREE.MeshPhongMaterial. (object {:color (or color 0xff1493)}))
           sphere (js/THREE.Mesh. geometry material)]
       (set-sphere-at sphere at)
       sphere)))

(defn make-sphere-field
  [rows cols [start-x start-y] [end-x end-y] radius z]
  (let [width (- end-x start-x)
        height (- end-y start-y)
        single-width (/ width cols)
        geometry (js/THREE.SphereGeometry. radius 16 16)]
    (mapcat
     (fn [row]
       (map 
        (fn [col]
          (let [y-portion (float (/ row rows))
                x-portion (float (/ col cols))
                at [(+ (* x-portion width) start-x (if (even? row) (* 0.5 single-width) 0))
                    (+ (* y-portion width sqrt-three-over-two) start-y)
                    z]
                sphere (make-sphere at 0xaa1133 radius geometry)]
            sphere))
        (range cols)))
     (range rows))))
