(ns membraneous.core
  (:require [membraneous.geometry :as geometry]
            [membraneous.skeleton :as skeleton]
            [membraneous.scene :as scene]
            [membraneous.events :as events]
            [membraneous.connect :as connect]))

(defn on-key-down [event])
(defn on-key-up [event])
(defn on-mouse-down [event])
(defn on-mouse-move [event])

(defn on-resize
  []
  (let [[width height] (scene/window-size)
        {:keys [camera renderer]} @scene/world]
    (set! (.-aspect camera) (/ width height))
    (.updateProjectionMatrix camera)
    (.setSize renderer width height)))

(defn init-connection
  [data])

(def websocket-handlers
  {:init init-connection
   :skeletons #(skeleton/receive-skeletons % (:scene @scene/world))})

(def event-handlers
  {:click (fn [event data] 
            (events/log (str "click!"))
            (events/log event))})

(def point-position (js/THREE.Vector3. -2500 2500 1500))

(defn init-scene
  [state]
  (let [scene (:scene state)
        camera (scene/camera [0 0 -200] (.-position scene))
        controls (js/THREE.OrbitControls. camera)
        ambient (scene/ambient-light 0x001111)
        point (scene/point-light {:color 0xffffff :position point-position})
        field (geometry/make-sphere-field 50 50 [-10 -10] [10 10] 25 5)]
    (doseq [sph field]
      (.add scene sph))
    (.add scene ambient)
    (.add scene point)
    (assoc state
      :camera camera
      :controls controls
      :lights {:ambient ambient :point point})))

(defn update-scene
  [state]
  (let [{:keys [lights time controls]} state]
    (.update controls)
    (.set
     (.-position (:point lights))
     (* (.-x point-position) (js/Math.cos (* time 0.2)))
     (* (.-y point-position) (js/Math.sin (* time 0.3)))
     (* (.-z point-position) (js/Math.sin (* time 0.5))))
    state))

(def on-load
  (set!
   (.-onload js/window)
   (fn []
     (events/init-websockets event-handlers websocket-handlers)
     (events/init-listeners
      {:key-down on-key-down
       :key-up on-key-up
       :mouse-down on-mouse-down
       :mouse-move on-mouse-move
       :resize on-resize})
     (scene/start init-scene update-scene))))

(connect/connect)

