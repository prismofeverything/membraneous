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
            (let [mouse (scene/interpret-mouse-position event)
                  object (scene/pick-object mouse @scene/world)]
              (scene/set-material-color object 0xaaaaaa)
              (set! (.-z (.-position object)) (+ (.-z (.-position object)) 10))
              (events/log (str "click! " (scene/interpret-mouse-position event)))
              (events/log object)))})

(def point-position (js/THREE.Vector3. -2500 2500 1500))

(def baseline 12)

(defn init-scene
  [state]
  (let [scene (:scene state)
        camera (scene/camera [0 0 -2] (.-position scene))
        controls (js/THREE.OrbitControls. camera)
        ambient (scene/ambient-light 0x001111)
        point (scene/point-light {:color 0xffffff :position point-position})
        field (geometry/make-sphere-field 20 20 [-10 -10] [10 10] 0.6 baseline)]
    (doseq [{:keys [sphere]} (vals field)]
      (.add scene sphere))
    (.add scene ambient)
    (.add scene point)
    (assoc state
      :camera camera
      :controls controls
      :field field
      :lights {:ambient ambient :point point})))

(defn update-scene
  [state]
  (let [{:keys [lights time controls]} state
        field (geometry/transient-force-cycle (:field state) baseline)]
    (.update controls)
    (.set
     (.-position (:point lights))
     (* (.-x point-position) (js/Math.cos (* time 0.2)))
     (* (.-y point-position) (js/Math.sin (* time 0.3)))
     (* (.-z point-position) (js/Math.sin (* time 0.5))))
    (assoc state :field field)))

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

