(ns membraneous.core
  (:require [membraneous.geometry :as geometry]
            [membraneous.skeleton :as skeleton]
            [membraneous.scene :as scene]
            [membraneous.events :as events]
            [membraneous.connect :as connect]))

(defn on-key-down 
  [event]
  (swap! scene/world assoc-in [:keyboard (.-keyCode event)] true))

(defn on-key-up
  [event]
  (swap! scene/world update-in [:keyboard] dissoc (.-keyCode event)))

(defn on-mouse-down 
  [event]
  (swap! scene/world update-in [:mouse :down] (constantly true)))

(defn on-mouse-up
  [event]
  (swap! scene/world update-in [:mouse :down] (constantly false)))

(defn on-resize
  []
  (let [[width height] (scene/window-size)
        {:keys [camera renderer]} @scene/world]
    (set! (.-aspect camera) (/ width height))
    (.updateProjectionMatrix camera)
    (.setSize renderer width height)))

(defn normalize-n
  [n d]
  (- (* 2.0 (/ (float n) d)) 1.0))

(defn interpret-mouse-position
  [x y]
  (let [[w h] (scene/window-size)
        x (normalize-n x w)
        y (* -1 (normalize-n y h))]
    [x y]))

(defn on-mouse-move 
  [event]
  (let [[x y] [(.-clientX event) (.-clientY event)]
        mouse (interpret-mouse-position x y)
        object (scene/pick-object mouse @scene/world)]
    (if (and object (-> @scene/world :mouse :down))
      (do
        (scene/set-material-color object 0xaaaaaa)
        (set! (.-z (.-position object)) (* (.-z (.-position object)) 1.2))))
    (swap! scene/world update-in [:mouse :position] (constantly mouse))))

(defn init-connection
  [data])

(def websocket-handlers
  {:init init-connection
   :skeletons #(skeleton/receive-skeletons % (:scene @scene/world))})

(def event-handlers
  {:click (fn [event data])})

(def point-position (js/THREE.Vector3. -2500 2500 1500))

(def baseline 5)

(defn init-scene
  [state]
  (let [scene (:scene state)
        look (js/THREE.Vector3. 0 0 0)
        camera (scene/camera [0 -14 0] look)
        ;; controls (js/THREE.OrbitControls. camera)
        ambient (scene/ambient-light 0x001111)
        point (scene/point-light {:color 0xffffff :position point-position})
        field (geometry/make-sphere-field 20 20 [-10 -10] [10 10] 0.5 baseline)]
    (doseq [{:keys [sphere]} (vals field)]
      (.add scene sphere))
    (.add scene ambient)
    (.add scene point)
    (assoc state
      :camera camera
      ;; :controls controls
      :look look
      :up 1.2
      :field field
      :mouse {:down false :position [0 0]}
      :keyboard {}
      :lights {:ambient ambient :point point})))

(defn update-scene
  [state]
  (let [{:keys [lights camera time keyboard look controls up]} state
        field (geometry/transient-force-cycle (:field state) baseline)
        up (if (get keyboard 38) (+ up 0.01) up)
        up (if (get keyboard 40) (- up 0.01) up)
        look-y (* 50 (Math/sin up))
        look-z (- 50 (* 50 (Math/cos up)))]
    ;; (.update controls)
    (.set look 0 look-y look-z)
    (.lookAt camera look)
    (.set
     (.-position (:point lights))
     (* (.-x point-position) (js/Math.cos (* time 0.2)))
     (* (.-y point-position) (js/Math.sin (* time 0.3)))
     (* (.-z point-position) (js/Math.sin (* time 0.5))))
    (assoc state :field field :up up)))

(def on-load
  (set!
   (.-onload js/window)
   (fn []
     (events/init-websockets event-handlers websocket-handlers)
     (events/init-listeners
      {:key-down on-key-down
       :key-up on-key-up
       :mouse-down on-mouse-down
       :mouse-up on-mouse-up
       :mouse-move on-mouse-move
       :resize on-resize})
     (scene/start init-scene update-scene))))

(connect/connect)

