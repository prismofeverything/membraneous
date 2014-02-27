(ns membraneous.core
  (:require 
   [clojure.string :as string]
   [clojure.set :as set]
   [cljs.core.async :refer [chan <! >! put!]]
   [cljs.reader :as reader]
   [domina :as dom]
   [domina.css :as css]
   [domina.events :as events]
   [singult.core :as sing]
   [membraneous.connect :as connect])
  (:require-macros 
   [cljs.core.async.macros :refer [go]]))

(def send (chan))
(def receive (chan))

(def ws-url "ws://localhost:19991/async")
(def ws (new js/WebSocket ws-url))

(def skeletons (atom {}))

(defn object
  [m]
  (let [out (js-obj)]
    (doall (map #(aset out (name (first %)) (second %)) m))
    out))

(defn log
  [e]
  (.log js/console e))

(def world (atom {}))

(def point-position (js/THREE.Vector3. -2500 2500 1500))

(defn random-color
  []
  (let [base (str (.toString (js/Math.random) 16) "000000")]
    (str "#" (.slice base 2 8))))

(defn window-size
  []
  [(.-innerWidth js/window) (.-innerHeight js/window)])

(defn set-sphere-at
  [sphere [x y z]]
  (.set (.-position sphere) (* 100 x) (* 100 y) (* 100 z)))

(defn make-sphere
  [at color radius]
  (let [material (js/THREE.MeshPhongMaterial. (object {:color (or color 0xff1493)}))
        geometry (js/THREE.SphereGeometry. radius 16 16)
        sphere (js/THREE.Mesh. geometry material)]
    (set-sphere-at sphere at)
    sphere))

(defn make-skeleton-molecule
  [color joints]
  (reduce
   (fn [[obj molecule] [joint at]] 
     (let [sphere (make-sphere at color 5)
           molecule (assoc molecule joint sphere)]
       (.add obj sphere)
       [obj molecule]))
   [(js/THREE.Object3D.) {}] joints))

(defn update-skeleton-molecule
  [molecule joints]
  (doseq [[joint at] joints]
    (let [sphere (get molecule joint)]
      (set-sphere-at sphere at)))
  molecule)

(defn receive-skeletons
  [data]
  (let [recent-skeletons (:skeletons data)
        missing-skeletons (set/difference (set (keys @skeletons)) (set (keys recent-skeletons)))]
    (doseq [gone missing-skeletons]
      (let [missing (get @skeletons gone)]
        (.remove (:scene @world) (:obj missing))
        (swap! skeletons dissoc gone)))
    (doseq [[id joints] recent-skeletons]
      (if-let [skeleton (get @skeletons id)]
        (do
          (swap! skeletons update-in [id :molecule] update-skeleton-molecule joints)
          (swap! skeletons update-in [id :joints] (constantly joints)))
        (let [color (random-color)
              [obj molecule] (make-skeleton-molecule color joints)]
          (.add (:scene @world) obj)
          (swap! skeletons assoc id {:joints joints :molecule molecule :obj obj}))))))

(defn aspect-ratio
  []
  (let [[width height] (window-size)]
    (/ width height)))

(defn init-camera
  [[x y z] look-at]
  (let [camera (js/THREE.PerspectiveCamera. 45 (aspect-ratio) 1 10000)]
    (.set (.-position camera) x y z)
    (.lookAt camera look-at)
    camera))

(defn init-renderer
  []
  (let [renderer (js/THREE.WebGLRenderer.)
        [width height] (window-size)]
    (.setSize renderer width height)
    (set! (.-shadowMapEnabled renderer) true)
    (set! (.-shadowMapType renderer) js/THREE.PCFShadowMap)
    renderer))

(defn init-viewport
  [renderer]
  (let [viewport (.getElementById js/document "viewport")]
    (.appendChild viewport (.-domElement renderer))
    viewport))

(defn init-lights
  []
  (let [ambient (js/THREE.AmbientLight. 0x0011111)
        point (js/THREE.SpotLight. 0xffffff 1 0 js/Math.PI 1)]
    (.copy (.-position point) point-position)
    (.set (.-position (.-target point)) 0 0 0)
    {:ambient ambient
     :point point}))

(defn on-key-down [event])
(defn on-key-up [event])
(defn on-mouse-down [event])
(defn on-mouse-move [event])

(defn on-resize
  []
  (let [[width height] (window-size)
        {:keys [camera renderer]} @world]
    (set! (.-aspect camera) (/ width height))
    (.updateProjectionMatrix camera)
    (.setSize renderer width height)))

(defn init-listeners
  []
  (.addEventListener js/document "keydown" on-key-down false)
  (.addEventListener js/document "keyup" on-key-up false)
  (.addEventListener js/document "mousedown" on-mouse-down false)
  (.addEventListener js/document "mousemove" on-mouse-move false)
  (.addEventListener js/window "resize" on-resize false))

(defn init-scene
  []
  (let [scene (js/THREE.Scene.)
        camera (init-camera [0 0 -200] (.-position scene))
        renderer (init-renderer)
        viewport (init-viewport renderer)
        controls (js/THREE.OrbitControls. camera)
        lights (init-lights)]
    (init-listeners)
    (doseq [[key light] (seq lights)]
      (.add scene light))
    {:clock (js/THREE.Clock.)
     :mouse (js/THREE.Vector2. 0 0)
     :scene scene
     :camera camera
     :renderer renderer
     :viewport viewport
     :controls controls
     :lights lights
     :time (.now js/Date)}))

(defn update
  [state]
  (let [{:keys [lights time]} state]
    (.set
     (.-position (:point lights))
     (* (.-x point-position) (js/Math.cos (* time 0.2)))
     (* (.-y point-position) (js/Math.sin (* time 0.3)))
     (* (.-z point-position) (js/Math.sin (* time 0.5))))
    state))

(defn render
  [state]
  (let [state (update state)
        {:keys [time controls renderer scene camera]} state]
    (.update controls)
    (.render renderer scene camera)
    state))

(defn update-time
  [state]
  (let [delta (.getDelta (:clock state))]
    (update-in state [:time] #(+ delta %))))

(defn animate
  []
  (.requestAnimationFrame js/window animate)
  (let [state (update-time @world)
        state (render state)]
    (swap! world merge state)))

(defn event-chan
  [c id el type data]
  (let [writer #(put! c [id % data])]
    (events/listen! el type writer)
    {:chan c
     :unsubscribe #(.removeEventListener el type writer)}))

(defn key-code
  [event]
  (.-keyCode (events/raw-event event)))

(defn init
  [data])

(defn dispatch-message
  []
  (go
   (while true
     (let [msg (<! receive)
           raw (.-data msg)
           data (reader/read-string raw)]
       (condp = (:op data)
         :init (init data)
         :skeletons (receive-skeletons data)
         (log (str "op not supported! " data)))))))

(defn make-sender
  []
  (log "HELLO")
  (event-chan send :click js/document.body :click {})
  (go
   (while true
     (let [[id event data] (<! send)]
       (condp = id
         :click (log "click!"))))))

(defn make-receiver []
  (set! 
   (.-onmessage ws)
   (fn [msg]
     (put! receive msg)))
  (set!
   (.-onopen ws)
   (fn [msg] 
     (.send ws {:op :init})))
  (dispatch-message))

(defn init-websockets!
  []
  (make-sender)
  (make-receiver))

(def on-load
  (set!
   (.-onload js/window)
   (fn []
     (init-websockets!)
     (let [state (init-scene)]
       (swap! world merge state)
       (animate)))))

(connect/connect)

