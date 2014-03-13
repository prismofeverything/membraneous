(ns membraneous.scene)

(def world (atom {}))

(defn window-size
  []
  [(.-innerWidth js/window) (.-innerHeight js/window)])

(defn aspect-ratio
  []
  (let [[width height] (window-size)]
    (/ width height)))

(defn camera
  [[x y z] look-at]
  (let [camera (js/THREE.PerspectiveCamera. 45 (aspect-ratio) 1 10000)]
    (.set (.-position camera) x y z)
    (.lookAt camera look-at)
    camera))

(defn ambient-light
  [color]
  (js/THREE.AmbientLight. color))

(defn point-light
  [{:keys [color position target]}]
  (let [point (js/THREE.SpotLight. color 1 0 js/Math.PI 1)
        target (or target (js/THREE.Vector3. 0 0 0))]
    (.copy (.-position point) position)
    (.copy (.-position (.-target point)) target)
    point))

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

(defn init-scene
  [init]
  (let [scene (js/THREE.Scene.)
        renderer (init-renderer)
        viewport (init-viewport renderer)
        state {:clock (js/THREE.Clock.)
               :mouse (js/THREE.Vector2. 0 0)
               :scene scene
               :renderer renderer
               :viewport viewport
               :time (.now js/Date)}]
    (init state)))

(defn render
  [state]
  (let [{:keys [time controls renderer scene camera]} state]
    (.render renderer scene camera)
    state))

(defn update-time
  [state]
  (let [delta (.getDelta (:clock state))]
    (update-in state [:time] #(+ delta %))))

(defn start
  [init update]
  (let [state (init-scene init)
        animate (fn animate []
                  (.requestAnimationFrame js/window animate)
                  (let [state (update-time @world)
                        state (update state)
                        state (render state)]
                    (swap! world merge state)))]
    (swap! world merge state)
    (animate)))
