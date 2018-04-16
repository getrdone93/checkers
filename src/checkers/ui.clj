(ns ui.core)

(import 
 '(java.awt Color Graphics Dimension BorderLayout Shape)
 '(java.awt.image BufferedImage)
 '(java.awt.geom Rectangle2D$Double Ellipse2D$Double)
 '(javax.swing JPanel JFrame JTextArea)
 '(java.awt.event MouseAdapter MouseEvent))

(def scale 100)
(def dim 80)
(def num-squares 64)
(def t1-color (. Color yellow))
(def t2-color (. Color magenta))
(def circ-dim (/ scale 2))
(def circ-hl (+ circ-dim 9))
(def shift (/ circ-dim 2))
(def t1-rows (set (range 0 3)))
(def t2-rows (set (range 5 8)))
(def checker-rows (clojure.set/union t1-rows t2-rows))
(def black (. Color black))
(def red (. Color red))

(defn hl-shift [cp] (- cp 4))

(defn gen-board [n br]
                (let [place (mod n 8)
                      r (quot n 8)
                      x (if (zero? place)
                          0
                        (* place scale))
                      y (* r scale)
                      c (if (zero? (mod r 2))
                          (if (zero? (mod n 2))
                            red
                            black)
                          (if (zero? (mod n 2))
                            black
                            red))
                      cp (checker-point x y)]
                  (if (< n num-squares)
                    (gen-board (inc n) (conj br {:square {:point [x y]
                                                           :color c
                                                           :valid-click-locs []
                                                           :square-obj (new Rectangle2D$Double x y scale scale)}
                                                 ;think about not casting each time and instead storing casted version
                                                 ;as a key
                                                 :checker (when (and (contains? checker-rows r) (= c black))
                                                            {:point cp
                                                             :color (cond 
                                                                      (contains? t1-rows r) t1-color
                                                                      (contains? t2-rows r) t2-color
                                                                      :else nil)
                                                             :valid-click-locs []
                                                             :checker-obj (new Ellipse2D$Double (first cp) 
                                                                               (second cp) circ-dim circ-dim)
                                                             :clicked false})})) br)))
(defn checker-point [sqx sqy] [(+ sqx shift) (+ sqy shift)])

(def board (atom (gen-board 0 [])))

(defn color-frame [g read-board]
  (let [img (new BufferedImage (* scale dim) (* scale dim) 
         (. BufferedImage TYPE_INT_ARGB))
        im-graph (. img (getGraphics))]
       (.setColor im-graph (. Color white))
       (.fillRect im-graph 0 0 (. img (getWidth)) (. img (getHeight)))
       ((fn draw-board [b]
          (when (not (empty? b))  
            (let [square ((first b) :square)
               checker ((first b) :checker)
               sqx (first (square :point))
               sqy (second (square :point))]
              (.setColor im-graph (square :color))
              (.fillRect im-graph sqx sqy scale scale)
              (when (and (= (square :color) black) (some? checker) (some? (checker :color)))
                (let [cx (first (checker :point))
                      cy (second (checker :point))]
                  (when (checker :clicked)
                    (.setColor im-graph (. Color green))
                    (.fillOval im-graph (hl-shift cx) (hl-shift cy) circ-hl circ-hl))
                  (.setColor im-graph (checker :color))
                  (.fillOval im-graph cx cy circ-dim circ-dim))))
            (draw-board (rest b)))) read-board)
       (. g (drawImage img 0 0 nil))
       (. im-graph (dispose))))

;proxy implements/extends a interface/class where the supplied arguments
;are arguments to the class' super constructor and then calls
;the supplied functions
(def panel (doto (proxy [JPanel] []
                        (paint [g] (color-frame g @board)))
             (.setPreferredSize (new Dimension 
                                     (/ (* scale dim) 5) 
                                     (/ (* scale dim) 5)))))

(defn get-board [] @board)

(def ml (proxy [MouseAdapter] []
          (mouseClicked [mouse-event] 
            (let [read-board (get-board)
                  find-clicked (fn [index ele key shapeKey]
                                 (when (and (some? (ele key)) (some? ((ele key) shapeKey)) 
                                            (. (cast Shape ((ele key) shapeKey)) (contains 
                                                                                   (. mouse-event (getX)) 
                                                                                   (. mouse-event (getY)))))
                                       [index ele]))
                  square (first (filter #(and (some? %) (= (((second %) :square) :color) (. Color black)) )
                                                    (map-indexed #(find-clicked %1 %2 :square :square-obj) read-board)))
                  checker (first (filter some? (map-indexed #(find-clicked %1 %2 :checker :checker-obj) read-board)))
                  curr-clicked (first (filter some? (map-indexed (fn [index ele] 
                                                  (when (and (some? (ele :checker)) ((ele :checker) :clicked))
                                                    [index ele])) read-board)))
                  update-clicked (fn [ele val] (when (some? ele)
                                                 (reset! board (assoc @board (first ele) 
                                                                      (assoc (second ele) :checker 
                                                                             (assoc ((second ele) :checker) :clicked val))))))]
                (if (move-checker? curr-clicked square)
                  (reset! board (move-checker curr-clicked square read-board))
                  (do
                    (update-clicked curr-clicked false)
                    (update-clicked checker true)))
                (. panel (repaint))))))

(defn move-checker? [checker square] 
  (cond 
    (or (nil? checker) (nil? square)) false
    (some? ((second square) :checker)) false
    :else true))

(defn move-checker [[chk-ind checker] 
                    [sq-ind {{sq-point :point} :square :as square}] 
                    read-board]
  (let [cp (checker-point (first sq-point) (second sq-point))
        move-chk (assoc (checker :checker) 
                        :point cp
                        :clicked false
                        :checker-obj (new Ellipse2D$Double (first cp) (second cp) circ-dim circ-dim))
        new-sq (assoc square :checker move-chk)
        new-chk (assoc checker :checker nil)]
    (assoc (assoc read-board chk-ind new-chk) sq-ind new-sq)))

(defn frame [] (doto 
                 (new JFrame) 
                 (-> (.getContentPane) (.add panel) (.addMouseListener ml))
                 .pack 
                 .show))