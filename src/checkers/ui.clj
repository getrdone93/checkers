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
(def circ-hl (+ circ-dim 5))
(def shift (/ circ-dim 2))
(def t1-rows (set (range 0 3)))
(def t2-rows (set (range 5 8)))
(def checker-rows (clojure.set/union t1-rows t2-rows))

(def img (new BufferedImage (* scale dim) (* scale dim) 
                 (. BufferedImage TYPE_INT_ARGB)))

(defn gen-board [n br]
                (let [place (mod n 8)
                      r (quot n 8)
                      x (if (zero? place)
                          0
                        (* place scale))
                      y (* r scale)
                      c (if (zero? (mod r 2))
                          (if (zero? (mod n 2))
                            (. Color red)
                            (. Color black))
                          (if (zero? (mod n 2))
                            (. Color black)
                            (. Color red)))
                      cx (+ x shift)
                      cy (+ y shift)]
                  (if (< n num-squares)
                    (gen-board (inc n) (conj br {:square {:point [x y]
                                                           :color c
                                                           :valid-click-locs []
                                                           :rectangle (new Rectangle2D$Double x y scale scale)}
                                                  :checker {:point [cx cy]
                                                            :color (cond 
                                                                     (contains? t1-rows r) t1-color
                                                                     (contains? t2-rows r) t2-color
                                                                     :else nil)
                                                            :valid-click-locs []
                                                            :circle (when (and (contains? checker-rows r) (= c (. Color black)))
                                                                      (new Ellipse2D$Double cx cy circ-dim circ-dim))
                                                            :clicked false}}))
                    br)))

(def board (atom (gen-board 0 [])))

(defn color-frame [g]
  (let [img (new BufferedImage (* scale dim) (* scale dim) 
                 (. BufferedImage TYPE_INT_ARGB))
        im-graph (. img (getGraphics))]
       (.setColor im-graph (. Color white))
       (.fillRect im-graph 0 0 (. img (getWidth)) (. img (getHeight)))
       ((fn draw-board [b]
          (when (not (empty? b))  
            (let [square ((first b) :square)
               checker ((first b) :checker)
               cx (first (checker :point))
               cy (second (checker :point))
               sqx (first (square :point))
               sqy (second (square :point))]
              (.setColor im-graph (square :color))
              (.fillRect im-graph sqx sqy scale scale)
              (when (and (= (square :color) (. Color black)) (some? (checker :color)))
                (when (checker :clicked)
                  (.setColor im-graph (. Color green))
                  (.fillOval im-graph cx cy circ-hl circ-hl))
                (.setColor im-graph (checker :color))
                (.fillOval im-graph cx cy circ-dim circ-dim)))
            (draw-board (rest b)))) @board)
       (. g (drawImage img 0 0 nil))
       (. im-graph (dispose))))

;proxy implements/extends a interface/class where the supplied arguments
;are arguments to the class' super constructor and then calls
;the supplied functions
(def panel (doto (proxy [JPanel] []
                        (paint [g] (color-frame g)))
             (.setPreferredSize (new Dimension 
                                     (/ (* scale dim) 5) 
                                     (/ (* scale dim) 5)))
             (.addMouseListener ml)))

(def ml (proxy [MouseAdapter] []
          (mouseClicked [mouse-event] 
            (let [mex (. mouse-event (getX))
                  mey (. mouse-event (getY))
                  find-obj (fn [x key] (when (some? (x key))
                                         (let [shape (cast Shape (x key))]
                                           (when (. shape (contains mex mey))
                                             x))))
                  square (first (filter some? (map #(find-obj %1 :rectangle) (map :square @board)))) 
                  checker (first (filter some? (map #(find-obj %1 :circle) (map :checker @board))))]
              (do 
                (when (some? checker)
                 ;(reset! board (assoc @board :checker (assoc checker :clicked true)))
                  ;(. panel (repaint))
                  
                  ))))))

(defn frame [] (doto 
                 (new JFrame) 
                 (-> (.getContentPane) (.add panel))
                 .pack 
                 .show))