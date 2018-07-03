;As with all journeys, it begins with a simple willingness, with an abiding faith in the unknown.
(ns checkers.ui
  (:refer checkers.board))

(import 
 '(java.awt Color Graphics Dimension BorderLayout Shape)
 '(java.awt.image BufferedImage)
 '(java.awt.geom Rectangle2D$Double Ellipse2D$Double)
 '(javax.swing JPanel JFrame JTextArea)
 '(java.awt.event MouseAdapter MouseEvent))

;for using doc function!
(use 'clojure.repl) 
;(use 'clojure.set)

(def dim 80)
;(def team-color {:team1 (. Color yellow)
;                 :team2 (. Color magenta)})

(def circ-hl (+ circ-dim 9))
(def king-dim (/ circ-dim 2.4))

(defn hl-shift [cp] (- cp 4))
(defn king-shift [cp] (+ cp 14))

(defn color-frame [g read-board]
  (let [img (new BufferedImage (* scale dim) (* scale dim) 
         (. BufferedImage TYPE_INT_ARGB))
        im-graph (. img (getGraphics))]
       (.setColor im-graph (. Color white))
       (.fillRect im-graph 0 0 (. img (getWidth)) (. img (getHeight)))
       ((fn draw-board [[{square :square
                 {[sqx sqy] :point} :square
                 checker :checker} :as eles]]
          (when (not (empty? eles))  
              (.setColor im-graph (square :color))
              (.fillRect im-graph sqx sqy scale scale)
              (when (and (= (square :color) black) (some? checker) (some? (checker :team)))
                (let [cx (first (checker :point))
                      cy (second (checker :point))]
                  (when (checker :clicked)
                    (.setColor im-graph green)
                    (.fillOval im-graph (hl-shift cx) (hl-shift cy) circ-hl circ-hl))
                  (.setColor im-graph (second (checker :team)))
                  (.fillOval im-graph cx cy circ-dim circ-dim)
                  (when (checker :king)
                    (.setColor im-graph cyan)
                    (.fillOval im-graph (king-shift cx) (king-shift cy) king-dim king-dim))))
            (draw-board (rest eles)))) read-board)
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


(defn hl-checker [read-board] (first 
                                (filter some? 
                                        (map-indexed (fn [index {ele :checker
                                                                 {clicked :clicked} :checker :as entry}] 
                                                       (when (and (some? ele) clicked)
                                                         [index entry])) read-board))))

(def ml (proxy [MouseAdapter] []
          (mouseClicked [mouse-event] 
            (let [read-board (get-board)
                  find-clicked (fn [index ele key shapeKey]
                                 (when (and (some? (ele key)) (some? ((ele key) shapeKey)) 
                                             (. ((ele key) shapeKey) (contains 
                                                                       (. mouse-event (getX)) 
                                                                       (. mouse-event (getY)))))
                                       [index ele]))
                  hl-c (hl-checker read-board)
                  clicked-square (first (filter #(and (some? %) (= (((second %) :square) :color) (. Color black)))
                                                           (map-indexed #(find-clicked %1 %2 :square :square-obj) read-board)))
                  clicked-checker (first (filter some? (map-indexed #(find-clicked %1 %2 :checker :checker-obj) read-board)))
                  update-clicked (fn [ele val] (when (some? ele)
                                                 (reset! board (assoc @board (first ele) 
                                                                      (assoc (second ele) :checker 
                                                                             (assoc ((second ele) :checker) :clicked val))))))
                  {move? :valid-move :as move-data} (valid-move? {:from hl-c
                                                                      :to clicked-square} read-board)]
              (if move?
                (reset! board (let [{board :read-board 
                                     moved-entry :new-entry} (move-checker {:from hl-c :to clicked-square} 
                                                                           (remove-jumped-chks move-data read-board))]
                                (king-me moved-entry board)))
                (do
                    (update-clicked hl-c false)
                    (update-clicked clicked-checker true)))
                (. panel (repaint))))))

(defn frame [] (doto 
                 (new JFrame) 
                 (-> (.getContentPane) (.add panel) (.addMouseListener ml))
                 .pack 
                 .show))