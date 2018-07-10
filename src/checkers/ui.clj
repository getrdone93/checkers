;As with all journeys, it begins with a simple willingness, with an abiding faith in the unknown.
(ns checkers.ui
  (:refer checkers.board)
  (:refer checkers.random-player))

(import 
 '(java.awt Color Graphics Dimension BorderLayout Shape)
 '(java.awt.image BufferedImage)
 '(java.awt.geom Rectangle2D$Double Ellipse2D$Double)
 '(javax.swing JPanel JFrame JTextArea)
 '(java.awt.event MouseAdapter MouseEvent))

(def dim 80)
(def circ-hl (+ circ-dim 9))
(def king-dim (/ circ-dim 2.4))
(def board (atom (gen-board 0 [])))
(def human-team :team2)
(defn get-board [] @board)
(defn hl-shift [cp] (- cp 4))
(defn king-shift [cp] (+ cp 14))

(defn draw-board [im-graph [{{sqc :color sqclick :clicked [sqx sqy] :point} :square
                             {[chkx chky] :point chkt :team chkc :clicked king :king :as checker} :checker} :as eles]]
           (when (not (empty? eles))  
               (.setColor im-graph (if sqclick green sqc))
               (.fillRect im-graph sqx sqy scale scale)
               (when (and (= sqc black) (some? checker) (some? chkt))
                 (when chkc
                   (.setColor im-graph green)
                   (.fillOval im-graph (hl-shift chkx) (hl-shift chky) circ-hl circ-hl))
                 (.setColor im-graph (second chkt))
                 (.fillOval im-graph chkx chky circ-dim circ-dim)
                 (when king
                   (.setColor im-graph cyan)
                   (.fillOval im-graph (king-shift chkx) (king-shift chky) king-dim king-dim)))
             (draw-board im-graph (rest eles))))

(defn color-frame [g read-board]
  (let [img (new BufferedImage (* scale dim) (* scale dim) (. BufferedImage TYPE_INT_ARGB))
        im-graph (. img (getGraphics))]
       (.setColor im-graph (. Color white))
       (.fillRect im-graph 0 0 (. img (getWidth)) (. img (getHeight)))
       (draw-board im-graph read-board)
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


(defn hl-checker [read-board] 
  (first (filter some? (map-indexed (fn [index {ele :checker
                                                {clicked :clicked} :checker :as entry}] 
                                      (when (and (some? ele) clicked)
                                        [index entry])) read-board))))

(defn find-clicked [index ele key shapeKey mouse-event]
  (when (and (some? (ele key)) (some? ((ele key) shapeKey)) 
              (. ((ele key) shapeKey) (contains 
                                        (. mouse-event (getX)) 
                                        (. mouse-event (getY)))))
                                              [index ele]))
(defn find-clicked-chk [index ele key shapeKey mouse-event]
  (let [[index {{[team _] :team} :checker} :as res] (find-clicked index ele key shapeKey mouse-event)]
    (when (= team human-team)
      res)))

(def ml (proxy [MouseAdapter] []
          (mouseClicked [mouse-event] 
            (let [read-board (get-board)
                  hl-c (hl-checker read-board)
                  clicked-square (first (filter #(and (some? %) (= (((second %) :square) :color) (. Color black)))
                                                           (map-indexed #(find-clicked %1 %2 :square :square-obj mouse-event) read-board)))
                  clicked-checker (first (filter some? (map-indexed #(find-clicked %1 %2 :checker :checker-obj mouse-event) read-board)))
                  update-clicked (fn [ele val] (when (some? ele)
                                                 (reset! board (assoc @board (first ele) 
                                                                      (assoc (second ele) :checker 
                                                                             (assoc ((second ele) :checker) :clicked val))))))
                  {move? :valid-move :as move-data} (valid-move? {:from hl-c
                                                                      :to clicked-square} read-board)]
             (if move?
               (do 
                 (let [ub (exec-move-checker move-data board {:from hl-c :to clicked-square} read-board)]
                   (. panel (paintImmediately 0 0 (. panel (getWidth)) (. panel (getHeight))))
;                   (Thread/sleep think-time-ms)
;	                 (let [rand-move (rand-chk-move ub)]
;	                   (exec-move-checker (valid-move? rand-move ub) board rand-move ub))
;                   (. panel (paintImmediately 0 0 (. panel (getWidth)) (. panel (getHeight))))
                   ))
               (do
                   (update-clicked hl-c false)
                   (update-clicked clicked-checker true)
                   (. panel (repaint))))))))

(defn frame [] (doto 
                 (new JFrame) 
                 (-> (.getContentPane) (.add panel) (.addMouseListener ml))
                 .pack 
                 .show))