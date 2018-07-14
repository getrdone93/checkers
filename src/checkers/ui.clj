;As with all journeys, it begins with a simple willingness, with an abiding faith in the unknown.
(ns checkers.ui
  (:refer checkers.board)
  (:refer checkers.random-player))

(import 
 '(java.awt Color Graphics Dimension BorderLayout Shape)
 '(java.awt.image BufferedImage)
 '(java.awt.geom Rectangle2D$Double Ellipse2D$Double)
 '(javax.swing JPanel JFrame JTextArea JButton AbstractButton)
 '(java.awt.event MouseAdapter MouseEvent ActionListener))

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

(defn button-click [action-event]
  (println "clicked!"))

(def submit-button
  (let [sb (new JButton "SUBMIT MOVE")
        _ (. sb (setVerticalTextPosition (AbstractButton/CENTER)))
        _ (. sb (setHorizontalTextPosition (AbstractButton/LEADING)))
        _ (. sb (addActionListener (proxy [ActionListener] []
                                     (actionPerformed [ae] (button-click ae)))))
        {{[_ y] :point} :square} (last (get-board))
        _ (. sb (setBounds 0 (+ y 100) (+ y 100) 60))]
    sb))

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

(defn hl-element [ele-key read-board]
  (set (filter some? (map-indexed 
                       (fn [index {ele ele-key {clicked :clicked} ele-key :as entry}] 
                         (when (and (some? ele) clicked)
                           [index entry])) read-board))))

(defn hl-checker [read-board] 
  (first (hl-element :checker read-board)))

(defn hl-squares [read-board]
  (hl-element :square read-board))

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

(defn clicked-square [mouse-event find-func read-board]
  (first (filter #(and (some? %) 
                       (= (((second %) :square) :color) (. Color black)))
                          (map-indexed #(find-func %1 %2 :square :square-obj mouse-event) read-board))))

(defn clicked-checker [mouse-event find-func read-board]
  (first (filter some? 
                 (map-indexed #(find-func %1 %2 :checker :checker-obj mouse-event) read-board))))

(defn valid-square? [checker square read-board]
  (let [{sp :simple-paths ajp :all-jump-paths} (paths checker read-board)]
    (or (some? (first (filter #(= square %) sp))) 
        (some? (first (set (map (fn [{p :path}]
                                (first (filter #(= square %) p))) ajp)))))))

(defn update-clicked [ele uk val]
  (when (some? ele)
    (reset! board (assoc @board (first ele) 
                         (assoc (second ele) uk 
                                (assoc ((second ele) uk) :clicked val))))))

(defn flip-clicked [[ind {{val :clicked} uk :as entry}] uk]
  (when (some? entry) 
      (reset! board (assoc @board ind 
                           (assoc entry uk 
                                  (assoc (entry uk) :clicked (not val)))))))

(def ml (proxy [MouseAdapter] []
          (mouseClicked [mouse-event] 
            (let [read-board (get-board)
                  hl-c (hl-checker read-board)
                  clicked-square (clicked-square mouse-event find-clicked read-board)
                  clicked-checker (clicked-checker mouse-event find-clicked read-board)]
             (do 
               (cond 
                 (and (nil? hl-c) (some? clicked-checker)) (flip-clicked clicked-checker :checker)
                 (and (some? hl-c) (some? clicked-checker)) (do
                                                              (flip-clicked hl-c :checker)
                                                              (flip-clicked clicked-checker :checker)
                                                              (map #(flip-clicked % :square) (hl-squares read-board)))
                 (and (some? hl-c) (some? clicked-square)) (when (valid-square? hl-c clicked-square read-board) 
                                                             (flip-clicked clicked-square :square)))
               (. panel (paintImmediately 0 0 (. panel (getWidth)) (. panel (getHeight)))))
             ))))

;(def ml (proxy [MouseAdapter] []
;          (mouseClicked [mouse-event] 
;            (let [read-board (get-board)
;                  hl-c (hl-checker read-board)
;                  clicked-square (clicked-square mouse-event find-clicked read-board)
;                  clicked-checker (clicked-checker mouse-event find-clicked read-board)
;                  update-clicked (fn [ele val] (when (some? ele)
;                                                 (reset! board (assoc @board (first ele) 
;                                                                      (assoc (second ele) :checker 
;                                                                             (assoc ((second ele) :checker) :clicked val))))))
;                  {move? :valid-move :as move-data} (valid-move? {:from hl-c
;                                                                      :to clicked-square} read-board)]
;             (if move?
;               (do 
;                 (let [ub (exec-move-checker move-data board {:from hl-c :to clicked-square} read-board)]
;                   (. panel (paintImmediately 0 0 (. panel (getWidth)) (. panel (getHeight))))
;                   (Thread/sleep think-time-ms)
;	                 (let [rand-move (rand-chk-move ub)]
;	                   (exec-move-checker (valid-move? rand-move ub) board rand-move ub))
;                   (. panel (paintImmediately 0 0 (. panel (getWidth)) (. panel (getHeight))))
;                   ))
;               (do
;                   (update-clicked hl-c false)
;                   (update-clicked clicked-checker true)
;                   (. panel (repaint))))))))

(defn frame [] (doto 
                 (new JFrame)
                 (-> (.getContentPane) (.add submit-button))
                 (-> (.getContentPane) (.add panel) (.addMouseListener ml))
                 .pack 
                 .show))