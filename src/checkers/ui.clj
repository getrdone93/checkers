;As with all journeys, it begins with a simple willingness, with an abiding faith in the unknown.
(ns checkers.ui
  (:refer checkers.board)
  (:refer checkers.random-player)
  (:refer clojure.set))

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
(def mjs 900)

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
                       (= (((second %) :square) :color) black))
                          (map-indexed #(find-func %1 %2 :square :square-obj mouse-event) read-board))))

(defn clicked-checker [mouse-event find-func read-board]
  (first (filter some? 
                 (map-indexed #(find-func %1 %2 :checker :checker-obj mouse-event) read-board))))

(defn valid-square? [checker square read-board]
  (let [{sp :simple-paths ajp :all-jump-paths} (paths checker read-board)]
    (or (some? (first (filter #(= square %) sp))) 
        (some? (first (filter some? (set (map (fn [{p :path}]
                                              (first (filter #(= square %) p))) ajp))))))))

(defn flip-clicked [[ind {{val :clicked} uk :as entry}] uk new-board]
  (when (some? entry) 
    (assoc new-board ind 
      (assoc entry uk 
             (assoc (entry uk) :clicked (not val))))))

(defn unclick-squares [[fs :as sqs] new-board]
  (if (some? fs)
    (unclick-squares (rest sqs) (flip-clicked fs :square new-board))
    new-board))

(defn simple-move? [hlsqs sps]
  (= 1 (count (intersection sps hlsqs))))

(defn jump-move? [hlsqs ajp]
  (and (some? ajp) 
       (> (count (intersection hlsqs 
                               (reduce #(union %1 %2) (map (fn [{p :path}]
                                                                   (set p)) ajp)))) 0)))

(defn exec-simple-move! [hlc hlsqs read-board]
  (let [{mb :read-board ne :new-entry} (move-checker {:from hlc :to (first hlsqs)} read-board)
        {kb :board ke :entry} (king-me ne mb)]
        (reset! board (unclick-squares [ke] kb))
        (. panel (paintImmediately 0 0 (. panel (getWidth)) (. panel (getHeight))))
        @board))

(defn exec-jump-move! [hlc ajp]
    (let [move-path (filter (fn [{p :path :as entry}]
                                      (when (reduce #(or %1 %2) 
                                                    (map (fn [[_ {{sqclk :clicked} :square}]]
                                                       sqclk) p))
                                        entry)) ajp)]
      ((fn traverse [[{p :path} :as mp] c rb]
         (if (some? p)
           (let [{mb :read-board ne :new-entry} (move-checker {:from c :to (last p)} 
                                                        (remove-checker (first p) rb))
                 {kb :board [kei ke] :entry} (king-me ne mb) 
                 nb (unclick-squares [[kei ke]] kb)]
             (reset! board nb)
             (. panel (paintImmediately 0 0 (. panel (getWidth)) (. panel (getHeight))))
             (Thread/sleep mjs)
             (traverse (rest mp) [kei (nb kei)] nb))
           @board)) move-path hlc @board)))

(defn get-clicked [ajp indicies]
  (set (filter some? (map (fn [{p :path :as entry}]
                            (when (((second (last p)) :square) :clicked)
                              entry)) (map ajp indicies)))))

(defn clicks-per-level [ajp ns levels]
  (if (empty? ns)
    levels
    (let [eles (vec (get-clicked ajp ns))]
      (if (empty? eles)
        levels
        (clicks-per-level ajp (reduce (fn [x y]
                                  (union x y)) (map :next eles)) 
                    (conj levels (= (count eles) 1)))))))

(defn single-path? [[{ns :next} :as ajp]]
  (let [levels (clicks-per-level ajp ns [])]
    (if (empty? levels)
      false
      (reduce #(and %1 %2) levels))))

(defn broken-path? [[fe :as tf-path]]
    (and (> (count tf-path) 1) 
         (or (and (true? fe) (true? (last tf-path)) (> (count tf-path) 2) 
                     (false? (reduce #(and %1 %2) (drop-last (rest tf-path)))))
                (and (false? fe) (true? (last tf-path))))))

(defn find-broken-path [[{ns :next p :path} :as ajp]]
  ((fn traverse [next-set jp cp]
     (if (some? (first next-set))
       (let [{ep :path entry-ns :next :as entry} (jp (first next-set))
             [_ {{sqclk :clicked} :square}] (last ep)
             np (conj cp sqclk)]
         (if (broken-path? np)
           {:broken-path true :path np} 
           (let [{bp :broken-path :as ret} (traverse entry-ns jp np)]
             (if bp
               ret
               (traverse (rest next-set) jp cp)))))
       {:broken-path false :path cp})) ns ajp []))

(defn reset-game! [team]
  (cond 
        (= team :team1) (do
                         (JOptionPane/showMessageDialog (new JFrame) "Team 1 wins!")
                         (reset! board (gen-board 0 [])))
        (= team :team2) (do
                         (JOptionPane/showMessageDialog (new JFrame) "Team 2 wins!")
                         (reset! board (gen-board 0 []))))
  (. panel (paintImmediately 0 0 (. panel (getWidth)) (. panel (getHeight)))))

(defn winner [read-board]
  (let [w (when (some? read-board)
                (game-over read-board))]
    (when (contains? #{:team1 :team2} w)
      w)))

(defn computer-move! [read-board move-func]
  (let [{nb :read-board} (move-func read-board)]
    (Thread/sleep 400) ;"think" for a bit
     (let [w (winner read-board)] 
       (if (some? w)
         (reset-game! w)
         (do 
           (reset! board nb)
           (. panel (paintImmediately 0 0 (. panel (getWidth)) (. panel (getHeight)))))))))

(defn human-move [read-board]
  (let [hlsqs (hl-squares read-board)
        hlc (hl-checker read-board)
        {sps :simple-paths ajp :all-jump-paths} (paths hlc read-board)
        sm (simple-move? hlsqs sps)
        jm (jump-move? hlsqs ajp)]
    (cond (and sm jm) nil ;not a valid move, so do nothing
          sm (exec-simple-move! hlc hlsqs read-board)
          jm (when (and (false? ((find-broken-path ajp) :broken-path))
                        (single-path? ajp)) 
               (exec-jump-move! hlc ajp)))))

(defn button-click [action-event]
  (let [hm (human-move @board)]
    (when (some? hm)
        (let [w (winner @board)] 
          (if (some? w)
            (reset-game! w)
            (computer-move! @board rand-chk-move))))))

(def ml (proxy [MouseAdapter] []
          (mouseClicked [mouse-event] 
            (let [read-board (get-board)
                  hl-c (hl-checker read-board)
                  clicked-square (clicked-square mouse-event find-clicked read-board)
                  clicked-checker (clicked-checker mouse-event find-clicked read-board)]

               (do 
                 (cond 
                   (and (nil? hl-c) (some? clicked-checker)) (reset! board (flip-clicked clicked-checker :checker read-board))
                   (and (some? hl-c) 
                        (some? clicked-checker)) (reset! board 
                                                         (let [new-board (flip-clicked clicked-checker :checker 
                                                                           (flip-clicked hl-c :checker read-board))] 
                                                                                           (unclick-squares 
                                                                                             (vec (hl-squares new-board)) new-board)))
                                                   
                                                              
                   (and (some? hl-c) (some? clicked-square)) (when (valid-square? hl-c clicked-square read-board) 
                                                                 (reset! board 
                                                                         (flip-clicked clicked-square :square read-board))))
                 (. panel (paintImmediately 0 0 (. panel (getWidth)) (. panel (getHeight)))))))))

(defn frame [] (doto 
                 (new JFrame)
                 (-> (.getContentPane) (.add submit-button))
                 (-> (.getContentPane) (.add panel) (.addMouseListener ml))
                 .pack 
                 .show))