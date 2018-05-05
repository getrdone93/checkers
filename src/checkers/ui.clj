;As with all journeys, it begins with a simple willingness, with an abiding faith in the unknown.
(ns ui.core)

(import 
 '(java.awt Color Graphics Dimension BorderLayout Shape)
 '(java.awt.image BufferedImage)
 '(java.awt.geom Rectangle2D$Double Ellipse2D$Double)
 '(javax.swing JPanel JFrame JTextArea)
 '(java.awt.event MouseAdapter MouseEvent))

;for using doc function!
(use 'clojure.repl) 

(def scale 100)
(def dim 80)
(def num-squares 64)
(def team-color {:team1 (. Color yellow)
                 :team2 (. Color magenta)})
(def circ-dim (/ scale 2))
(def circ-hl (+ circ-dim 9))
(def shift (/ circ-dim 2))
(def t1-rows (set (range 0 3)))
(def t2-rows (set (range 5 8)))
(def checker-rows (clojure.set/union t1-rows t2-rows))
(def black (. Color black))
(def red (. Color red))

(defn hl-shift [cp] (- cp 4))

(defn checker-point [sqx sqy] [(+ sqx shift) (+ sqy shift)])

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
                                                 :checker (when (and (contains? checker-rows r) (= c black))
                                                            {:point cp
                                                             :team (cond 
                                                                     (contains? t1-rows r) [:team1 (. Color yellow)]
                                                                     (contains? t2-rows r) [:team2 (. Color magenta)]
                                                                     :else nil)
                                                             :valid-click-locs []
                                                             :checker-obj (new Ellipse2D$Double (first cp) 
                                                                               (second cp) circ-dim circ-dim)
                                                             :clicked false})})) 
                    br)))
(def board (atom (gen-board 0 [])))

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
                    (.setColor im-graph (. Color green))
                    (.fillOval im-graph (hl-shift cx) (hl-shift cy) circ-hl circ-hl))
                  (.setColor im-graph (second (checker :team)))
                  (.fillOval im-graph cx cy circ-dim circ-dim)))
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

(defn get-board [] @board)

(def move-func {:team2 [(fn [ind] (- ind 9)) (fn [ind] (- ind 7))]
                :team1 [(fn [ind] (+ ind 9)) (fn [ind] (+ ind 7))]})

(defn valid-index? 
  "performs a bounds check on ind"
  [ind] 
  (and (< 0 ind) (< ind num-squares)))

(defn compute-all-moves
  "compute moves for the highlighted checker"
  [[chk-ind {chk :checker 
             {[team _] :team} 
             :checker :as checker}] read-board]
  (let [v1 ((first (move-func team)) chk-ind)
        v2 ((second (move-func team)) chk-ind)]
    (filter some? [(when (valid-index? v1)
                     (read-board v1))
                   (when (valid-index? v2)
                     (read-board v2))])))

(defn move [ind read-board] 
  (when (and (valid-index? ind) (= black (((read-board ind) :square) :color)))
    (read-board ind)))

(defn classify-move [o-team ind move la-func]
  (when (some? move)
    (let [{chk :checker
           [team _] :team} move]
      (cond
		         (nil? chk) [false move]
		         (= o-team team) [false nil]
		         :else (let [la-ind (la-func (la-func ind))
                         la-sq (move la-ind)]
                      (if (and (some? la-sq) (nil? (la-sq :checker)))
                               [true move]
                               [false nil]))))))

(defn paths [o-team [ind {chk :checker 
                      [team _] :team 
                      :as square}] read-board sub-path res]
  (let [[left right] (move-func o-team)
        left-move (move (left ind) read-board)
        right-move (move (right ind) read-board)
        [left-recur nl-sq] (classify-move o-team ind left-move left)
        [right-recur nr-sq] (classify-move o-team ind right-move right)]
    (if (true? left-recur)
      (paths o-team [(left ind) left-move] read-board (conj sub-path nl-sq) res)
      (if (true? right-recur)
        (paths o-team [(right ind) right-move] read-board (conj sub-path nr-sq) res)
        (let [lp (if (some? nl-sq) 
                   (conj sub-path nl-sq)
                   sub-path) 
              rp (if (some? nr-sq)
                   (conj sub-path nr-sq)
                   sub-path)]
          (conj (conj res lp) rp))))))

(defn move-checker [[chk-ind checker] 
                    [sq-ind {{sq-point :point} :square :as square}] 
                    read-board]
  (let [cp (checker-point (first sq-point) (second sq-point))
        move-chk (assoc (checker :checker) 
                        :point cp
                        :clicked false
                        :checker-obj (new Ellipse2D$Double (first cp) 
                                          (second cp) circ-dim circ-dim))]
    (assoc (assoc read-board chk-ind (assoc checker :checker nil)) 
           sq-ind 
           (assoc square :checker move-chk))))

(defn valid-move? [checker square read-board]
  (if (and (some? checker) (some? square)) 
    (reduce #(or %1 %2) (map #(= % (second square)) 
                             (compute-all-moves checker read-board)))
    false))

(def ml (proxy [MouseAdapter] []
          (mouseClicked [mouse-event] 
            (let [read-board (get-board)
                  find-clicked (fn [index ele key shapeKey]
                                 (when (and (some? (ele key)) (some? ((ele key) shapeKey)) 
                                             (. ((ele key) shapeKey) (contains 
                                                                       (. mouse-event (getX)) 
                                                                       (. mouse-event (getY)))))
                                       [index ele]))
                  clicked-square (first (filter #(and (some? %) (= (((second %) :square) :color) (. Color black)))
                                                           (map-indexed #(find-clicked %1 %2 :square :square-obj) read-board)))
                  clicked-checker (first (filter some? (map-indexed #(find-clicked %1 %2 :checker :checker-obj) read-board)))
                  hl-checker (first (filter some? (map-indexed (fn [index ele] 
                                                  (when (and (some? (ele :checker)) ((ele :checker) :clicked))
                                                    [index ele])) read-board)))
                  update-clicked (fn [ele val] (when (some? ele)
                                                 (reset! board (assoc @board (first ele) 
                                                                      (assoc (second ele) :checker 
                                                                             (assoc ((second ele) :checker) :clicked val))))))
                  move? (valid-move? hl-checker clicked-square read-board)]

                (if move?
                  (reset! board (move-checker hl-checker clicked-square read-board))
                  (do
                    (update-clicked hl-checker false)
                    (update-clicked clicked-checker true)))
                
                (. panel (repaint))))))

(defn frame [] (doto 
                 (new JFrame) 
                 (-> (.getContentPane) (.add panel) (.addMouseListener ml))
                 .pack 
                 .show))


;these are the ones i care about
;(map (fn [[ind ele]] ind) (filter some? (map-indexed (fn [ind {{color :color} :square :as ele}] 
;                                                             (when (= black color)
;                                                               [ind ele])) @board)))

;(defn move-checker? [checker square] 
;  (cond 
;    (or (nil? checker) (nil? square)) false
;    (some? ((second square) :checker)) false
;    :else true))