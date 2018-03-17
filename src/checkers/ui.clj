(ns ui.core)

(import 
 '(java.awt Color Graphics Dimension BorderLayout)
 '(java.awt.image BufferedImage)
 '(javax.swing JPanel JFrame JTextArea)
 '(java.awt.event MouseAdapter MouseEvent))

(def scale 100)
(def dim 80)
(def num-squares 64)
(def t1-color (. Color yellow))
(def t2-color (. Color magenta))
(def circ-dim (/ scale 2))
(def shift (/ circ-dim 2))
(def t1-rows (set (range 0 3)))
(def t2-rows (set (range 5 8)))
(def checker-rows (clojure.set/union t1-rows t2-rows))

(def img (new BufferedImage (* scale dim) (* scale dim) 
                 (. BufferedImage TYPE_INT_ARGB)))

;(defn colorFrame [img g] 
;  (let [imGraph (. img (getGraphics))
;        t1Rows (set (range 0 3))
;        t2Rows (set (range 5 8))
;        circRows (set (clojure.set/union t1Rows t2Rows))
;        circDim (/ scale 2)
;        shift (/ circDim 2)]
;    (.setColor imGraph (. Color white))
;    (.fillRect imGraph 0 0 (. img (getWidth)) (. img (getHeight)))
;    (dotimes [n num-squares]
;      (let [place (mod n 8)
;            r (quot n 8)
;            x (if (zero? place)
;                0
;                (* place scale))
;            y (* r scale)
;            c (if (zero? (mod r 2))
;                (if (zero? (mod n 2))
;                  (. Color red)
;                  (. Color black))
;                (if (zero? (mod n 2))
;                  (. Color black)
;                  (. Color red)))]
;        (.setColor imGraph c)
;        (.fillRect imGraph x y scale scale)
;        (when (and (contains? circRows r) (= c (. Color black)))
;          (do
;            (.setColor imGraph (if (contains? t1Rows r)
;                                 t1-color
;                                 t2-color))
;            (.fillOval imGraph (+ x shift) (+ y shift) circDim circDim)))))
;    (. g (drawImage img 0 0 nil))
;    (. imGraph (dispose))))

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
                            (. Color red)))]
                  (if (< n num-squares)
                    (gen-board (inc n) (conj br {:square {:point [x y]
                                                           :color c
                                                           :valid-click-locs []}
                                                  :checker {:point [(+ x shift) (+ y shift)]
                                                            :color (if (contains? t1-rows r)
                                                                     t1-color
                                                                     t2-color)}}))
                    br)))

(def board (gen-board 0 []))

(defn color-frame [img g]
  (let [im-graph (. img (getGraphics))]
       (.setColor im-graph (. Color white))
       (.fillRect im-graph 0 0 (. img (getWidth)) (. img (getHeight)))
       ((fn draw-board [b]
         (let [square ((first board) :square)
               checker ((first board) :checker)
               cx (first (checker :point))
               cy (second (checker :point))
               sqx (first (square :point))
               sqy (second (square :point))]
	         (when (not (empty? b))     
            (.setColor im-graph (((first board) :square) :color))
            (.fillRect im-graph sqx sqy scale scale)
            (when (= (square :color) (. Color black))
              (.setColor im-graph (checker :color))
              (.fillOval im-graph cx cy circ-dim circ-dim)))
          (draw-board (rest b)))) board)
       (. g (drawImage img 0 0 nil))
       (. im-graph (dispose))))

(def ml (proxy [MouseAdapter] []
          (mouseClicked [mouse-event] 
            (spit "output.txt" "clicked something\n" :append true))))

;proxy implements/extends a interface/class where the supplied arguments
;are arguments to the class's super constructor and then calls
;the supplied functions
(def panel (doto (proxy [JPanel] []
                        (paint [g] (color-frame img g)))
             (.setPreferredSize (new Dimension 
                                     (/ (* scale dim) 5) 
                                     (/ (* scale dim) 5)))
             (.addMouseListener ml)))

(defn frame [] (doto 
                 (new JFrame) 
                 (-> (.getContentPane) (.add panel))
                 .pack 
                 .show))
  