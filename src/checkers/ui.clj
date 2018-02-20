(ns ui.core)

(import 
 '(java.awt Color Graphics Dimension BorderLayout)
 '(java.awt.image BufferedImage)
 '(javax.swing JPanel JFrame))

(def scale 100)
(def dim 80)
(def num-squares 64)

(def img (new BufferedImage (* scale dim) (* scale dim) 
                 (. BufferedImage TYPE_INT_ARGB)))

(defn colorFrame [img g] 
  (let [imGraph (. img (getGraphics))]
    (.setColor imGraph (. Color white))
    (.fillRect imGraph 0 0 (. img (getWidth)) (. img (getHeight)))
    (dotimes [n num-squares]
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
            ]
        (.setColor imGraph c)
        (.fillRect imGraph x y scale scale)))
    (. g (drawImage img 0 0 nil))
    (. imGraph (dispose))))

;proxy implements/extends a interface/class where the supplied arguments
;are arguments to the class's super constructor and then calls
;the supplied functions
(def panel (doto (proxy [JPanel] [(new BorderLayout)]
                        (paint [g] (colorFrame img g)))
             (.setPreferredSize (new Dimension 
                                     (/ (* scale dim) 5) 
                                     (/ (* scale dim) 5)))))

(defn frame [] (doto (new JFrame) (.add panel) .pack .show))