(ns ui.core)

(import 
 '(java.awt Color Graphics Dimension BorderLayout)
 '(java.awt.image BufferedImage)
 '(javax.swing JPanel JFrame))

(def scale 25)
(def dim 80)

(def img (new BufferedImage (* scale dim) (* scale dim) 
                 (. BufferedImage TYPE_INT_ARGB)))

(defn colorFrame [img g] 
  (let [imGraph (. img (getGraphics))]
    (doto imGraph
      (.setColor (. Color white))
      (.fillRect 0 0 (. img (getWidth)) (. img (getHeight)))
      (.setColor (. Color red))
      (.fillRect 0 0 scale scale)
      (.setColor (. Color black))
      (.fillRect scale scale scale scale)
;      (#(for [x (range 8) y (range 8)] 
;         (do 
;           (.setColor % (if (= (mod (+ x y) 2) 0)
;                          (. Color red)
;                          (. Color black)))
;           (.fillRect % 0 0 scale scale))
;         ))
      )
        
    
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