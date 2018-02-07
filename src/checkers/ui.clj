(ns ui.core)

(import 
 '(java.awt Color Graphics Dimension)
 '(java.awt.image BufferedImage)
 '(javax.swing JPanel JFrame))

(def scale 5)

(def img (new BufferedImage (* scale dim) (* scale dim) 
                 (. BufferedImage TYPE_INT_ARGB)))

(def panel (doto (proxy [JPanel] []
                        (paint [g] img))
             (.setPreferredSize (new Dimension 
                                     (* scale dim) 
                                     (* scale dim)))))

;how to draw a grid on the frame?
;i think i just need to update the img with a grid of squares

(defn frame (doto (new JFrame) (.add panel) .pack .show))