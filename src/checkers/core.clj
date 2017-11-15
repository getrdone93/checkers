(ns checkers.core)

;lets write a function to define the board instead of statically defning it. true means
;playable square, false means otherwise.  
(defn genBoard [m mct n occupy res] 
  (if (< mct m)
    (genBoard m (inc mct) n (not occupy) 
              (conj res ((fn genRow [n count occupy row] (if (< count n)
                                          (genRow n (inc count) (not occupy) (conj row (assoc (assoc {} :playable occupy) :pieceHere occupy)))
                                                                    row)) n 0 occupy [])))
    res
    )
  )