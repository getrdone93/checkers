(ns checkers.core)

;define board. true means checker is occupying space and false means nothing is there. 
(def board [[false true false true false true false true] 
            [true false true false]])


;lets write a function to define the board instead of statically defning it
(def board [m n] 
  (genBoard m 0 n false []))

(defn genBoard [m mct n occupy res] 
  (if (< mct m)
    (genBoard m (inc mct) n (not occupy) 
              (conj res ((fn genRow [n count occupy row] (if (< count n)
                                                            (genRow n (inc count) (not occupy) (assoc row count occupy))
                                                            row)) n 0 occupy [])))
    res
    )
  )