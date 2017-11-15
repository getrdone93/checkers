(ns checkers.core)

;dynamically generate board. (doesnt account for initial empty spots in rows 4 and 5)
(defn genBoard [m mct n occupy res] 
  (if (< mct m)
    (genBoard m (inc mct) n (not occupy) 
              (conj res ((fn genRow [n count occupy row] (if (< count n)
                                          (genRow n (inc count) (not occupy) (conj row (assoc (assoc {} :playable occupy) :pieceHere occupy)))
                                                                    row)) n 0 occupy [])))
    res
    )
  )

;static board
(def board [[{:playable false, :pieceHere false}
  {:playable true, :pieceHere true}
  {:playable false, :pieceHere false}
  {:playable true, :pieceHere true}
  {:playable false, :pieceHere false}
  {:playable true, :pieceHere true}
  {:playable false, :pieceHere false}
  {:playable true, :pieceHere true}]
            
 [{:playable true, :pieceHere true}
  {:playable false, :pieceHere false}
  {:playable true, :pieceHere true}
  {:playable false, :pieceHere false}
  {:playable true, :pieceHere true}
  {:playable false, :pieceHere false}
  {:playable true, :pieceHere true}
  {:playable false, :pieceHere false}]
 
 [{:playable false, :pieceHere false}
  {:playable true, :pieceHere true}
  {:playable false, :pieceHere false}
  {:playable true, :pieceHere true}
  {:playable false, :pieceHere false}
  {:playable true, :pieceHere true}
  {:playable false, :pieceHere false}
  {:playable true, :pieceHere true}]
 
 [{:playable true, :pieceHere false}
  {:playable false, :pieceHere false}
  {:playable true, :pieceHere false}
  {:playable false, :pieceHere false}
  {:playable true, :pieceHere false}
  {:playable false, :pieceHere false}
  {:playable true, :pieceHere false}
  {:playable false, :pieceHere false}]
 
 [{:playable false, :pieceHere false}
  {:playable true, :pieceHere false}
  {:playable false, :pieceHere false}
  {:playable true, :pieceHere false}
  {:playable false, :pieceHere false}
  {:playable true, :pieceHere false}
  {:playable false, :pieceHere false}
  {:playable true, :pieceHere false}]
 
 [{:playable true, :pieceHere true}
  {:playable false, :pieceHere false}
  {:playable true, :pieceHere true}
  {:playable false, :pieceHere false}
  {:playable true, :pieceHere true}
  {:playable false, :pieceHere false}
  {:playable true, :pieceHere true}
  {:playable false, :pieceHere false}]
 
 [{:playable false, :pieceHere false}
  {:playable true, :pieceHere true}
  {:playable false, :pieceHere false}
  {:playable true, :pieceHere true}
  {:playable false, :pieceHere false}
  {:playable true, :pieceHere true}
  {:playable false, :pieceHere false}
  {:playable true, :pieceHere true}]
 
 [{:playable true, :pieceHere true}
  {:playable false, :pieceHere false}
  {:playable true, :pieceHere true}
  {:playable false, :pieceHere false}
  {:playable true, :pieceHere true}
  {:playable false, :pieceHere false}
  {:playable true, :pieceHere true}
  {:playable false, :pieceHere false}]])

(defn printSquare [square] 
        (when (not (empty? square))
          (if (square :playable)
            (if (square :pieceHere)
              (print "o ")
              (print "u "))
            (print " "))))

(defn printRow [row]
     (when (not (empty? row))
       (printSquare (first row))
       (printRow (rest row))))

;output the board
(defn output [board] 
  (when (not (empty? board))
    (printRow (first board))
    (println)
    (output (rest board))))