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

;output the board
(defn output [board] 
  (when (not (empty? board))
    ((fn printRow [row]
      (when (not (empty? row))
        ((fn printSquare [square] 
          (when (not (empty? square))
            (if (square :playable)
              (if (square :pieceHere)
                (print "O ")
                (print "U "))
              (print "x ")))) (first row))
        (printRow (rest row)))) (first board))
    (println)
    (output (rest board))))

(def upperBoundDim (dec (count board)))
(def lowerBoundDim 0)

;source and destination should be an element from the board, namely, the map element at [m n] or [x y]
(defn validMove [source destination board] 
  )

;accepts a pair as [m n]
(defn getElement [pair board]
  (if (or (or (> (first pair) upperBoundDim) (< (first pair) lowerBoundDim)) 
          (or (> (second pair) upperBoundDim (< (second pair) lowerBoundDim))))
    ;invalid index, return nil
    nil
    (nth (nth board (first pair)) (second pair))))

;define move function
(defn move [source destination board] 
  (if (validityCheck source destination)
    ;move is valid, so enact move on board and obtain new board
    
    ;othwerise, do an error or something
    ))
