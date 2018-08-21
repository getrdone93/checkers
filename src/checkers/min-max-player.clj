(ns checkers.min-max-player
  (:refer checkers.board))


(def evaluation-func [my-team other-team read-board go-func]
  (let [over (go-func read-board)]
    (cond 
      (= over win-team) 1
      (= over other-team) -1
      (= over :tie) 0)))

(defn generate-tree [read-board]
  )
