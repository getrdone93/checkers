(ns checkers.random-player
  (:refer checkers.board))

(def team :team1)


(defn random-jump [ajp]
  (last ((ajp (first (shuffle ((first ajp) :next)))) :path)))

;
;come back with
;(valid-move? {:from hl-c :to clicked-square} read-board)
;and {:from hl-c :to clicked-square}
(defn rand-chk-move [read-board] 
  (let [[checker {sps :simple-paths ajp :all-jump-paths}] 
        (first (shuffle (movable-checkers team read-board)))]
    (cond
      (and (some? sps) (some? ajp)) (if (zero? (rand-int 2))
                                      {:from checker :to (first (shuffle sps))}
                                      {:from checker :to (random-jump ajp)})
      (some? sps) {:from checker :to (first (shuffle sps))}
      (some? ajp) {:from checker :to (random-jump ajp)}
      ;if we get here, then theres a problem with movable checkers
      :else nil)))