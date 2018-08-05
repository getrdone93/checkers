(ns checkers.random-player
  (:refer checkers.board))

(def team :team1)
(def think-time-ms 600)


(defn random-jump [ajp]
  (ajp (first (shuffle ((first ajp) :next)))))

;
;come back with
;(valid-move? {:from hl-c :to clicked-square} read-board)
;and {:from hl-c :to clicked-square}
(defn rand-chk-move [read-board] 
  (let [[checker {sps :simple-paths ajp :all-jump-paths}] 
        (first (shuffle (movable-checkers team read-board)))]
    (cond
      (some? ajp) {:from checker :to (random-jump ajp)}
      (some? sps) {:from checker :to (first (shuffle sps))}
      ;if we get here, then theres a problem with movable checkers
      :else nil)))


(defn rand-chk-move-new [read-board] 
  (let [[checker {sps :simple-paths ajp :all-jump-paths}] 
        (first (shuffle (movable-checkers team read-board)))]
    (cond
      (some? ajp) (let [{p :path} (random-jump ajp)]
                    (move-checker {:from checker :to (last p)} (remove-checker (first p) read-board)))
      (some? sps) (move-checker {:from checker :to (first (shuffle sps))} read-board)
      :else nil)))