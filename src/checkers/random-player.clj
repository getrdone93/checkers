(ns checkers.random-player
  (:refer checkers.board))

(def r-team :team1)
(def think-time-ms 600)


(defn random-jump [ajp]
  (ajp (first (shuffle ((first ajp) :next)))))

(defn rand-chk-move [read-board] 
  (let [[checker {sps :simple-paths ajp :all-jump-paths}] 
        (first (shuffle (movable-checkers r-team read-board)))]
    (cond
      (some? ajp) (let [{p :path} (random-jump ajp)]
                    (move-checker {:from checker :to (last p)} (remove-checker (first p) read-board)))
      (some? sps) (move-checker {:from checker :to (first (shuffle sps))} read-board)
      :else nil)))
