(ns checkers.min-max-player
  (:refer checkers.board)
  (:refer clojure.set))

(defn evaluation [win-team other-team read-board]
  (let [over (game-over read-board)]
    (cond 
      (= over win-team) 1
      (= over other-team) -1
      (= over :tie) 0)))

(defn exec-jump [{from :from to :to :as move-form} jumped-chk read-board]
     (let [{mb :read-board ne :new-entry} (move-checker move-form (remove-checker jumped-chk read-board))]
       ((king-me ne mb) :board)))

(defn dfs [ajp ajp-i read-board res]
  ((fn [jps ajp-ind b curr-ns r]
     (let [{p :path cns :next} (jps ajp-ind)
           [sci _] (last p)
           nji (first (difference cns curr-ns))]
       (if (nil? nji)
         (if (empty? cns)
           (conj r b)
           r)
         (let [{[jc [mci _] :as nep] :path nens :next} (jps nji)
               board-sc [sci (b sci)]
               board-mc [mci (b mci)]
               nb (exec-jump {:from board-sc :to board-mc} jc b)]
          (recur jps ajp-ind b (conj curr-ns nji) (into r (dfs jps nji nb r))))))) ajp ajp-i read-board #{} res))

; (def dfs-test (map (fn [[chk {ajp :all-jump-paths}]]
;                    (checkers.min-max-player/dfs ajp 0 four-jump-board [])) four-jump-vm))
            
