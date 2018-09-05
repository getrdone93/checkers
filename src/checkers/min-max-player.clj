(ns checkers.min-max-player
  (:refer checkers.board)
  (:refer clojure.set))

(def team :team1)

(defn evaluate [win-team other-team read-board]
  (let [over (game-over read-board)]
    (cond 
      (= over win-team) 1
      (= over other-team) -1
      (= over :tie) 0
      :else :non-terminal)))

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

(defn take-action
  ([read-board move-checker simple-paths] (mapv (fn [mv] 
                                                  (let [{rb :read-board ne :new-entry}
                                                        (move-checker {:from move-checker :to mv})]
                                                    ((king-me ne rb) :board))) simple-paths))
  ([read-board all-jump-paths] (dfs all-jump-paths 0 read-board [])))

(defn next-states [state tm] (mapv (fn [[chk mv]]
                                (cond 
                                 (some? (mv :all-jump-paths)) (take-action @board (mv :all-jump-paths))
                                 (some? (mv :simple-paths)) (take-action @board chk (mv :simple-paths))))
                                (valid-moves state tm)))

(defn max [state alpha beta ot]
  (let [su (evaluate team ot state)]
    (if (contains? {1 -1 0} su)
      su
      ((fn [[s :as poss-states]]
        ) (reduce #(into %1 %2) (next-states state team))))))

;; (def dfs-test (mapv (fn [[chk {ajp :all-jump-paths}]]
;;                     (checkers.min-max-player/dfs ajp 0 four-jump-board [])) four-jump-vm))
             

;; (defn take-action [read-board {chk :move-checker sp :simple-paths ajp :all-jump-paths}]
;;   (cond
;;     (and (some? sp) (some? ajp)) nil ;bad input
;;     (some? sp) (mapv (fn [mv] 
;;                            (let [{rb :read-board ne :new-entry} (move-checker {:from chk :to mv})]
;;                              ((king-me ne rb) :board))) sp)
;;     (some? ajp) (dfs ajp 0 read-board [])))
