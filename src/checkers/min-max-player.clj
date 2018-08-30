(ns checkers.min-max-player
  (:refer checkers.board)
  (:refer clojure.set))

(defn evaluation [win-team other-team read-board]
  (let [over (game-over read-board)]
    (cond 
      (= over win-team) 1
      (= over other-team) -1
      (= over :tie) 0)))

;; (defn max [[[[ind {{{t :team} :checker} :as entry}] paths] :as mov-chks] {la :last-action
;;                                                                           s :state} other-team]
;;   (let [utility (evaluation t other-team s)]
;;     (if (contains? #{0 -1 1} utility)
;;       {:last-action la :state s :utility utility}
;;       )))

;; (defn take-action [state {[ci chk] :move-checker sp :simple-paths ajp :all-jump-paths}]
;;   (cond
;;     (and (some? sp) (some? ajp)) nil bad input
;;     (some? sp) (mapv (fn [mv] 
;;                            (let [{rb :read-board ne :new-entry} (move-checker {:from [ci chk] :to mv})]
;;                              ((king-me ne rb) :board))) sp)
;;     (some? ajp) ((fn enumerate-jumps [[{p :path ns :next} :as path] res]
;;                     ) ajp [])))

(defn exec-jump [{from :from to :to :as move-form} jumped-chk read-board]
     (let [{mb :read-board ne :new-entry} (move-checker move-form (remove-checker jumped-chk read-board))]
       ((king-me ne mb) :board)))

(defn dfs [ajp ajp-i read-board res]
  ((fn recur-ns [jps ajp-ind b curr-ns r]
     (let [{[[sci _] :as chks] :path cns :next} (jps ajp-ind)
           nji (first (difference cns curr-ns))]
       (if (nil? nji)
         (conj r b)
         (let [{[jc [mci _] :as nep] :path nens :next} (jps nji)
               board-sc [sci (b sci)]
               board-mc [mci (b mci)]
               nb (exec-jump {:from board-sc :to board-mc} jc b)]
          (recur jps ajp-ind b (conj curr-ns nji) (into r (dfs jps nji nb r))))))) ajp ajp-i read-board #{} res))
