(ns checkers.min-max-player
  (:refer checkers.board))

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

(defn dfs [ajp read-board res]
  ((fn recur-ns [ajp-ind jps b]
     (let [{[[sci sc] :as chks] :path cns :next} (jps ajp-ind)]
       (if (nil? (first cns))

         (let [{[_ [mci mc] :as nep] :path nens :next} (jps (first cns))
               ]
           )))) 0 ajp read-board))
