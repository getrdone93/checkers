(ns checkers.min-max-player
  (:refer checkers.board)
  (:refer clojure.set))

(def team :team1)

(defn evaluate [win-team other-team read-board tie]
  (let [{over :board-result t :tie} (game-over read-board tie)]
    {:utility (cond 
               (= over win-team) 1
               (= over other-team) -1
               (= over :tie) 0
               :else :non-terminal)
     :tie t}))

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
  ([read-board sc simple-paths] (mapv (fn [mv] 
                                        (let [{rb :read-board ne :new-entry}
                                              (move-checker {:from sc :to mv} read-board)]
                                          ((king-me ne rb) :board))) simple-paths))
  ([read-board all-jump-paths] (dfs all-jump-paths 0 read-board [])))

(defn next-states [state tm] (mapv (fn [[chk mv]]
                                     (cond 
                                      (some? (mv :all-jump-paths)) (take-action state (mv :all-jump-paths))
                                      (some? (mv :simple-paths)) (take-action state chk (mv :simple-paths))))
                                   (valid-moves tm state)))

(defn next-states-flat [state tm]
  (reduce #(into %1 %2) (next-states state tm)))

(def calls (atom 0))
(def max-depth 5)

(defn general-search [state alpha beta ot min-max tie d]
  (let [{su :utility t :tie} (evaluate team ot state tie)
        max? (= min-max max)]
    (do
      (swap! calls inc)
      (if (= (mod @calls 5000) 0)
        (println "calls " @calls " depth " d " su " su)))
    (if (or (contains? #{1 -1 0} su) (>= d max-depth))
      {:state state :value (if (= su :non-terminal)
                             (rand 1)
                             su)}
      ((fn [[s :as poss-states] pv a b]
         (if (some? s)
           (let [nv (min-max pv ((if max?
                                   (general-search s a b ot min t (inc d))
                                   (general-search s a b ot max t (inc d))) :value))]
             (if (if max?
                   (>= nv b)
                   (<= nv a))
               {:state s :value nv}
               (if max?
                 (recur (rest poss-states) nv (max a nv) b)
                 (recur (rest poss-states) nv a (min b nv)))))
           {:value pv})) (next-states-flat state team) (if max?
                                                         Double/NEGATIVE_INFINITY
                                                         Double/POSITIVE_INFINITY) alpha beta))))
(defn alpha-beta-search [state]
  (general-search state Double/NEGATIVE_INFINITY Double/POSITIVE_INFINITY :team2 max {:board [] :team-counts #{} :times 0}
                  0))
