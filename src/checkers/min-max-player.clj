(ns checkers.min-max-player
  (:refer checkers.board)
  (:refer clojure.set))

(def mm-team :team1)

(defn evaluate [win-team other-team read-board tie]
  (let [{over :board-result t :tie} (game-over read-board tie)]
    {:utility (cond 
               (= over win-team) 1
               (= over other-team) -1
               (= over :tie) 0
               :else :non-terminal)
     :tie t}))

(defn dfs-over-ajp [ajp ajp-i read-board res cmp]
  ((fn [jps ajp-ind b curr-ns r mp]
     (let [{p :path cns :next} (jps ajp-ind)
           [sci _] (last p)
           nji (first (difference cns curr-ns))]
       (if (nil? nji)
         (if (empty? cns)
           (conj r {:board b :action mp})
           r)
         (let [{[jc [mci _] :as nep] :path nens :next} (jps nji)
               board-sc [sci (b sci)]
               board-mc [mci (b mci)]
               mv {:from board-sc :to board-mc :jumped-chk jc}
               nb (exec-jump mv b)]
          (recur jps ajp-ind b (conj curr-ns nji)
                 (into r (dfs-over-ajp jps nji nb r (conj mp mv))) mp))))) ajp ajp-i read-board #{} res cmp))

(defn take-action
  ([read-board sc sps] (mapv (fn [mv] 
                               (let [{rb :read-board ne :new-entry}
                                     (move-checker {:from sc :to mv} read-board)]
                                 {:board ((king-me ne rb) :board) :action [{:from sc :to mv}]})) sps))
  ([read-board ajp] (dfs-over-ajp ajp 0 read-board [] [])))

(defn next-states [state tm] (mapv (fn [[chk mv]]
                                     (cond 
                                      (some? (mv :all-jump-paths)) (take-action state (mv :all-jump-paths))
                                      (some? (mv :simple-paths)) (take-action state chk (mv :simple-paths))))
                                   (valid-moves tm state)))

(defn next-states-flat [state tm]
  (reduce #(into %1 %2) (next-states state tm)))

(def calls (atom 0))
(def max-depth 5)

(defn log-output [d su]
  (do
    (swap! calls inc)
    (if (= (mod @calls 5000) 0)
      (println "calls " @calls " depth " d " su " su))))

(defn general-search [state alpha beta ot min-max tie d]
  (let [{su :utility t :tie} (evaluate mm-team ot state tie)
        max? (= min-max max)]
    (log-output d su)
    (if (or (contains? #{1 -1 0} su) (>= d max-depth))
      {:state state :value (if (= su :non-terminal)
                             (rand 1)
                             su)}
      ((fn [[{s :board act :action} :as poss-states] sa pv a b]
         (if (some? s)
           (let [tv ((if max?
                       (general-search s a b ot min t (inc d))
                       (general-search s a b ot max t (inc d))) :value)
                 nv (min-max pv tv)]
             (if (if max?
                   (>= nv b)
                   (<= nv a))
               {:state s :value nv :acts (conj sa {:action act :value nv})}
               (if max?
                 (recur (rest poss-states) (conj sa {:action act :value nv}) nv (max a nv) b)
                 (recur (rest poss-states) (conj sa {:action act :value nv}) nv a (min b nv)))))
           {:state state :value pv :acts sa})) (next-states-flat state mm-team) [] (if max?
                                                                         Double/NEGATIVE_INFINITY
                                                                         Double/POSITIVE_INFINITY) alpha beta))))

(defn alpha-beta-search [state]
  (general-search state Double/NEGATIVE_INFINITY Double/POSITIVE_INFINITY :team2
                  max {:board [] :team-counts #{} :times 0} 0))

(defn max-abs [state]
  (let [abs (alpha-beta-search state)]
    ((first (filter #(= (abs :value) (% :value)) (abs :acts))) :action)))
