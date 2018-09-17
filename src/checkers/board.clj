(ns checkers.board
  (:refer clojure.set))

(import 
 '(java.awt Color Graphics Dimension BorderLayout Shape)
 '(java.awt.geom Rectangle2D$Double Ellipse2D$Double))

(def scale 100)
(def num-squares 64)
(def circ-dim (/ scale 2))
(def t1-rows (set (range 0 3)))
(def t2-rows (set (range 5 8)))
(def checker-rows (clojure.set/union t1-rows t2-rows))
(def shift (/ circ-dim 2))
(def king-ind {:team1 #{56 58 60 62}
               :team2 #{1 3 5 7}})

(def black (. Color black))
(def red (. Color red))
(def green (. Color green))
(def cyan (. Color cyan))
(def yellow (. Color yellow))
(def magenta (. Color magenta))
(def default-tie-limit 20)

(defn checker-point [sqx sqy] [(+ sqx shift) (+ sqy shift)])

(defn gen-board [n br]
                (let [place (mod n 8)
                      r (quot n 8)
                      x (if (zero? place)
                          0
                        (* place scale))
                      y (* r scale)
                      c (if (zero? (mod r 2))
                          (if (zero? (mod n 2))
                            red
                            black)
                          (if (zero? (mod n 2))
                            black
                            red))
                      cp (checker-point x y)]
                  (if (< n num-squares)
                    (gen-board (inc n) (conj br {:square {:point [x y]
                                                           :color c
                                                           :valid-click-locs []
                                                           :square-obj (new Rectangle2D$Double x y scale scale)
                                                           :clicked false}
                                                 :checker (when (and (contains? checker-rows r) (= c black))
                                                            {:point cp
                                                             :team (if (contains? t1-rows r)
                                                                    [:team1 yellow]
                                                                    [:team2 magenta]) 
                                                             :valid-click-locs []
                                                             :checker-obj (new Ellipse2D$Double (first cp) 
                                                                               (second cp) circ-dim circ-dim)
                                                             :clicked false
                                                             :king false})})) 
                    br)))

(defn king-me [{[ind {chk :checker
                        {[team _] :team
                         king :king} :checker :as entry}] :entry
                         read-board :board}] 
  (if (and (false? king) (contains? (king-ind team) ind))
    (let [nc (assoc entry :checker (assoc chk :king true))
          nb (assoc read-board ind nc)]
      {:board nb :entry [ind nc]})
    {:board read-board :entry [ind entry]}))

(def move-func {:team2 [(fn [ind] (- ind 9)) (fn [ind] (- ind 7))]
                :team1 [(fn [ind] (+ ind 9)) (fn [ind] (+ ind 7))]})

(def all-move-funcs 
  (flatten (map move-func (keys move-func))))

(defn valid-index? [ind] 
  (and (< 0 ind) (< ind num-squares)))

(defn move [ind read-board] 
  (when (and (valid-index? ind) (= black (((read-board ind) :square) :color)))
    (read-board ind)))

(defn filter-nil [data]
  (set (filter some? data)))

(defn check-square [[_ {chk :checker
                        :as square} :as entry]]
  (when (and (some? square) (nil? chk))
    entry))

(defn gen-simple-moves [[ind {chk :checker 
                          :as square} :as entry] funcs read-board]
    (set (map (fn [ind]
                (check-square [ind (move ind read-board)])) 
              (map 
                (fn [mf] 
                  (mf ind)) funcs))))

(defn simple-paths [[ind {chk :checker
                          {[team _] :team
                           king :king} :checker} :as entry] read-board]
    (let [norm-moves (gen-simple-moves entry (move-func team) read-board)
          res (if king
                (union norm-moves (gen-simple-moves entry all-move-funcs read-board))
                norm-moves)]
      (if (= res #{nil})
             nil
             (filter-nil res))))

(defn jump [[si {sc :checker 
                {[st _] :team} :checker} :as se] 
            [mi {mc :checker} :as me]
            [ei {ec :checker} :as ee]]
  (when (and (some? se) (some? me) (some? ee) (some? mc) (not= st (first (mc :team))) (nil? ec))
    [me ee]))

(defn jump-squares [start-index df read-board] 
  (let [mid-index (df start-index)
        end-index (df mid-index)
        res (filter some? (filter (fn [x] 
                                    (when (some? (second x)) 
                                      x))
                          [[mid-index (move mid-index read-board)] 
                           [end-index (move end-index read-board)]]))]
    (when (= 2 (count res))
      res)))

(defn gen-jump-moves [[ind {{[team _] :team} :checker} 
                       :as entry] funcs read-board] 
  (filter-nil (set (map (fn [mf]
                          (let [js (jump-squares ind mf read-board)]
                            (jump entry (first js) (second js)))) funcs))))

(defn jump-paths [[ind {chk :checker 
                           {[team _] :team
                            king :king} :checker} :as entry] read-board]
  (let [norm-moves (gen-jump-moves entry (move-func team) read-board)]
    (if king
      (filter-nil (union norm-moves 
                         (gen-jump-moves entry all-move-funcs read-board)))
      norm-moves)))

(defn remove-checker [[ci entry] read-board]
  (assoc read-board ci (assoc entry :checker nil)))

(defn move-checker [{[chk-ind {{team :team} :checker :as checker}] :from
                     [sq-ind {{[sqx sqy] :point} :square :as square}] :to}
                    read-board]
  (let [[cpx cpy] (checker-point sqx sqy)
        move-chk (assoc (checker :checker) 
                        :point [cpx cpy]
                        :clicked false
                        :team team
                        :checker-obj (new Ellipse2D$Double cpx cpy circ-dim circ-dim))
        sq-entry (assoc square :checker move-chk)]
    {:board (assoc (assoc read-board chk-ind (assoc checker :checker nil)) 
                        sq-ind 
                        sq-entry)
     :entry [sq-ind sq-entry]}))

(defn ajp [[chk-ind {chk :checker 
                          {[team _] :team} :checker 
                          :as square} :as entry] {curr-ajp :ajp
                                                  crb :read-board
                                                  c-ajp-i :hi
                                                  base-ind :bi
                                                  :as res} jumps]
  (if (some? (first jumps))
    (let [ni (inc c-ajp-i)
          nce (assoc (curr-ajp base-ind) :next (conj ((curr-ajp base-ind) :next) ni))
          new-ajp (assoc (conj curr-ajp {:path (first jumps) :next #{}})
                         base-ind nce)
          {nrb :board
           ne :entry} (move-checker {:from entry :to (last (first jumps))} 
                                        (remove-checker (first (first jumps)) crb))]
      (let [{n-ajp :ajp new-hi :hi} (ajp ne {:ajp new-ajp :bi ni :hi ni :read-board nrb} (jump-paths ne nrb))]
        (ajp entry {:ajp n-ajp :bi base-ind :hi new-hi :read-board crb} (rest jumps))))
    res))

(defn all-jump-paths [checker read-board]
  (let [jps (jump-paths checker read-board)
        fe {:path [checker] :next #{}}
        ajp-val ((fn base-move [jumps {c-ajp :ajp crb :read-board chi :hi :as res}]
					         (if (some? (first jumps))
					           (let [ni (inc chi)
					                 temp-ajp (conj c-ajp {:path (first jumps) :next #{}})
					                 nfe (assoc (c-ajp 0) :next (conj ((c-ajp 0) :next) ni))
					                 n-ajp (assoc temp-ajp 0 nfe)
					                 {nrb :board ne :entry} (move-checker {:from checker :to (last (first jumps))} 
					                                                               (remove-checker (first (first jumps)) crb))
					                 {ajp-res :ajp nrb :read-board hi :hi} (ajp ne {:ajp n-ajp :read-board nrb :hi ni :bi ni} 
					                                                            (jump-paths ne nrb))]
					             (base-move (rest jumps) {:ajp ajp-res
					                                      :read-board read-board
					                                      :hi hi}))
					           c-ajp)) jps {:ajp (conj [] fe) :read-board read-board :hi 0})]
    (if (= ajp-val [fe])
      nil
      ajp-val)))

(defn valid-simple-move? [{from :from
                           to :to} sps read-board]
   (and (not= from to) (reduce #(or %1 %2) 
                               (map (fn [e]
                                     (= to e)) (if (nil? sps)
                                                 #{sps}
                                                 sps)))))

(defn valid-jump-move? [{from :from
                        to :to} ajp read-board]
  (when (some? ajp) 
    ((fn find-index [indicies]
       (when (some? (first indicies))
         (if (= to (last ((ajp (first indicies)) :path)))
           (first indicies)
           (find-index (rest indicies))))) ((first ajp) :next))))

(defn paths [checker read-board]
  {:simple-paths (simple-paths checker read-board)
   :all-jump-paths (all-jump-paths checker read-board)})

(defn valid-move? [{from :from
                     to :to :as move} read-board]
  (if (and (some? from) (some? to))
	  (let [{sps :simple-paths 
          ajps :all-jump-paths} (paths from read-board)
	        valid-jump (valid-jump-move? move ajps read-board)
	        valid-simple (valid-simple-move? move sps read-board)]
	    {:simple-paths sps :all-jump-paths ajps :valid-move (or (some? valid-jump) valid-simple)
	     :ajp-move-index valid-jump})
	  false))

(defn checkers [team read-board] 
  (set (filter some? (map #((fn [{{[chk-t _] :team} :checker :as entry} tm] 
                                                     (when (= chk-t tm)
                                                       entry)) % team) read-board))))

(defn get-moves [mov-chks move-key]
  (filter some? (map (fn [[chk {mv move-key}]]
                                      (when (some? mv)
                                        [chk {move-key mv}])) mov-chks)))

(defn movable-checkers [team read-board]
  (set (filter some? (map-indexed #((fn [ind {{[t _] :team} :checker :as entry} tm]
                                      (let [paths (paths [ind entry] read-board)]
                                        (when (and (= t tm) (not= (set (vals paths)) #{nil})) 
                                          [[ind entry] paths]))) %1 %2 team) read-board))))

(defn get-simples [mov-chks]
  (get-moves mov-chks :simple-paths))

(defn get-jumps [mov-chks]
  (get-moves mov-chks :all-jump-paths))

(defn valid-moves [team read-board]
	(let [mov-chks (movable-checkers team read-board)
	      jumps (get-jumps mov-chks)]
	  (if (empty? jumps)
	    (get-simples mov-chks)
	    jumps)))

;ui depends on this way
(def tie-state (atom {:board [] :team-counts #{} :times 0}))

(defn n-move-tie [{cb :board tcs :team-counts t :times} nb]
        (let [ntcs (set [(count (checkers :team1 nb)) 
                         (count (checkers :team2 nb))])]
          (if (= ntcs tcs)
            {:board cb :team-counts tcs :times (inc t)}
            {:board nb :team-counts ntcs :times 1})))

(defn game-over
  ([read-board tie-st] (game-over read-board tie-st default-tie-limit))
  ([read-board tie-st tie-limit] (let [chk-t1 (count (checkers :team1 read-board))
                                       mv-t1 (count (movable-checkers :team1 read-board))
                                       chk-t2 (count (checkers :team2 read-board))
                                       mv-t2 (count (movable-checkers :team2 read-board))]
                                   (let [t (n-move-tie tie-st read-board)]
                                     {:board-result (cond
                                                     (or (and (= 0 mv-t1 mv-t2) (= chk-t1 chk-t2))
                                                         (>= (t :times) tie-limit)) :tie
                                                         (or (zero? chk-t1) (zero? mv-t1)) :team2
                                                         (or (zero? chk-t2) (zero? mv-t2)) :team1)
                                      :tie t}))))

(defn exec-jump [{from :from to :to jumped-chk :jumped-chk} read-board]
  ((king-me (move-checker {:from from :to to} (remove-checker jumped-chk read-board))) :board))

(defn exec-multiple-jumps [[j :as jumps] read-board]
  (if (some? j)
    (exec-multiple-jumps (rest jumps) (exec-jump j read-board))
    read-board))

(defn invoke-action [[{jc :jumped-chk} :as action] read-board]
  (if (some? jc)
    (exec-multiple-jumps action read-board)
    ((king-me (move-checker (first action) read-board)) :board)))

(defn clear-repl []
  (map #(ns-unmap *ns* %) (keys (ns-interns *ns*))))

