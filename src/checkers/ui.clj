;As with all journeys, it begins with a simple willingness, with an abiding faith in the unknown.
(ns ui.core)

(import 
 '(java.awt Color Graphics Dimension BorderLayout Shape)
 '(java.awt.image BufferedImage)
 '(java.awt.geom Rectangle2D$Double Ellipse2D$Double)
 '(javax.swing JPanel JFrame JTextArea)
 '(java.awt.event MouseAdapter MouseEvent))

;for using doc function!
(use 'clojure.repl) 
(use 'clojure.set)

(def scale 100)
(def dim 80)
(def num-squares 64)
(def team-color {:team1 (. Color yellow)
                 :team2 (. Color magenta)})
(def king-ind {:team1 #{56 58 60 62}
               :team2 #{1 3 5 7}})
(def circ-dim (/ scale 2))
(def circ-hl (+ circ-dim 9))
(def shift (/ circ-dim 2))
(def king-dim (/ circ-dim 2.4))
(def t1-rows (set (range 0 3)))
(def t2-rows (set (range 5 8)))
(def checker-rows (clojure.set/union t1-rows t2-rows))
(def black (. Color black))
(def red (. Color red))
(def green (. Color green))
(def cyan (. Color cyan))

(defn hl-shift [cp] (- cp 4))
(defn king-shift [cp] (+ cp 14))

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
                                                           :square-obj (new Rectangle2D$Double x y scale scale)}
                                                 :checker (when (and (contains? checker-rows r) (= c black))
                                                            {:point cp
                                                             :team (if (contains? t1-rows r)
                                                                    [:team1 (. Color yellow)]
                                                                    [:team2 (. Color magenta)]) 
                                                             :valid-click-locs []
                                                             :checker-obj (new Ellipse2D$Double (first cp) 
                                                                               (second cp) circ-dim circ-dim)
                                                             :clicked false
                                                             :king false})})) 
                    br)))
(def board (atom (gen-board 0 [])))

(defn color-frame [g read-board]
  (let [img (new BufferedImage (* scale dim) (* scale dim) 
         (. BufferedImage TYPE_INT_ARGB))
        im-graph (. img (getGraphics))]
       (.setColor im-graph (. Color white))
       (.fillRect im-graph 0 0 (. img (getWidth)) (. img (getHeight)))
       ((fn draw-board [[{square :square
                 {[sqx sqy] :point} :square
                 checker :checker} :as eles]]
          (when (not (empty? eles))  
              (.setColor im-graph (square :color))
              (.fillRect im-graph sqx sqy scale scale)
              (when (and (= (square :color) black) (some? checker) (some? (checker :team)))
                (let [cx (first (checker :point))
                      cy (second (checker :point))]
                  (when (checker :clicked)
                    (.setColor im-graph green)
                    (.fillOval im-graph (hl-shift cx) (hl-shift cy) circ-hl circ-hl))
                  (.setColor im-graph (second (checker :team)))
                  (.fillOval im-graph cx cy circ-dim circ-dim)
                  (when (checker :king)
                    (.setColor im-graph cyan)
                    (.fillOval im-graph (king-shift cx) (king-shift cy) king-dim king-dim))))
            (draw-board (rest eles)))) read-board)
       (. g (drawImage img 0 0 nil))
       (. im-graph (dispose))))

;proxy implements/extends a interface/class where the supplied arguments
;are arguments to the class' super constructor and then calls
;the supplied functions
(def panel (doto (proxy [JPanel] []
                        (paint [g] (color-frame g @board)))
             (.setPreferredSize (new Dimension 
                                     (/ (* scale dim) 5) 
                                     (/ (* scale dim) 5)))))

(defn get-board [] @board)

(defn king-me [[ind {chk :checker
                   {[team _] :team
                    king :king} :checker :as entry}] read-board] 
  (if (and (false? king) (contains? (king-ind team) ind))
    (assoc read-board ind  
           (assoc entry :checker (assoc chk :king true)))
    read-board)) 

(def move-func {:team2 [(fn [ind] (- ind 9)) (fn [ind] (- ind 7))]
                :team1 [(fn [ind] (+ ind 9)) (fn [ind] (+ ind 7))]})

(def all-move-funcs 
  (flatten (map move-func (keys move-func))))

(defn valid-index? 
  "performs a bounds check on ind"
  [ind] 
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
    (let [norm-moves (gen-simple-moves entry (move-func team) read-board)]
      (if king
        (union norm-moves (gen-simple-moves entry all-move-funcs read-board))
        norm-moves)))

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

(defn jump-paths [[ind {chk :checker 
                      {[team _] :team} :checker 
                      :as square} :as entry] read-board]
  (let [[left right] (move-func team)
        left-jump-sqs (jump-squares ind left read-board)
        left-jump (jump entry (first left-jump-sqs) (second left-jump-sqs))
        right-jump-sqs (jump-squares ind right read-board)
        right-jump (jump entry (first right-jump-sqs) (second right-jump-sqs))]
    {:left left-jump
     :right right-jump}))

(defn gen-jump-moves [[ind {{[team _] :team} :checker} 
                       :as entry] funcs read-board] 
  (filter-nil (set (map (fn [mf]
                          (let [js (jump-squares ind mf read-board)]
                            (jump entry (first js) (second js)))) funcs))))

(defn jump-paths-new [[ind {chk :checker 
                           {[team _] :team
                            king :king} :checker} :as entry] read-board]
  (let [norm-moves (gen-jump-moves entry (move-func team) read-board)]
    (if king
      (filter-nil (union norm-moves 
                         (gen-jump-moves entry all-move-funcs read-board)))
      norm-moves)))

(defn new-key [key c-func]
  (keyword (str "p" (c-func key))))

(defn update-prev-next [key [lk rk] paths]
  (let [prev-key (new-key key dec)
        {prev-next :next :as prev-entry} (paths prev-key)]
    (if (some? prev-entry)
      (assoc paths prev-key (assoc prev-entry :next
                                   (set (filter some? (clojure.set/union prev-next #{lk} #{rk})))))
      paths)))

(defn add-next [key key-func nks paths]
  (let [c-key (new-key key key-func)
        {next :next :as entry} (paths c-key)]
    (if (some? entry)
      (assoc paths c-key (assoc entry :next
                                   (set (filter some? (clojure.set/union next nks)))))
      paths)))

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
    {:read-board (assoc (assoc read-board chk-ind (assoc checker :checker nil)) 
                        sq-ind 
                        sq-entry)
     :new-entry [sq-ind sq-entry]}))

(defn get-keys [num-keys key-bag]
  (let [ks (take num-keys key-bag)]
    [ks (difference key-bag (set ks))]))

(defn get-key-bag [num-keys ub]
  (set (repeatedly num-keys #(rand-int ub))))

(defn hl-checker [read-board] (first 
                                (filter some? 
                                        (map-indexed (fn [index {ele :checker
                                                                 {clicked :clicked} :checker :as entry}] 
                                                       (when (and (some? ele) clicked)
                                                         [index entry])) read-board))))

(defn ajp-new3 [[ind {chk :checker 
                      {[team _] :team} :checker 
                      :as square} :as entry] read-board curr-key key-bag res jps expl-set]
  (cond
    (some? (first jps)) (let [[[p1k] nkb] (get-keys 1 key-bag)
                              p1kw (keyword (str "p" p1k))
                              p1-entry {:path (first jps) :next #{}}
		                          new-res (assoc (add-next curr-key (fn [x] x) #{p1k} res) p1kw p1-entry)
		                          {p1-rb :read-board
			                          p1-ne :new-entry} (move-checker {:from entry :to (last (first jps))} read-board)
                              [p1kb p2kb] (get-keys 10 nkb)
                              ji (first (second (first jps)))
                              new-es (conj expl-set ind)]
                          (if (contains? new-es ji)
                            (ajp-new3 entry read-board curr-key p2kb new-res (rest jps) new-es)
                            (merge (ajp-new3 p1-ne p1-rb p1k (set p1kb) new-res (jump-paths-new p1-ne p1-rb) new-es) 
                                   (ajp-new3 entry read-board curr-key p2kb new-res (rest jps) new-es))))
       :else 
        res))


(defn starting-keys [paths]
  (difference (set (keys paths)) ((fn keys-in-ns [ps res] 
										                    (let [[_ pm] (first ps)]
										                      (if (nil? pm)
										                      res
										                      (keys-in-ns (rest ps) 
										                           (union res (pm :next)))))) paths #{})))

(defn all-jump-paths [checker read-board]
  (let [[[fk] kb] (get-keys 1 (get-key-bag 100 (* 100 100)))
        ;paths-old (ajp checker read-board fk kb {})
        paths (ajp-new3 checker read-board fk kb {} (jump-paths-new checker read-board) #{})]
  (assoc paths :start {:path checker 
                       :next (starting-keys paths)})))

(defn valid-simple-move? [{from :from
                           to :to} sps read-board]
   (and (not= from to) (reduce #(or %1 %2) 
                               (map (fn [e]
                                     (= to e)) sps))))

(defn valid-jump-move? [{from :from
                        to :to} {{next :next} :start :as jp} read-board]
        (if (some? jp)
          (first (filter some? (map (fn [key]
                                      (when (= (last ((key jp) :path)) to)
                                        key)) next)))
          nil))

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
	     :ajp-move-key valid-jump})
	  false))

(defn remove-jumped-chks [{jp :all-jump-paths
                           move-key :ajp-move-key :as move-data} read-board]
  (if (some? move-key)
	    (let [[jci jc] (first ((jp move-key) :path))]
       (assoc read-board jci (assoc jc :checker nil)))
	    read-board))

(defn remove-checker [[ci entry] read-board]
  (assoc read-board ci (assoc entry :checker nil)))

;(load-file "/home/tanderson/git/checkers/src/checkers/ui.clj")

(def ml (proxy [MouseAdapter] []
          (mouseClicked [mouse-event] 
            (let [read-board (get-board)
                  find-clicked (fn [index ele key shapeKey]
                                 (when (and (some? (ele key)) (some? ((ele key) shapeKey)) 
                                             (. ((ele key) shapeKey) (contains 
                                                                       (. mouse-event (getX)) 
                                                                       (. mouse-event (getY)))))
                                       [index ele]))
                  hl-c (hl-checker read-board)
                  clicked-square (first (filter #(and (some? %) (= (((second %) :square) :color) (. Color black)))
                                                           (map-indexed #(find-clicked %1 %2 :square :square-obj) read-board)))
                  clicked-checker (first (filter some? (map-indexed #(find-clicked %1 %2 :checker :checker-obj) read-board)))
                  update-clicked (fn [ele val] (when (some? ele)
                                                 (reset! board (assoc @board (first ele) 
                                                                      (assoc (second ele) :checker 
                                                                             (assoc ((second ele) :checker) :clicked val))))))
                  {move? :valid-move :as move-data} (valid-move? {:from hl-c
                                                                      :to clicked-square} read-board)]
              (if move?
                (reset! board (let [{board :read-board 
                                     moved-entry :new-entry} (move-checker {:from hl-c :to clicked-square} 
                                                                           (remove-jumped-chks move-data read-board))]
                                (king-me moved-entry board)))
                (do
                    (update-clicked hl-c false)
                    (update-clicked clicked-checker true)))
                (. panel (repaint))))))

(defn frame [] (doto 
                 (new JFrame) 
                 (-> (.getContentPane) (.add panel) (.addMouseListener ml))
                 .pack 
                 .show))