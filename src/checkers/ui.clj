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
(def circ-dim (/ scale 2))
(def circ-hl (+ circ-dim 9))
(def shift (/ circ-dim 2))
(def t1-rows (set (range 0 3)))
(def t2-rows (set (range 5 8)))
(def checker-rows (clojure.set/union t1-rows t2-rows))
(def black (. Color black))
(def red (. Color red))

(defn hl-shift [cp] (- cp 4))

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
                                                             :clicked false})})) 
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
                    (.setColor im-graph (. Color green))
                    (.fillOval im-graph (hl-shift cx) (hl-shift cy) circ-hl circ-hl))
                  (.setColor im-graph (second (checker :team)))
                  (.fillOval im-graph cx cy circ-dim circ-dim)))
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

(def move-func {:team2 [(fn [ind] (- ind 9)) (fn [ind] (- ind 7))]
                :team1 [(fn [ind] (+ ind 9)) (fn [ind] (+ ind 7))]})

(defn valid-index? 
  "performs a bounds check on ind"
  [ind] 
  (and (< 0 ind) (< ind num-squares)))

(defn move [ind read-board] 
  (when (and (valid-index? ind) (= black (((read-board ind) :square) :color)))
    (read-board ind)))

(defn check-square [[_ {chk :checker
                        :as square} :as entry]]
  (when (and (some? square) (nil? chk))
    entry))

(defn simple-paths [[ind {chk :checker 
                      {[team _] :team} :checker 
                      :as square} :as entry] read-board]
  (let [[left right] (move-func team)
        left-ent [(left ind) (move (left ind) read-board)]
        right-ent [(right ind) (move (right ind) read-board)]]
    {:left (check-square left-ent) :right (check-square right-ent)}))

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
    (if (= 2 (count res))
           res
           nil)
    ;could be a when here
    ))

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

(defn new-key [key c-func]
  (keyword (str "p" (c-func key))))

(defn look-back [key]
  (if (< (dec key) 0)
    (new-key key (fn [x] x))
    (new-key key dec)))

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
;                    
;                    [chk-ind {{team :team} :checker :as checker}] 
;                    [sq-ind {{[sqx sqy] :point} :square :as square}] 
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

(defn ajp [[ind {chk :checker 
                      {[team _] :team} :checker 
                      :as square} :as entry] read-board curr-key key-bag res]
  (let [{left :left right :right} (jump-paths entry read-board)]
    (cond
	     (and (some? left) (some? right)) (let [[[nlk nrk] nkb] (get-keys 2 key-bag)
                                               [lk rk] [(keyword (str "p" nlk)) (keyword (str "p" nrk))]
                                               left-entry {:path left :next #{}}
																			         {left-rb :read-board
																			          nlc :new-entry} (move-checker {:from entry :to (last left)} read-board)
																			         right-entry {:path right :next #{}}
																			         {right-rb :read-board
																			          nrc :new-entry} (move-checker {:from entry :to (last right)} read-board)
                                                new-res (add-next curr-key (fn [x] x) #{lk rk} res)]
	                                     (let [left-res (ajp nlc left-rb nlk nkb (assoc new-res lk left-entry))
	                                           right-res (ajp nrc right-rb nrk nkb (assoc new-res rk right-entry))]
	                                          (merge left-res right-res)))
	     (some? left) (let [[[nlk] nkb] (get-keys 1 key-bag)
                          lk (keyword (str "p" nlk))
                          left-entry {:path left :next #{}}
									        new-res (add-next curr-key (fn [x] x) #{lk} res)
									        {left-rb :read-board
									         nlc :new-entry} (move-checker {:from entry :to (last left)} read-board)]
	                    (ajp nlc left-rb nlk nkb (assoc new-res lk left-entry)))
	     (some? right) (let [[[nrk] nkb] (get-keys 1 key-bag)
                           rk (keyword (str "p" nrk))
                           right-entry {:path right :next #{}}
									         new-res (add-next curr-key (fn [x] x) #{rk} res)
									         {right-rb :read-board
									          nrc :new-entry} (move-checker {:from entry :to (last right)} read-board)]
									                    (ajp nrc right-rb nrk nkb (assoc new-res rk right-entry)))
	     :else res)))

(defn starting-keys [paths]
  (difference (set (keys paths)) ((fn keys-in-ns [ps res] 
										                    (let [[_ pm] (first ps)]
										                      (if (nil? pm)
										                      res
										                      (keys-in-ns (rest ps) 
										                           (union res (pm :next)))))) paths #{})))

(defn all-jump-paths [checker read-board]
  (let [key-bag (get-key-bag 100 (* 100 100))
        [[fk] kb] (get-keys 1 key-bag)
        paths (ajp checker read-board fk kb {})]
  (assoc paths :start {:path checker 
                       :next (starting-keys paths)})))

(defn valid-simple-move? [{from :from
                        to :to} read-board]
  (let [{left :left right :right} (simple-paths from read-board)]
    (and (not= from to) (or (= to left) (= to right)))))

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
	  (let [{{left :left right :right} :simple-paths :as sps ajps :all-jump-paths} (paths from read-board)
	        valid-jump (valid-jump-move? move ajps read-board)
	        valid-simple (valid-simple-move? move read-board)]
	    {:simple-paths sps :all-jump-paths ajps :valid-move (or (some? valid-jump) valid-simple)
	     :move-type (cond 
	                  (some? valid-jump) :all-jump-paths
	                  valid-simple :simple-paths
	                  :else nil) :ajp-move-key valid-jump})
	  false))

(defn remove-jumped-chks [{jp :all-jump-paths
                           move-key :ajp-move-key :as move-data} read-board]
  (if (some? move-key)
	    (let [v (println "jp: " jp)
            v (println "move-key: " move-key)
            v (println "(jp move-key): " (jp move-key))
            [jci jc] (first ((jp move-key) :path))]
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
                (do
                (reset! board ((move-checker {:from hl-c
                                              :to clicked-square} 
                                             (remove-jumped-chks move-data read-board)) :read-board))
;                (reset! board ((move-checker {:from hl-c
;                                              :to clicked-square} read-board) :read-board))
                )
                (do
                    (update-clicked hl-c false)
                    (update-clicked clicked-checker true)))
              
              

;                (if move?
;                  (reset! board ((move-checker hl-c clicked-square read-board) :read-board))
;                  (do
;                    (update-clicked hl-c false)
;                    (update-clicked clicked-checker true)))
                (. panel (repaint))))))

(defn frame [] (doto 
                 (new JFrame) 
                 (-> (.getContentPane) (.add panel) (.addMouseListener ml))
                 .pack 
                 .show))