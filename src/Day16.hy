(require hyrule *)
(import networkx :as nx)

(setv lines (with [o (open "./rsc/day16.txt")]
  (as-> o it (.read it) (.strip it) (.split it "\n"))))

(setv dirs #( 1 -1 1j -1j))
(setv g (.DiGraph nx))

(for [#(i r) (enumerate lines)]
  (for [#(j c) (enumerate r)]
    (when (= c "#") (continue))
    (setv z (+ i (* 1j j)))
    (when (= c "S") (setv start #(z 1j)))
    (when (= c "E") (setv stop z))
    (for [d dirs] (g.add-node #(z d)))))

(for [#(z d) (. g nodes)]
  (when (in #((+ z d) d) (. g nodes)) (g.add-edge #(z d) #((+ z d) d) :weight 1))
  (for [r [-1j 1j]] (g.add-edge #(z d) #(z (* d r)) :weight 1000)))

(for [d dirs] (g.add-edge #(stop d) "stop" :weight 0))

(setv silver (nx.shortest-path-length g start "stop" :weight "weight"))

(setv gold (len (sfor p (nx.all-shortest-paths g start "stop" :weight "weight")
                      #(z _) (get p (slice None -1)) z)))

(print (.format "Silver: {}" silver))
(print (.format "Gold:   {}" gold))
