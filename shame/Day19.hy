(require hyrule *)

(setv #(towels patterns)
  (with [o (open "./rsc/day19-example.txt")]
    (let [i (-> o (.read ) (.split "\n\n"))
          t (-> i (get 0) (.strip) (.split ", "))
          p (-> i (get 1) (.split))]
      #(t p))))

(defn cp [p ts [m {}]]
  (cond (= p "") 1
        (in p m) (get m p)
        True     (let [tot (sum (lfor t ts :if (p.startswith t)
                                      (cp (get p (slice (len t) None)) ts m)))]
                    (print (.format "{} : {}" p tot))
                    (setv (get m p) tot)
                    tot)))

(defn solve [ts ps]
  (let [tp (lfor p ps (let [c (cp p ts)] #((> c 0) c)))
        _ (print tp)
        possible (sum (map (fn [a] (get a 0)) tp))
        total    (sum (map (fn [a] (get a 1)) tp))]
    #(possible total)))

(setv #(silver gold) (solve towels patterns))

(print (.format "Silver: {}" silver))
(print (.format "Gold:   {}" gold))
