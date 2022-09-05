(ns sicp.queens)

(def empty-board [])

(defn adjoin-position [new-row k rest-of-queens]
  (cons new-row rest-of-queens))

(defn safe? [k positions]
  (let [candidate (first positions)]
    (loop [top (dec candidate)
           bot (inc candidate)
           [check & more] (rest positions)]
      (cond
        (nil? check) true
        (= check candidate) false
        (= check top) false
        (= check bot) false
        :else (recur (dec top) (inc bot) more)))))

(defn queens [board-size]
  (letfn [(queen-cols [k]
            (if (= k 0)
              (list empty-board)
              (filter (fn [positions] (safe? k positions))
                      (mapcat
                       (fn [rest-of-queens]
                         (map (fn [new-row]
                                (adjoin-position new-row k rest-of-queens))
                              (range 1 (inc board-size))))
                       (queen-cols (dec k))))))]
    (queen-cols board-size)))

(comment

  (count (queens 8))

  (queens 4)

  ;; => ((3 1 4 2) (2 4 1 3))

  (queens 5)
  ;; => ((4 2 5 3 1)
  ;;     (3 5 2 4 1)
  ;;     (5 3 1 4 2)
  ;;     (4 1 3 5 2)
  ;;     (5 2 4 1 3)
  ;;     (1 4 2 5 3)
  ;;     (2 5 3 1 4)
  ;;     (1 3 5 2 4)
  ;;     (3 1 4 2 5)
  ;;     (2 4 1 3 5))

  .)
