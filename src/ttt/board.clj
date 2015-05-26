(ns ttt.board
    (:use [ttt.matrix]))

(defn create-empty-board [n m]
    (create-empty-matrix n m))

(defn count-markers [coll marker]
  (count (filter #(= marker %) coll)))

(defn has-valid-positions? [coll]
  (> (count-markers coll nil) 0))

(defn find-valid-position [coll]
  "Non-greedy search"
  (let [coll-size (count coll)]
    (loop [crt-coll coll
           idx 0]
      (let [elem (first crt-coll)]
        (if (nil? elem)
          idx
          (if (= idx coll-size)
            nil
            (recur (rest crt-coll) (inc idx))))))))

(defn check-coll [coll marker length]
  (= length (count-markers coll marker)))

(defn- check-coll-winner [coll]
  (if (apply = coll) (first coll) nil))

(defn check-lines [board]
  (loop [first-line (first board)
         other-lines (rest board)]
    (if (nil? first-line)
      nil
      (or (check-coll-winner first-line)
          (recur (first other-lines)
                 (rest other-lines))))))

(defn check-columns [board]
  (check-lines (transpose board)))

(defn check-diagonals [board]
  (or (check-coll-winner (get-diagonal board 0 0))
      (check-coll-winner (get-diagonal (transpose-lines board) 0 0))))

(defn put-marker [board line-idx col-idx marker]
  (assoc-in board [line-idx col-idx] marker))

(defn put-marker-in-line [board line-idx line marker]
  (assoc-in board [line-idx] (replace {nil marker} line)))
