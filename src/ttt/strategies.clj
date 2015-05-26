(ns ttt.strategies
  (:require [ttt.board :as board]
            [ttt.matrix :as matrix]))

(defn- other-marker [marker]
  (if (= marker "x") "y" "x"))

(defn- has-winning-position [coll marker length]
  (if (and (board/check-coll coll marker (dec length))
           (board/has-valid-positions? coll))
    (board/find-valid-position coll)))

(defn- determine-winning-position-from-line [board marker length]
  (loop [line (first board)
         other-lines (rest board)
         line-idx -1]
    (if (nil? line)
      nil
      (let [pos (has-winning-position line marker length)]
        (if (-> pos nil? not)
          [(inc line-idx) pos]
          (recur (first other-lines)
                 (rest other-lines)
                 (inc line-idx)))))))

(defn- determine-winning-position-from-colum [board marker length]
  (let [reversed-position (determine-winning-position-from-line (matrix/transpose board) marker length)]
    (if (-> reversed-position nil? not)
      (-> reversed-position reverse vec))))

(defn- determine-winning-position-from-diagonal [board marker length]
  (let [first-diagonal (matrix/get-diagonal board 0 0)
        second-diagonal (matrix/get-diagonal (matrix/transpose-lines board) 0 0)
        first-diag-idx (has-winning-position first-diagonal marker length)
        second-diag-idx (has-winning-position second-diagonal marker length)]
    (if (-> first-diag-idx nil? not)
      (vector first-diag-idx first-diag-idx)
      (if (-> second-diag-idx nil? not)
        (vector second-diag-idx (- length second-diag-idx 1))))))

(defn determine-optimistic-move [board marker length]
  (or (determine-winning-position-from-line board marker length)
       (determine-winning-position-from-colum board marker length)
       (determine-winning-position-from-diagonal board marker length)))

(defn determine-pesimistic-move [board marker length]
  (determine-optimistic-move  board (other-marker marker) length))
