(ns ttt.game
  (:require [ttt.board :as board]
            [ttt.strategies :as strg]))

(defn game-over-with-winner? [board]
  (or (board/check-lines board)
      (board/check-columns board)
      (board/check-diagonals board)))

(defn game-over-with-remise? [board]
  (-> board flatten board/has-valid-positions? not))

(defn- play [board l]
  (println "playing..")
  ;todo implement me)

(defn- start-game [n m l]
  (let [game-board (board/create-empty-board n m)]
    (play game-board l)))

(defn -main
  "Start game"
  [& args]
  (println "Start tic-tac-toe")
  (start-game 3 3 3))
