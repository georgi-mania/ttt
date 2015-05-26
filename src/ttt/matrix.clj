(ns ttt.matrix)

(defn- create-empty-line [m]
  (vec (take m (repeat nil))))

(defn create-empty-matrix
  "Creates a matrix of size n x m"
  [n m]
  (vec (repeat n (create-empty-line m))))

(defn get-n-size
  "Get number of lines for a matrix"
  [matrix]
  (count matrix))

(defn get-m-size
  "Get number of columns for a matrix"
  [matrix]
  (count (first matrix)))

(defn get-element-at-pos [matrix line-idx col-idx]
  (-> matrix
      (nth line-idx)
      (nth col-idx)))

(defn- get-elements-from-column [matrix col-idx]
  (map #(get-element-at-pos matrix % col-idx) (range 0 (get-n-size matrix))))

(defn transpose [matrix]
  (loop [column-idx 0
         transposed-matrix []]
    (let [column (get-elements-from-column matrix column-idx)]
      (if (= column-idx (get-m-size matrix))
        transposed-matrix
        (recur (inc column-idx)
               (conj transposed-matrix (vec column)))))))

(defn transpose-lines [matrix]
  (loop [line (first matrix)
         other-lines (rest matrix)
         transposed-matrix []]
    (if (nil? line)
      transposed-matrix
      (recur (first other-lines)
             (rest other-lines)
             (conj transposed-matrix (-> line reverse vec))))))

(defn get-diagonal
  "Retrieves the elements from a diagonal starting from a given position in matrix represented by (line-idx, col-idx)"
  [matrix line-idx col-idx]
  (loop [i line-idx
         j col-idx
         diag []]
    (if (or (= i (get-n-size matrix))
            (= j (get-m-size matrix)))
      diag
      (recur (inc i)
             (inc j)
             (conj diag (get-element-at-pos matrix i j))))))
