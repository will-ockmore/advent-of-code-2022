(local fennel (require :lib.fennel))
(local lume (require :lib.lume))

(local utils (require :solutions.utils))

(fn read-input []
  (icollect [line (io.lines :./solutions/inputs/8.txt)]
    (icollect [tree _ (string.gmatch line "(%d)")]
      (tonumber tree))))

(fn tree-visible [i j trees]
  "A tree is visible in a direction if all trees in that direction until the edge are smaller"
  (var visible {:up true :down true :left true :right true})
  ;; Check up
  (for [n (- i 1) 1 -1]
    (when (>= (. trees n j) (. trees i j))
      (tset visible :up false)))
  ;; Check down
  (for [n (+ i 1) (length trees)]
    (when (>= (. trees n j) (. trees i j))
      (tset visible :down false)))
  ;; Check left
  (for [n (- j 1) 1 -1]
    (when (>= (. trees i n) (. trees i j))
      (tset visible :left false)))
  ;; Check right
  (for [n (+ j 1) (length (. trees i))]
    (when (>= (. trees i n) (. trees i j))
      (tset visible :right false)))
  (or (. visible :up) (. visible :down) (. visible :left) (. visible :right)))

(fn count-visible [trees]
  (accumulate [sum 0 i row (ipairs trees)]
    (+ sum (accumulate [sumn 0 j _ (ipairs row)]
             (if (tree-visible i j trees) (+ sumn 1) sumn)))))

(fn tree-visible-score [i j trees]
  "A tree is visible in a direction if all trees in that direction until the edge are smaller"
  (var visible {:up 0 :down 0 :left 0 :right 0})
  ;; Check up
  (for [n (- i 1) 1 -1 :until (> (. visible :up) 0)]
    (when (or (>= (. trees n j) (. trees i j)) (= n 1))
      (tset visible :up (- i n))))
  ;; Check down
  (for [n (+ i 1) (length trees) :until (> (. visible :down) 0)]
    (when (or (>= (. trees n j) (. trees i j)) (= n (length trees)))
      (tset visible :down (- n i))))
  ;; Check left
  (for [n (- j 1) 1 -1 :until (> (. visible :left) 0)]
    (when (or (>= (. trees i n) (. trees i j)) (= n 1))
      (tset visible :left (- j n))))
  ;; Check right
  (for [n (+ j 1) (length (. trees i)) :until (> (. visible :right) 0)]
    (when (or (>= (. trees i n) (. trees i j)) (= n (length (. trees i))))
      (tset visible :right (- n j))))
  (* (. visible :up) (. visible :down) (. visible :left) (. visible :right)))

(fn max-scenic-score [trees]
  (var max 0)
  (each [i row (ipairs trees)]
    (each [j _ (ipairs row)]
      (let [score (tree-visible-score i j trees)]
        (if (> score max) (set max score)))))
  max)

(fn part-1 []
  (fennel.view (->> (read-input) (count-visible))))

(fn part-2 []
  (fennel.view (->> (read-input) (max-scenic-score))))

{: part-1 : part-2 : read-input : tree-visible-score : max-scenic-score}
