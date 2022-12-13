(local fennel (require :lib.fennel))
(local lume (require :lib.lume))

(local utils (require :solutions.utils))

(fn read-input []
  (icollect [line (io.lines :./solutions/inputs/9.txt)]
    (let [(direction moves) (string.match line "(%a) (%d+)")]
      [direction (tonumber moves)])))

(local bold "\027[1m")
(local bold-off "\027[\021m")
(local grid-size 20)
(local show-moves false)

(fn serialize-move [move]
  (fennel.view [(lume.round (. move 1)) (lume.round (. move 2))]))

(fn print-move [rope]
  (let [head (. rope 1)
        tail (. rope (length rope))
        [head-x head-y] (. head (length head))
        min-x (- head-x grid-size)
        max-x (+ head-x grid-size)
        min-y (- head-y grid-size)
        max-y (+ head-y grid-size)
        serialized-moves (lume.map tail serialize-move)
        current-positions (-> (lume.map rope lume.last)
                              (lume.map serialize-move))]
    (var output "")
    (for [y max-y min-y -1]
      (var line "")
      (for [x min-x max-x]
        (set line
             (let [current-move (serialize-move [x y])
                   knot (lume.find current-positions current-move)
                   tail-history (lume.find serialized-moves current-move)]
               (.. line
                   (if (and (= x head-x) (= y head-y)) (.. bold :H bold-off)
                       knot (- knot 1) tail-history "#"
                       (and (= (% x (/ grid-size 2)) 0)
                            (= (% y (/ grid-size 2)) 0))
                       "┼" (= (% y (/ grid-size 2)) 0) "─"
                       (= (% x (/ grid-size 2)) 0) "│" " ")))))
      (set output (.. output line "\n")))
    (io.write output)))

(fn move-tail [target tail]
  (let [[target-x target-y] target
        [x y] tail
        rel-x (- target-x x)
        rel-y (- target-y y)
        distance (lume.distance 0 0 rel-x rel-y)
        angle (lume.angle 0 0 rel-x rel-y)
        radians (/ angle math.pi)]
    (var tail-x x)
    (var tail-y y)
    ;; Only move if more than one space away
    (if (> distance 1)
        (do
          (if (= rel-x 0) ;; Up or down
              (set tail-y (+ tail-y (/ rel-y 2)))
              (= rel-y 0) ;; Left or right
              (set tail-x (+ tail-x (/ rel-x 2))))
          ;; Move diagonally only if more than 1 diagonal move away
          (if (> distance (math.sqrt 2))
              (if (and (> radians 0) (< radians 0.5))
                  ;; Diagonal up and right
                  (do
                    (set tail-x (+ tail-x 1))
                    (set tail-y (+ tail-y 1)))
                  (and (> radians 0.5) (< radians 1))
                  ;; Diagonal up and left
                  (do
                    (set tail-x (- tail-x 1))
                    (set tail-y (+ tail-y 1)))
                  (and (> radians -1) (< radians -0.5))
                  ;; Diagonal down and left
                  (do
                    (set tail-x (- tail-x 1))
                    (set tail-y (- tail-y 1)))
                  (and (> radians -0.5) (< radians 0))
                  ;; Diagonal down and right
                  (do
                    (set tail-x (+ tail-x 1))
                    (set tail-y (- tail-y 1)))))))
    [tail-x tail-y]))

(fn calculate-moves [input rope]
  (each [_ [direction moves] (ipairs input)]
    ;; Run the moves and update position if required
    (for [n 1 moves]
      ;; First move head
      (let [head-moves (. rope 1)
            [head-x head-y] (. head-moves (length head-moves))]
        (match direction
          :R (table.insert (. rope 1) [(+ head-x 1) head-y])
          :L (table.insert (. rope 1) [(- head-x 1) head-y])
          :U (table.insert (. rope 1) [head-x (+ head-y 1)])
          :D (table.insert (. rope 1) [head-x (- head-y 1)])))
      ;; Then move tail if required. Transform to coords centered on tail
      (each [i knot (ipairs (lume.slice rope 2))]
        (let [prev (. rope i)
              [prev-x prev-y] (lume.last prev)
              [current-x current-y] (lume.last knot)
              [x y] (move-tail [prev-x prev-y] [current-x current-y])]
          (table.insert (. rope (+ i 1)) [x y]))))
    (when show-moves
      (os.execute :clear)
      (print-move rope)
      (io.flush)
      (os.execute "sleep 0.3")))
  (. rope (length rope)))


(fn part-1 []
  (fennel.view (-> (read-input) (calculate-moves [[[0 0]] [[0 0]]])
                   (lume.map serialize-move) (lume.unique) (length))))

(fn part-2 []
  (fennel.view (-> (read-input)
                   (calculate-moves [[[0 0]]
                                     [[0 0]]
                                     [[0 0]]
                                     [[0 0]]
                                     [[0 0]]
                                     [[0 0]]
                                     [[0 0]]
                                     [[0 0]]
                                     [[0 0]]
                                     [[0 0]]])
                   (lume.map serialize-move) (lume.unique) (length))))

{: part-1 : part-2}
