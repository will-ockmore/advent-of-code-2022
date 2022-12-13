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

(fn print-move [head-x head-y tail-x tail-y tail-moves]
  (let [min-x (- head-x grid-size)
        max-x (+ head-x grid-size)
        min-y (- head-y grid-size)
        max-y (+ head-y grid-size)
        rel-tail-x (- tail-x head-x)
        rel-tail-y (- tail-y head-y)
        serialized-moves (lume.map tail-moves serialize-move)]
    (var output "")
    (for [y max-y min-y -1]
      (var line "")
      (for [x min-x max-x]
        (set line
             (.. line
                 (if (and (= x head-x) (= y head-y)) (.. bold :H bold-off)
                     (and (= x tail-x) (= y tail-y)) (.. bold :T bold-off)
                     (lume.find serialized-moves
                                (fennel.view [(lume.round x) (lume.round y)]))
                     "#"
                     (and (= (% x (/ grid-size 2)) 0)
                          (= (% y (/ grid-size 2)) 0))
                     "┼" (= (% y (/ grid-size 2)) 0) "─"
                     (= (% x (/ grid-size 2)) 0) "│" " "))))
      (set output (.. output line "\n")))
    (io.write output)))

(fn calculate-moves [input]
  (var head-x 0)
  (var head-y 0)
  (var tail-x 0)
  (var tail-y 0)
  (var tail-moves [])
  (var head-moves [])
  (each [_ [direction moves] (ipairs input)]
    ;; Run the moves and update position if required
    (for [n 1 moves] 
      ;; First move head
      (match direction
        :R (set head-x (+ head-x 1))
        :L (set head-x (- head-x 1))
        :U (set head-y (+ head-y 1))
        :D (set head-y (- head-y 1)))
      ;; Then move tail if required. Transform to coords centered on tail
      (let [rel-x (- head-x tail-x)
            rel-y (- head-y tail-y)
            distance (lume.distance 0 0 rel-x rel-y)
            angle (lume.angle 0 0 rel-x rel-y)
            radians (/ angle math.pi)]
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
                        (set tail-y (- tail-y 1))))))))
      ;; Store every location of tail
      (table.insert tail-moves [tail-x tail-y])
      (table.insert head-moves [head-x head-y])
      (when show-moves
        (os.execute :clear)
        (print-move head-x head-y tail-x tail-y tail-moves)
        (io.flush)
        (os.execute "sleep 0.3"))))
  tail-moves)

(fn part-1 []
  (fennel.view (-> (read-input) (calculate-moves) (lume.map serialize-move)
                   (lume.unique) (length))))

(fn part-2 []
  (fennel.view (->> (read-input))))

{: part-1 : part-2}
