(local fennel (require :lib.fennel))
(local json (require :lib.json))
(local lume (require :lib.lume))

(local utils (require :solutions.utils))

(fn read-input []
  (icollect [line (io.lines :./solutions/inputs/18.txt)]
    (icollect [x (string.gmatch line "(%d+)")]
      (tonumber x))))

(fn initialise-space [min-x max-x min-y max-y min-z max-z]
  (let [space []]
    (for [x min-x (+ max-x 2)]
      (var row [])
      (for [y min-y (+ max-y 2)]
        (var col [])
        (for [z min-z (+ max-z 2) ]
          (table.insert col false))
        (table.insert row col))
      (table.insert space row))
    space))

(fn fill-space [input]
  (let [[[min-x max-x] [min-y max-y] [min-z max-z]] (accumulate [[[min-x max-x]
                                                                  [min-y max-y]
                                                                  [min-z max-z]] [[(. input
                                                                                                                                                       1
                                                                                                                                                       1)
                                                                                                                                                    (. input
                                                                                                                                                       1
                                                                                                                                                       1)]
                                                                                                                                                   [(. input
                                                                                                                                                       1
                                                                                                                                                       2)
                                                                                                                                                    (. input
                                                                                                                                                       1
                                                                                                                                                       2)]
                                                                                                                                                   [(. input
                                                                                                                                                       1
                                                                                                                                                       3)
                                                                                                                                                    (. input
                                                                                                                                                       1
                                                                                                                                                       3)]] _ [x
                                                                                                                                                                                                                                                                                                                 y
                                                                                                                                                                                                                                                                                                                 z] (ipairs input)]
                                                      [[(if (< x min-x) x min-x)
                                                        (if (< max-x x) x max-x)]
                                                       [(if (< y min-y) y min-y)
                                                        (if (< max-y y) y max-y)]
                                                       [(if (< z min-z) z min-z)
                                                        (if (< max-z z) z max-z)]])
        space (initialise-space min-x max-x min-y max-y min-z max-z)]
    (each [_ [x y z] (ipairs input)]
      (tset space (+ x 1) (+ y 1) (+ z 1) true))
    space))

(fn count-exposed-sides [space]
  (var exposed-sides 0)
  (each [x row (ipairs space)]
    (each [y col (ipairs row)]
      (each [z v (ipairs col)]
        (if v
            (set exposed-sides
                 (+ exposed-sides (if (?. space (+ x 1) y z) 0 1)
                    (if (?. space (- x 1) y z) 0 1)
                    (if (?. space x (+ y 1) z) 0 1)
                    (if (?. space x (- y 1) z) 0 1)
                    (if (?. space x y (+ z 1)) 0 1)
                    (if (?. space x y (- z 1)) 0 1)))))))
  exposed-sides)

(fn count-inner-exposed-sides [space]
  (var exposed-sides 0)
  (each [x row (ipairs space)]
    (each [y col (ipairs row)]
      (each [z v (ipairs col)]
        (if (= v true)
            (set exposed-sides
                 (+ exposed-sides (if (= :water (?. space (+ x 1) y z)) 1 0)
                    (if (= :water (?. space (- x 1) y z)) 1 0)
                    (if (= :water (?. space x (+ y 1) z)) 1 0)
                    (if (= :water (?. space x (- y 1) z)) 1 0)
                    (if (= :water (?. space x y (+ z 1))) 1 0)
                    (if (= :water (?. space x y (- z 1))) 1 0)))))))
  exposed-sides)

(fn has-exposed-sides [space [x y z]]
  (or (not (?. space (+ x 1) y z)) (not (?. space (- x 1) y z))
      (not (?. space x (+ y 1) z)) (not (?. space x (- y 1) z))
      (not (?. space x y (+ z 1))) (not (?. space x y (- z 1)))))

(fn count-outer-surface [space total [x y z]]
  (let [point (. space x y z)
        left-coords [(+ x 1) y z]
        right-coords [(- x 1) y z]
        up-coords [x (+ y 1) z]
        down-coords [x (- y 1) z]
        forward-coords [x y (+ z 1)]
        back-coords [x y (- z 1)]
        left (?. space (+ x 1) y z)
        right (?. space (- x 1) y z)
        up (?. space x (+ y 1) z)
        down (?. space x (- y 1) z)
        forward (?. space x y (+ z 1))
        back (?. space x y (- z 1))
        new-total (if (and point (not (= point :visited)))
                      (+ total (if left 0 1) (if right 0 1) (if up 0 1)
                         (if down 0 1) (if forward 0 1) (if back 0 1))
                      total)
        next-location (if point
                          (if (and (has-exposed-sides space left-coords)
                                   (not (= left :visited)) left)
                              [(+ x 1) y z]
                              (and (has-exposed-sides space right-coords)
                                   (not (= right :visited)) right)
                              [(- x 1) y z]
                              (and (has-exposed-sides space up-coords)
                                   (not (= up :visited)) up)
                              [x (+ y 1) z]
                              (and (has-exposed-sides space down-coords)
                                   (not (= down :visited)) down)
                              [x (- y 1) z]
                              (and (has-exposed-sides space forward-coords)
                                   (not (= forward :visited)) forward)
                              [x y (+ z 1)]
                              (and (has-exposed-sides space back-coords)
                                   (not (= back :visited)) back)
                              [x y (- z 1)])
                          ;; If this is not an occupied point move centrally
                          [(+ x 1) (+ y 1) (+ z 1)])]
    (tset space x y z :visited)
    (if (not next-location) (do
                              (print (fennel.view next-location))
                              (print (length (. space 1 1)))
                              (print (fennel.view left))
                              (print (fennel.view right))
                              (print (fennel.view up))
                              (print (fennel.view down))
                              (print (fennel.view left))
                              (print (fennel.view right))
                              new-total)
        (count-outer-surface space new-total next-location))))

(fn flood-fill [space]
  ;; Starting in the top corner, fill the space, creating a "cast" of the droplet
  (fn fill [[x y z]]
    ;; Fill current location and move to the next available
    (tset space x y z :water)
    (let [
          ;; A valid direction is one which has not been filled, and is also not occupied by the droplet
          left (if (= false (?. space (+ x 1) y z)) [(+ x 1) y z])
          right (if (= false (?. space (- x 1) y z)) [(- x 1) y z])
          up (if (= false (?. space x (+ y 1) z)) [x (+ y 1) z])
          down (if (= false (?. space x (- y 1) z)) [x (- y 1) z])
          forward (if (= false (?. space x y (+ z 1))) [x y (+ z 1)])
          back (if (= false (?. space x y (- z 1))) [x y (- z 1)])]
      (if left (fill left))
      (if right (fill right))
      (if up (fill up))
      (if down (fill down))
      (if forward (fill forward))
      (if back (fill back))))

  (fill [1 1 1])
  space)

(fn part-1 []
  (fennel.view (-> (read-input) (fill-space) (count-exposed-sides))))

(fn part-2 []
  ;; Incorrect - off by less than a percent on the real input, but works on the test input. Not sure why.
  (fennel.view (-> (read-input) (fill-space)  (flood-fill)
                   (count-inner-exposed-sides))))

{: part-1 : part-2 : flood-fill}
