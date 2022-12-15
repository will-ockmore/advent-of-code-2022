(local fennel (require :lib.fennel))
(local lume (require :lib.lume))

(local utils (require :solutions.utils))

(local animate false)
(local display-result false)

(fn read-input []
  (icollect [line (io.lines :./solutions/inputs/14.txt)]
    (icollect [j i (string.gmatch line "(%d+),(%d+)")]
      [(tonumber i) (tonumber j)])))

(fn transform-coords [input]
  (let [min-j-before-transform (accumulate [min math.huge _ row (ipairs input)]
                                 (let [new-min (accumulate [min-j math.huge _ [i
                                                                               j] (ipairs row)]
                                                 (if (< j min-j) j min-j))]
                                   (if (< new-min min) new-min min)))
        j-offset (- min-j-before-transform 1)
        max-j-before-transform (accumulate [max 0 _ row (ipairs input)]
                                 (let [new-max (accumulate [max-j 0 _ [i j] (ipairs row)]
                                                 (if (< max-j j) j max-j))]
                                   (if (< max new-max) new-max max)))
        max-i-before-transform (accumulate [max 0 _ row (ipairs input)]
                                 (let [new-max (accumulate [max-i 0 _ [i j] (ipairs row)]
                                                 (if (< max-i i) i max-i))]
                                   (if (< max new-max) new-max max)))
        coords (icollect [_ row (ipairs input)]
                 (icollect [_ [i j] (ipairs row)]
                   [(+ i 1) (- j j-offset)]))
        source [1 (- 500 j-offset)]
        max-i (+ max-i-before-transform 1)
        max-j (- max-j-before-transform j-offset)]
    {: coords : source : max-i : max-j}))

(fn draw-rocks [input]
  (let [screen []
        {: coords : max-i : max-j} input]
    ;; Draw the air
    (for [i 1 max-i]
      (var row [])
      (for [j 1 max-j]
        (table.insert row j "."))
      (table.insert screen row))
    ;; Draw the rocks
    (each [_ seq (ipairs coords)]
      (var [last-i last-j] (. seq 1))
      (each [_ [end-i end-j] (ipairs (lume.slice seq 2))]
        (for [i last-i end-i (if (< end-i last-i) -1 1)]
          (for [j last-j end-j (if (< end-j last-j) -1 1)]
            (tset screen i j "#")))
        (set [last-i last-j] [end-i end-j])))
    (lume.merge input {: screen})))

(fn add-floor [input]
  (let [{: screen : source} input
        space (do (var air []) (for [_ 1 200] (table.insert air ".")) air)
        [i j] source
        source [i (+ j (length space))]
        new-screen (icollect [_ row (ipairs screen)]
                     (lume.concat space row space))]
    (table.insert new-screen (icollect [_ _ (ipairs (. new-screen 1))]
                               "."))
    (table.insert new-screen (icollect [_ _ (ipairs (. new-screen 1))]
                               "#"))
    (lume.merge input {:screen new-screen : source})))


(fn print-screen [screen]
  (io.write (accumulate [out "" _ row (ipairs screen)]
              (.. out (accumulate [line "" _ item (ipairs row)]
                        (.. line item)) "\n")))
  (os.execute "sleep 0.01"))

(fn simulate-sand [input round]
  (let [{: screen : max-i : max-j :source [source-i source-j]} input
        round-one (= round :one)
        round-two (= round :two)]
    (var finished? false)
    (var sand-i 1)
    (var sand-j source-j)
    ;; Number of grains of sand that have come to rest
    (var sand-count 0)
    (while (not finished?)
      (tset screen sand-i sand-j :o)
      (when animate
       (os.execute :clear)
       (print-screen screen)
       )
      ;; First try to move down
      (if (and round-one (= sand-i max-i)) (set finished? true)
          (= "." (. screen (+ sand-i 1) sand-j))
          (do
            (tset screen sand-i sand-j ".")
            (set sand-i (+ sand-i 1))) ;; Try to move down and left
          (and round-one (= sand-j 1)) (set finished? true)
          (= "." (. screen (+ sand-i 1) (- sand-j 1)))
          (do
            (tset screen sand-i sand-j ".")
            (set sand-i (+ sand-i 1))
            (set sand-j (- sand-j 1))) ;; Try to move down and right
          (and round-one (= sand-j max-j)) (set finished? true)
          (= "." (. screen (+ sand-i 1) (+ sand-j 1)))
          (do
            (tset screen sand-i sand-j ".")
            (set sand-i (+ sand-i 1))
            (set sand-j (+ sand-j 1)))
          (and round-two (= sand-i source-i)) (do (set sand-count (+ sand-count 1) ) (set finished? true))
          (do
            (set sand-i 1)
            (set sand-j source-j)
            (set sand-count (+ sand-count 1)))))
    (when display-result (print-screen screen))
    sand-count))

(fn part-1 []
  (fennel.view (-> (read-input) (transform-coords) (draw-rocks) (simulate-sand :one))))

(fn part-2 []
  (fennel.view (-> (read-input) (transform-coords) (draw-rocks) (add-floor)
                   (simulate-sand :two))))

{: part-1 : part-2}
