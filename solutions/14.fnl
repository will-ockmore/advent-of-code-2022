(local fennel (require :lib.fennel))
(local lume (require :lib.lume))

(local utils (require :solutions.utils))

(fn read-input []
  (icollect [line (io.lines :./solutions/inputs/14.txt)]
    (icollect [j i (string.gmatch line "(%d+),(%d+)")]
      [(tonumber i) (tonumber j)])))

(fn transform-coords [input]
  (let [min-j-before-transform (accumulate [max 0 _ [i j] (ipairs input)]
                                 (if (< j min-j) j min-j))
        j-offset (- min-j 1)
        max-j-before-transform (accumulate [max 0 _ [i j] (ipairs input)]
                                 (if (< max-j j) j max-j))
        max-i (accumulate [max 0 _ [i j] (ipairs input)]
                (if (< max-i i) i max-i))
        coords (icollect [_ [i j] (ipairs input)]
                 [(+ i 1) (- j j-offset)])
        source [1 (- 500 j-offset)]
        max-j (- max-j-before-transform j-offset)]
    {: coords : source : max-i : max-j}))

(fn draw-rocks [input]
  (let [{: coords : max-i : max-j}]))

(fn part-1 []
  (fennel.view (-> (read-input))))

(fn part-2 []
  (fennel.view (-> (read-input))))

{: part-1 : part-2}
