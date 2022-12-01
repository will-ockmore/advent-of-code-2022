(local fennel (require :lib.fennel))
(local lume (require :lib.lume))

(local utils (require :solutions.utils))

(fn read-input []
  (icollect [line (io.lines :./solutions/inputs/1.txt)]
    (icollect [calories _ (string.gmatch line "(%d+)")]
      (tonumber calories))))

(fn gather-reindeer-calories [calories-list]
  (let [reindeer []]
    (var current-reindeer [])
    (each [_ v (ipairs calories-list)]
      (ipairs calories-list)
      (if (= (length v) 0)
        (do (table.insert reindeer (accumulate [sum 0 i n (ipairs current-reindeer)]
                                            (+ sum (. n 1))))
          (set current-reindeer []))
        (table.insert current-reindeer v)))
    reindeer))

(fn top-3 [sorted-calories] (+  (. sorted-calories (length sorted-calories))
                                (. sorted-calories (- (length sorted-calories) 1))
                                (. sorted-calories (- (length sorted-calories) 2))))

(fn part-1 []
  (fennel.view (-> (read-input) (gather-reindeer-calories) (utils.max))))

(fn part-2 []
  (fennel.view (-> (read-input) (gather-reindeer-calories) (utils.sorted) (top-3))))

{: part-1 : part-2}
