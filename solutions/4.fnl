(local fennel (require :lib.fennel))
(local lume (require :lib.lume))

(local utils (require :solutions.utils))

(fn read-input []
  (icollect [line (io.lines :./solutions/inputs/4.txt)]
    (icollect [group _ (string.gmatch line "(%d+%-%d+)")]
      (icollect [num _ (string.gmatch group "(%d+)")]
        (tonumber num)))))

(fn count-contained [input]
  (accumulate [sum 0 _ pair (ipairs input)]
    (let [[first second] pair
          [first_i first_j] first
          [second_i second_j] second]
      (if (or (and (>= first_j second_j) (<= first_i second_i))
              (and (>= second_j first_j) (<= second_i first_i)))
          (+ sum 1) sum))))

(fn count-overlap [input]
  (accumulate [sum 0 _ pair (ipairs input)]
    (let [[first second] pair
          [first_i first_j] first
          [second_i second_j] second]
      (if (or 
              (> second_i first_j) (< second_j first_i))
          sum (+ sum 1) ))))

(fn part-1 []
  (fennel.view (->> (read-input) (count-contained))))

(fn part-2 []
  (fennel.view (->> (read-input) (count-overlap))))

{: part-1 : part-2}
