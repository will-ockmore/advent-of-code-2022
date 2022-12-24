(local fennel (require :lib.fennel))
(local json (require :lib.json))
(local lume (require :lib.lume))

(local utils (require :solutions.utils))

(fn read-input []
  (let [contents (-> (assert (io.open :solutions/inputs/17.txt)) (: :read))]
    (icollect [char (string.gmatch contents ".")]
      char)))

;; Represent each row as a binary number
(local walls (bor (lshift 1 9) (lshift 1 1)))
(local shapes [[(bor (lshift 1 6) (lshift 1 5) (lshift 1 4) (lshift 1 3))]
               [(bor (lshift 1 5))
                (bor (lshift 1 6) (lshift 1 5) (lshift 1 4))
                (bor (lshift 1 5))]
               [(bor (lshift 1 6) (lshift 1 5) (lshift 1 4))
                (bor (lshift 1 4))
                (bor (lshift 1 4))]
               [(bor (lshift 1 6))
                (bor (lshift 1 6))
                (bor (lshift 1 6))
                (bor (lshift 1 6))]
               [(bor (lshift 1 6) (lshift 1 5))
                (bor (lshift 1 6) (lshift 1 5))]])

(local should-print? false)

(fn print-stack [stack shape shape-index]
        (os.execute "sleep 0.1")
  (os.execute "clear")

(let [newstack (lume.clone stack)]
                       (for [_ 1 8]
                         (table.insert newstack 0))
                       (each [i row (ipairs shape)]
                         (let [index (+ shape-index (- i 1))
                               stack-row (or (. stack index) 0)]
                           (tset newstack index (bor row stack-row))))
  (var row "")
  (let [output (accumulate [s "" i v (ipairs newstack)]
                 (do
                   (set row "|")
                   (for [n 2 8]
                     (set row
                          (.. 
                              (if (not (= 0 (band (lshift 1 n) v))) "#" ".") row)))
                   (set row (.. "|" row))
                   (.. row "\n" s)))]
    (io.write output))))

(fn simulate [input iterations]
  (var current-move 1)
  (let [stack [(bor (lshift 1 8) (lshift 1 7) (lshift 1 6) (lshift 1 5)
                    (lshift 1 4) (lshift 1 3) (lshift 1 2))]]
    (for [i 1 iterations]
      (var stopped? false)
      (var shape (lume.clone (. shapes (+ 1 (% (- i 1)  (length shapes) )))))
      (var shape-index (+ 4 (length stack)))
      (while (not stopped?)
        (when should-print? (print-stack stack shape shape-index))
        (var overlaps? false)
        ;; Move sideways
        (var new-shape [])
        (each [i row (ipairs shape) :until overlaps?]
          (let [index (+ shape-index (- i 1))
                stack-row (or (. stack index) 0)
                new-row (if (= ">" (. input current-move)) (rshift row 1)
                            (lshift row 1))]
            (set overlaps?
                 (or overlaps? (not (= 0 (band walls new-row)))
                     (not (= 0 (band stack-row new-row)))))
            (table.insert new-shape new-row)))
        (if (not overlaps?) (set shape new-shape))
        (set overlaps? false)
        (set current-move (+ 1 (% current-move  (length input))))
        ;; Move down
        (each [i row (ipairs shape) :until overlaps?]
          (let [index (+ shape-index (- i 2))
                stack-row (or (. stack index) 0)]
            (set overlaps? (or overlaps? (not (= 0 (band stack-row row)))))))
        (if overlaps?
            (do
              (set stopped? true)
              (each [i row (ipairs shape)]
                (let [index (+ shape-index (- i 1))
                      stack-row (or (. stack index) 0)]
                  (tset stack index (bor row stack-row)))))
            (set shape-index (- shape-index 1)))))
    (- (length stack) 1)))

(fn part-1 []
  (fennel.view (-> (read-input) (simulate 2022) )))

(fn part-2 []
  (fennel.view (-> (read-input))))

{: part-1 : part-2}
