(local fennel (require :lib.fennel))
(local lume (require :lib.lume))

(local utils (require :solutions.utils))

(fn read-input []
  (icollect [line (io.lines :./solutions/inputs/10.txt)]
    (let [(op value) (string.match line "(%a+)%s?(%-?%d*)")]
      [op (tonumber value)])))

(fn process-instructions [input]
  ;; Store the value of the X register per cycle
  (var cycles [[1 1]])
  (each [_ instruction (ipairs input)]
    (match instruction
      [:addx n] (do
                  (table.insert cycles
                                [(. (lume.last cycles) 2)
                                 (. (lume.last cycles) 2)])
                  (table.insert cycles
                                [(. (lume.last cycles) 2)
                                 (+ n (. (lume.last cycles) 2))]))
      [:noop]
      (table.insert cycles [(. (lume.last cycles) 2) (. (lume.last cycles) 2)])))
  (lume.slice cycles 2))

(fn sum-interesting-signals [cycles]
  (accumulate [sum 0 i [v _] (ipairs cycles)]
    (if (= (% (+ i 20) 40) 0)
        (+ sum (* i v))
        sum)))

(fn draw-crt [cycles]
  (each [i [v _] (ipairs cycles)]
    (let [position (% i 40)
          sprite (+ (% v 40) 1)] 
      (if (< (math.abs (- sprite position)) 2) (io.write "#") (io.write "."))
      (when (= position 0)
        (io.write "\n")))))

(fn part-1 []
  (fennel.view (-> (read-input) (process-instructions)
                   (sum-interesting-signals))))

(fn part-2 []
  (fennel.view (->> (read-input) (process-instructions) (draw-crt))))

{: part-1 : part-2}
