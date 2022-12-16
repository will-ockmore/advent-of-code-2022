(local fennel (require :lib.fennel))
(local lume (require :lib.lume))

(local utils (require :solutions.utils))

(fn read-input []
  (icollect [line (io.lines :./solutions/inputs/15.txt)]
    (let [(sensor-x sensor-y beacon-x beacon-y)
                    (string.match line
                                  "x=(-?%d+),%sy=(-?%d+).*x=(-?%d+),%sy=(-?%d+)")]
      {: sensor-x : sensor-y : beacon-x : beacon-y})))


(fn manhattan-distance [start end]
  (let [[i j] start
        [k m] end]
    (+ (- k i) (- m j))))


;; See https://www.wolframalpha.com/input?i=solve+abs%28y-j%29+%2B+abs%28x-i%29+%3D+M+for+i

(fn values-for-x2 [beacon-x beacon-y row-y distance] 
  (let [
        y-distance (math.abs (- row-y beacon-y))


        ]
    []))


(fn part-1 []
  (fennel.view (-> (read-input))))

(fn part-2 []
  (fennel.view (->> (read-input))))

{: part-1 : part-2}
