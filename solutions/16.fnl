(local fennel (require :lib.fennel))
(local lume (require :lib.lume))

(local utils (require :solutions.utils))

(fn read-input []
  (let [valves-list (icollect [line (io.lines :./solutions/inputs/16.txt)]
                      (let [(valve rate valves) (string.match line
                                                              "Valve (%u+).*rate=(%d+);.*valves? (.*)")
                            valves (icollect [dest (string.gmatch valves
                                                                  "(%u+)")]
                                     dest)]
                        {: valve :rate (tonumber rate) : valves}))
        result (accumulate [acc {} _ {: valve : rate : valves} (ipairs valves-list)]
                 (do
                   (tset acc valve {: rate : valves : valve})
                   acc))]
    result))

(fn floyd-warshall-shortest-paths [input]
  (let [indicies (icollect [k _ (pairs input)] k)
        graph (icollect [i _ (ipairs indicies)] (icollect [i _ (ipairs indicies)] math.inf) )]
    graph))



(fn discover-paths [input]
  (let [cache {}
        nonzero-valves (-> (lume.filter input #(< 0 (. $1 :rate)))
                           (lume.map :valve)
                           (lume.sort)
                           (fennel.view))]
    (fn find-best-path [start mins-remaining activated-valves]
      (let [{: rate : valves} (. input start)
            already-activated (lume.find activated-valves start)
            destinations valves]
        ;; Base case - if one minute remaining, or all valves activated, nothing can be improved
        (if (or (= (fennel.view (lume.sort activated-valves)) nonzero-valves)
                (<= mins-remaining 1)) 0 
            (let [paths (icollect [_ valve (ipairs destinations)]
                          (let [cached (. cache
                                          (fennel.view [valve
                                                        (- mins-remaining 1) (lume.sort activated-valves)]))]
                            (if cached cached
                                (find-best-path valve (- mins-remaining 1)
                                                activated-valves))))]
              ;; Only try opening the valve if the rate is greater than zero and we have not already visited
              (if (and (not (= rate 0)) (not already-activated))
                  (each [_ valve (ipairs destinations)]
                    (let [new-mins-remaining (- mins-remaining 2)
                          pressure (* rate (- mins-remaining 1))
                          cached (. cache
                                    (fennel.view [valve new-mins-remaining (lume.sort activated-valves)]))
                          best-path-after-open-valve (if cached cached
                                                         (find-best-path valve
                                                                         new-mins-remaining
                                                                         (lume.concat activated-valves
                                                                                      [start])))]
                      (table.insert paths
                                    (+ pressure best-path-after-open-valve)))))
              ;; The best path is the one which maximises the flow rate
              (let [best-path (accumulate [max 0 _ v (ipairs paths)]
                                (if (< max v) v max))]
                (tset cache (fennel.view [start mins-remaining (lume.sort activated-valves)]) best-path)
                best-path)))))

    (find-best-path :AA 30 [])
    ))

(fn part-1 []
  (fennel.view (-> (read-input) (discover-paths))))

(fn part-2 []
  (print :hi))

{: part-1 : part-2}
