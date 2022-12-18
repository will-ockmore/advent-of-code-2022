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
                   (tset acc valve {: rate : valves})
                   acc))]
    result))

(fn discover-paths [input]
  (print (fennel.view input))
  (let [cache {}]
    (fn find-best-path [start mins-remaining current-path]
      (let [{: rate : valves} (. input start)
            new-path (lume.concat current-path [start])
            already-visited (lume.find current-path start)
            destinations (lume.shuffle valves)]
        ;; Base case - if one minute remaining, just release pressure for 1 minute
        (if (<= mins-remaining 1)
            (do
              (tset cache (fennel.view [start mins-remaining]) rate) rate)
            ;; DFS
            (let [paths (icollect [_ valve (ipairs destinations)]
                          (let [cached (. cache
                                          (fennel.view [start mins-remaining]))]
                            (if cached cached
                                (find-best-path valve (- mins-remaining 1)
                                                new-path))))]
              ;; Only try opening the valve if the rate is greater than zero and we have not already visited
              (if (and (not (= rate 0)) (not already-visited))
                  (each [_ valve (ipairs destinations)]
                    (let [pressure (* rate mins-remaining)
                          new-mins-remaining (- mins-remaining 1)
                          cached (. cache
                                    (fennel.view [start new-mins-remaining]))
                          best-path-after-open-valve (if cached cached
                                                         (find-best-path valve
                                                                         new-mins-remaining
                                                                         new-path))]
                      (print (fennel.view cache))
                      (print mins-remaining start valve)
                      (table.insert paths
                                    (+ pressure best-path-after-open-valve)))))
              ;; The best path is the one which maximises the flow rate
              (let [best-path (accumulate [max 0 _ v (ipairs paths)]
                                (if (< max v) v max))]
                (tset cache (fennel.view [start mins-remaining]) best-path))))))

    (find-best-path :AA 30 [])))

(fn part-1 []
  (fennel.view (-> (read-input) (discover-paths))))

(fn part-2 []
  (print :hi))

{: part-1 : part-2}
