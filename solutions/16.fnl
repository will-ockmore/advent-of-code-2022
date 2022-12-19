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
  (let [indicies (-> (icollect [k _ (pairs input)]
                       k)
                     (lume.sort))
        index-map (accumulate [acc {} i k (ipairs indicies)]
                    (do
                      (tset acc k i)
                      acc))
        distances (icollect [i _ (ipairs indicies)]
                    (icollect [i _ (ipairs indicies)]
                      math.huge))]
    (each [_ {: valves : valve} (pairs input)]
      (let [i (. index-map valve)]
        (each [_ v (ipairs valves)]
          (let [j (. index-map v)]
            (tset distances i j 1)))))
    (each [i _ (ipairs indicies)]
      (tset distances i i 0))
    (each [i _ (ipairs distances)]
      (each [j _ (ipairs distances)]
        (each [k _ (ipairs distances)]
          (let [direct (. distances i j)
                indirect (+ (. distances i k) (. distances k j))]
            (if (< indirect direct) (tset distances i j indirect))))))
    (each [i v (ipairs indicies)]
      (print (accumulate [s (.. v " ") _ d (ipairs (. distances i))]
               (.. s " " (if (= d math.huge) "∞" d)))))
    {: index-map : distances : input}))

(fn get-eventual-pressure [input distances index-map path time]
  (fn get-distance [start end]
    (. distances (. index-map start) (. index-map end)))

  (accumulate [acc {:pressure 0
                    :time-remaining (- time (get-distance :AA (. path 1)))} i v (ipairs path) :until (< (. acc
                                                                                                                              :time-remaining)
                                                                                                                           1)]
    (let [
next-valve (?. path (+ i 1))
          new-time (- (. acc :time-remaining) 1)
          extra-pressure (* new-time (. input v :rate))
          pressure (+ extra-pressure (. acc :pressure))
          time-remaining (if next-valve (- new-time (get-distance v next-valve)) new-time)]
      {: pressure : time-remaining})))

(fn discover-paths [{: index-map : distances : input}]
  (let [valves-with-nonzero-rate (-> (lume.keys input)
                                     (lume.filter #(< 0 (. input $1 :rate))))
        permutations []]
    (fn find-permutations [curr n]
      (if (= n 0) (table.insert permutations curr)
          (each [_ v (ipairs valves-with-nonzero-rate)]
            (if (not (lume.find curr v))
                (find-permutations (lume.concat curr [v]) (- n 1))))))

    (find-permutations [] (length valves-with-nonzero-rate))
    (icollect [_ path (ipairs permutations)]
      (get-eventual-pressure input distances index-map path 30))))

(fn find-best-pressure [paths]
  (accumulate [max 0 _ {: pressure} (ipairs paths)]
              (if (< max pressure) pressure max)
              ))

(fn part-1 []
  (fennel.view (-> (read-input) (floyd-warshall-shortest-paths)
                   (discover-paths) (find-best-pressure))))

(fn part-2 []
  (print :hi))

{: part-1 : part-2}
