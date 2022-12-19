(local fennel (require :lib.fennel))
(local json (require :lib.json))
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
  (print (json.encode input))
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
    (each [i v (ipairs indicies)]
      (print (accumulate [s (.. v " ") _ d (ipairs (. distances i))]
               (.. s " " (if (= d math.huge) "∞" d)))))
    (print "")
    (each [k _ (ipairs distances)]
      (each [i _ (ipairs distances)]
        (each [j _ (ipairs distances)]
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
    (let [next-valve (?. path (+ i 1))
          new-time (- (. acc :time-remaining) 1)
          extra-pressure (* new-time (. input v :rate))
          pressure (+ extra-pressure (. acc :pressure))
          time-remaining (if next-valve
                             (- new-time (get-distance v next-valve))
                             new-time)]
      {: pressure : time-remaining})))

(fn get-permutations [tbl]
  (let [permutations []]
    ;; Heap's algorithm

    (fn find-permutations [k A]
      (if (= k 1) (table.insert permutations (fennel.view A))
          (do
            (find-permutations (- k 1) (lume.clone A))
            (for [i 1 k]
              (do
                (if (= (% k 2) 1)
                    (let [first (. A i)
                          second (. A k)]
                      (tset A i second)
                      (tset A k first))
                    (let [first (. A 1)
                          second (. A k)]
                      (tset A 1 second)
                      (tset A k first)))
                (find-permutations (- k 1) (lume.clone A)))))))

    (find-permutations (length tbl) tbl)
    ;; Remove duplicates
    (-> (lume.unique permutations) (lume.map fennel.eval))))

(fn discover-paths [{: index-map : distances : input}]
  (let [valves-with-nonzero-rate (-> (lume.keys input)
                                     (lume.filter #(< 0 (. input $1 :rate)))
                                     (lume.sort))
        permutations {}]
    (fn find-permutations [curr rest]
      (if (= (length rest) 0) (tset permutations (fennel.view curr) true)
          (each [i v (ipairs rest)]
            (find-permutations (lume.concat curr [v])
                               (lume.concat (lume.slice rest 1 (- i 1))
                                            (lume.slice rest (+ i 1)))))))

    (print (fennel.view valves-with-nonzero-rate))
    (find-permutations [] valves-with-nonzero-rate)
    permutations))

(fn find-best-pressure [paths]
  (accumulate [max 0 _ {: pressure} (ipairs paths)]
    (if (< max pressure) pressure max)))

(fn part-1 []
  (fennel.view (-> (read-input) (floyd-warshall-shortest-paths)
                   (discover-paths))))

(fn part-2 []
  (print :hi))

{: part-1 : part-2 : get-permutations}
