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
    (each [k _ (ipairs distances)]
      (each [i _ (ipairs distances)]
        (each [j _ (ipairs distances)]
          (let [direct (. distances i j)
                indirect (+ (. distances i k) (. distances k j))]
            (if (< indirect direct) (tset distances i j indirect))))))
    {: index-map : distances : input}))

(fn discover-paths [{: index-map : distances : input} minutes]
  (fn get-distance [start end]
    (. distances (. index-map start) (. index-map end)))

  (let [valves-with-nonzero-rate (-> (lume.keys input)
                                     (lume.filter #(< 0 (. input $1 :rate)))
                                     (lume.sort))
        seen {}
        paths []]
    (fn traverse [current time-remaining pressure closed-valves path]
      (let [new-time (- time-remaining 1)
            extra-pressure (* new-time (. input current :rate))
            pressure (+ extra-pressure pressure)
            seen-key (fennel.view [current new-time])
            seen-pressure (or (. seen seen-key) -1)]
        (if (< seen-pressure pressure)
            (do
              (tset seen seen-key pressure)
              (if (= (length closed-valves) 0)
                  (table.insert paths {: pressure : closed-valves : path}))
              (each [_ dest (ipairs closed-valves)]
                (let [time-at-dest (- new-time (get-distance current dest))]
                  (if (<= 1 time-at-dest)
                      (traverse dest time-at-dest pressure
                                (lume.filter closed-valves #(not (= $1 dest)))
                                (lume.concat path [dest]))
                      (table.insert paths {: pressure : closed-valves : path}))))))))

    (each [_ valve (ipairs valves-with-nonzero-rate)]
      (traverse valve (- minutes (get-distance :AA valve)) 0
                (lume.filter valves-with-nonzero-rate #(not (= $1 valve)))
                [:AA valve]))
    paths))

(fn discover-paths-2 [{: index-map : distances : input} minutes]
  (fn get-distance [start end]
    (. distances (. index-map start) (. index-map end)))

  (let [valves-with-nonzero-rate (-> (lume.keys input)
                                     (lume.filter #(< 0 (. input $1 :rate)))
                                     (lume.sort))
        valve-masks (accumulate [masks {} i valve (ipairs valves-with-nonzero-rate)]
                      (lume.merge masks {valve (lshift 1 i)}))
        seen {}
        result {}]
    (fn traverse [current time-remaining pressure closed-valves visited]
      (let [new-time (- time-remaining 1)
            extra-pressure (* new-time (. input current :rate))
            pressure (+ extra-pressure pressure)
            seen-key (fennel.view [current new-time])
            seen-pressure (or (. seen seen-key) -1)]
        (if (< seen-pressure pressure)
            (do
              (tset seen seen-key pressure)
              (if (= (length closed-valves) 0)
                  (tset result (tostring visited)
                        (if (< (or (. result (tostring visited)) -1) pressure)
                            pressure
                            (. result (tostring visited)))))
              (each [_ dest (ipairs closed-valves)]
                (let [time-at-dest (- new-time (get-distance current dest))]
                  (if (<= 1 time-at-dest)
                      (traverse dest time-at-dest pressure
                                (lume.filter closed-valves #(not (= $1 dest)))
                                (bor visited (. valve-masks dest)))
                      (tset result (tostring visited)
                            (if (< (or (. result (tostring visited)) -1)
                                   pressure)
                                pressure
                                (. result (tostring visited)))))))))))

    (each [_ valve (ipairs valves-with-nonzero-rate)]
      (traverse valve (- minutes (get-distance :AA valve)) 0
                (lume.filter valves-with-nonzero-rate #(not (= $1 valve))) 0))
    result))

(fn find-best-pressure [paths]
  (accumulate [max 0 _ {: pressure} (ipairs paths)]
    (if (< max pressure) pressure max)))

(fn find-best-pressure [paths]
  (accumulate [max 0 _ pressure (pairs paths)]
    (if (< max pressure) pressure max)))

(fn find-best-pressure-2 [result]
  (accumulate [max 0 visited pressure (pairs result)]
    (accumulate [local-max max e-visited e-pressure (pairs result)]
      (let [combined-pressure (+ pressure e-pressure)
            visited (tonumber visited)
            e-visited (tonumber e-visited)]
        (if (and (= 0 (band visited e-visited)) (< max combined-pressure))
            combined-pressure
            max)))))

(fn part-1 []
  (fennel.view (-> (read-input) (floyd-warshall-shortest-paths)
                   (discover-paths-2 30) (find-best-pressure))))

(fn part-2 []
  (fennel.view (-> (read-input) (floyd-warshall-shortest-paths)
                   (discover-paths-2 26) (find-best-pressure-2))))

{: part-1 : part-2}
