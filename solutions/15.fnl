(local fennel (require :lib.fennel))
(local lume (require :lib.lume))

(local utils (require :solutions.utils))

(fn read-input []
  (icollect [line (io.lines :./solutions/inputs/15.txt)]
    (let [(sensor-x sensor-y beacon-x beacon-y) (string.match line
                                                              "x=(-?%d+),%sy=(-?%d+).*x=(-?%d+),%sy=(-?%d+)")]
      {:sensor-x (tonumber sensor-x)
       :sensor-y (tonumber sensor-y)
       :beacon-x (tonumber beacon-x)
       :beacon-y (tonumber beacon-y)})))

(fn manhattan-distance [start end]
  (let [[i j] start
        [k m] end]
    (+ (math.abs (- k i)) (math.abs (- m j)))))

(fn sensor-range-at-row [sensor-x sensor-y row-y distance]
  ;; Manhattan distance from centerpoint on x is the sensor range minus the y distance
  (let [y-distance (math.abs (- row-y sensor-y))
        x-distance (math.abs (- distance y-distance))
        first (- sensor-x x-distance)
        second (+ sensor-x x-distance)]
    [first second]))

(fn calculate-manhattan-distance [input]
  (icollect [_ entry (ipairs input)]
    (let [{: sensor-x : sensor-y : beacon-x : beacon-y} entry
          distance (manhattan-distance [sensor-x sensor-y] [beacon-x beacon-y])]
      (lume.merge entry {: distance}))))

(fn get-positions-for-line [input line-y]
  ;; An input can only influence the line if the manhattan-distance to its beacon is within range
  (accumulate [ranges [] _ entry (ipairs input)]
    (let [{: sensor-x : sensor-y : distance} entry
          is-in-range? (<= (math.abs (- sensor-y line-y)) (math.abs distance))]
      (if is-in-range?
          (let [range (sensor-range-at-row sensor-x sensor-y line-y distance)]
            (table.insert ranges {: range : sensor-x : sensor-y})))
      ranges)))

(fn merge-ranges [input]
  (let [ranges (-> (lume.map input #(. $1 :range))
                   (lume.sort #(< (. $1 1) (. $2 1))))]
    (accumulate [merged [(lume.first ranges)] _ range (ipairs (lume.slice ranges
                                                                          2))]
      (let [[start end] range
            [current-start current-end] (table.remove merged)]
        (if (<= start current-end)
            (if (< current-end end) (table.insert merged [current-start end])
                (table.insert merged [current-start current-end]))
            (do
              (table.insert merged [current-start current-end])
              (table.insert merged [start end])))
        merged))))

(fn count-positions [input line-y]
  (print (fennel.view input))
  (let [beacons (read-input)
        beacons-on-line (-> (lume.filter beacons #(= (. $1 :beacon-y) line-y))
                            (lume.map #(. $1 :beacon-x))
                            (lume.unique))]
    (accumulate [sum 0 _ [start end] (ipairs input)]
      (let [size (- (+ end 1) start)
            beacons-in-range (-> (lume.filter beacons-on-line
                                              #(<= start $1 end))
                                 (length))
            possible-points (- size beacons-in-range)]
        (print size possible-points beacons-in-range)
        (+ sum possible-points)))))

(local search-size 4000000)
(fn find-beacon [input]
  (var beacon nil)
  (for [n 0 search-size :until beacon]
    (let [ranges (-> (get-positions-for-line input n) (merge-ranges))]
      (each [_ [start end] (ipairs ranges) :until beacon]
        (if (not (<= start 0 search-size end))
            (if (not (< start 0)) (set beacon [(- start 1) n])
                (not (< search-size end)) (set beacon [(+ end 1) n]))))))
  beacon)

(local row 2000000)

(fn part-1 []
  (fennel.view (-> (read-input) (calculate-manhattan-distance)
                   (get-positions-for-line row) (merge-ranges)
                   (count-positions row))))

(fn part-2 []
  (fennel.view (-> (read-input) (calculate-manhattan-distance) (find-beacon))))

{: part-1 : part-2 : count-positions}
