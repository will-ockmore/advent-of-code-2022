(local fennel (require :lib.fennel))
(local lume (require :lib.lume))

(local utils (require :solutions.utils))

(fn read-input []
  (icollect [line (io.lines :./solutions/inputs/12.txt)]
    (icollect [letter _ (string.gmatch line "(%a)")]
      letter)))

(fn convert-input [input]
  ;; Convert to numbers and store start and end
  (var start nil)
  (var end nil)
  (var heightmap (icollect [i row (ipairs input)]
                   (icollect [j letter (ipairs row)]
                     (match letter
                       :S (do
                            (set start [i j])
                            (string.byte :a))
                       :E (do
                            (set end [i j])
                            (string.byte :z))
                       _ (string.byte letter)))))
  {: start : end : heightmap})

(fn a*-rank [node]
  (+ (. node :distance) (. node :distance-to-end)))

(fn push [queue element-to-insert]
  "Insert element in order"
  (var inserted false)
  (each [i el (ipairs queue) :until inserted]
    (when (and (>= (a*-rank el) (a*-rank element-to-insert))
               (or (= i 1)
                   (<= (a*-rank (. queue (- i 1))) (a*-rank element-to-insert))))
      (table.insert queue i element-to-insert)
      (set inserted true)))
  (when (not inserted)
    (table.insert queue element-to-insert)))

(fn find-node [queue i j]
  (var node nil)
  (var node-index nil)
  (each [index curr (ipairs queue) :until node]
    (let [[k m] (. curr :coords)]
      (when (and (= i k) (= j m))
        (set node curr)
        (set node-index index))))
  [node node-index])

(fn pop [queue index]
  (table.remove queue (or index 1)))

(fn manhattan-distance [start end]
  (let [[i j] start
        [k m] end]
    (+ (- k i) (- m j))))

(fn find-shortest-path-a* [{: heightmap : start : end}]
  "A* algorithm to traverse from top left to bottom right of matrix"
  (let [q []
        visited {}
        max-i (length heightmap)
        max-j (length (. heightmap 1))
        [start-i start-j] start
        [end-i end-j] end]
    ;; Insert the start node
    (var current-node {:distance 0
                       :distance-to-end (manhattan-distance [start-i start-j]
                                                            [end-i end-j])
                       :coords [start-i start-j]
                       :from-coords nil})
    (table.insert q current-node)
    (while (not (. visited (fennel.view [end-i end-j])))
      (let [[i j] (. current-node :coords)
            nodes-to-visit (icollect [_ [k m] (ipairs [[(- i 1) j]
                                                       [i (- j 1)]
                                                       [(+ i 1) j]
                                                       [i (+ j 1)]])]
                             (when (and (< 0 k) (< 0 m) (>= max-i k)
                                        (>= max-j m)
                                        (not (. visited (fennel.view [k m])))
                                        ;; Can only go at most one heigher, but any amount lower
                                        (<= (. heightmap k m)
                                            (+ 1 (. heightmap i j))))
                               [k m]))]
        (each [_ [k m] (ipairs nodes-to-visit)]
          (let [node {:distance (+ (. current-node :distance) 1)
                      :distance-to-end (manhattan-distance [k m] [end-i end-j])
                      :coords [k m]
                      :from-coords [i j]}
                [prev-entry prev-entry-index] (find-node q k m)]
            (when (> (or (?. prev-entry :distance) math.huge)
                     (. node :distance))
              (when prev-entry
                (pop q prev-entry-index))
              (push q node))))
        (tset visited (fennel.view [i j]) current-node))
      (set current-node (pop q)))
    ;; Walk path backwards to start node
    (local path [])
    (set current-node (. visited (fennel.view [end-i end-j])))
    (while (. current-node :from-coords)
      (table.insert path current-node)
      (set current-node (. visited (fennel.view (. current-node :from-coords)))))
    {: path : heightmap : start : end}))

(fn find-closest-a-location [{: heightmap : end}]
  ;; Observing the input, I can see that only the first column of locations
  ;; contains a values that are next to a b value; therefore the path must start
  ;; from the left edge
  {:path (accumulate [best-path nil i _ (ipairs heightmap)]
               (let [{: path} (find-shortest-path-a* {: heightmap :start [i 1] : end})]
                 (if (and best-path (< (length best-path) (length path))) best-path path)))})

(local bold "\027[1m")
(local bold-off "\027[\021m")
(local faint "\027[2m")
(local faint-off "\027[\022m")

(fn print-path [{: path} input]
  "Display path superimposed on map"
  (print "")
  (each [i row (ipairs input)]
    (var line "")
    (each [j letter (ipairs row)]
      (local is-path (< 0 (length (find-node path i j))))
      (set line (.. line
                    (if is-path (.. bold letter bold-off)
                        (.. faint letter faint-off)))))
    (set line (.. line "\n"))
    (io.write line))
  (print "")
  path)

(fn part-1 []
  (let [input (read-input)]
    (fennel.view (-> (convert-input input) (find-shortest-path-a*)
                     (print-path input) (length)))))

(fn part-2 []
  (let [input (read-input)]
    (fennel.view (-> (convert-input input) (find-closest-a-location)
                     (print-path input) (length)))))

{: part-1 : part-2}
