(local fennel (require :lib.fennel))
(local lume (require :lib.lume))

(local utils (require :solutions.utils))

(fn read-input []
  (icollect [line (io.lines :./solutions/inputs/3.txt)]
    (let [midpoint (/ (length line) 2)]
      [(string.sub line 1 midpoint) (string.sub line (- midpoint))])))

(fn get-priority [letter]
  (let [byte (string.byte letter)]
    (if (< byte 97) (- byte 38) (- byte 96))))

(fn check-for-duplicates [first-compartment second-compartment]
  (accumulate [result nil letter _ (string.gmatch first-compartment "(%a)") :until result]
    (if (string.find second-compartment letter) (get-priority letter))))

(fn convert-to-priorities [input]
  (icollect [_ rucksack (ipairs input)]
    (let [[first second] rucksack]
      (check-for-duplicates first second))))

(fn read-input-2 []
  (var group [])
  (let [groups []]
    (each [i line (ipairs (utils.get-contents :./solutions/inputs/3.txt))]
      (table.insert group line)
      (if (= (% i 3) 0) (do
                          (table.insert groups group)
                          (set group []))))
    groups))

(fn group-priority [groups]
  (icollect [_ group (ipairs groups)]
    (let [counts (icollect [_ v (ipairs group)]
                   (utils.counter (icollect [letter _ (string.gmatch v "(%a)")]
                                    letter)))
          badge (accumulate [letter nil k _ (pairs (. counts 1)) :until letter]
                  (if (and (utils.any (icollect [m _ (pairs (. counts 2))]
                                          (= k m)))
                           (utils.any (icollect [m _ (pairs (. counts 3))]
                                        (= k m))))
                      k))]
      (get-priority badge))))

(fn part-1 []
  (fennel.view (->> (read-input) (convert-to-priorities) (utils.sum))))

(fn part-2 []
  (fennel.view (->> (read-input-2) (group-priority) (utils.sum))))

{: part-1 : part-2}
