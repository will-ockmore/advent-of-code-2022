(local fennel (require :lib.fennel))
(local lume (require :lib.lume))

(local utils (require :solutions.utils))

(fn read-input []
  (icollect [char (string.gmatch (. (icollect [line (io.lines :./solutions/inputs/6.txt)]
                                            line) 1) "(%a)")] char))

(fn find-start-of-packet [input] 
  (var last-4 [])
  (var start-of-packet nil)
  (each [i v (ipairs input) :until (= (type start-of-packet) "number")]
    (if (= 4 (length last-4)) (table.remove last-4 1))
    (table.insert last-4 v)
    (if (= 4 (length last-4) (length (lume.unique last-4))) (set start-of-packet i)))
  start-of-packet)

(fn find-start-of-message [input] 
  (var last-14 [])
  (var start-of-message nil)
  (each [i v (ipairs input) :until (= (type start-of-message) "number")]
    (if (= 14 (length last-14)) (table.remove last-14 1))
    (table.insert last-14 v)
    (if (= 14 (length last-14) (length (lume.unique last-14))) (set start-of-message i)))
  start-of-message)

(fn part-1 []
  (fennel.view (->> (read-input) (find-start-of-packet) )))

(fn part-2 []
  (fennel.view (->> (read-input)  (find-start-of-message))))

{: part-1 : part-2}
