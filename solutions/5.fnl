(local fennel (require :lib.fennel))
(local lume (require :lib.lume))

(local utils (require :solutions.utils))

(fn get-stacks [stacks-by-line]
  (var stacks (icollect [_ v (ipairs (lume.clone (. stacks-by-line
                                                    (length stacks-by-line))))]
                []))
  (each [_ v (lume.ripairs stacks-by-line)]
    (var stack-index 1)
    (each [_ m (ipairs v)]
      (let [[_ crate trailing-spaces] m]
        (table.insert (. stacks stack-index) crate)
        (set stack-index (+ 1 stack-index (// (length trailing-spaces) 4))))))
  stacks)

(fn read-input []
  (let [box-matches (icollect [_ line (ipairs (utils.get-contents :./solutions/inputs/5.txt))]
                      (icollect [leading-spaces boxes spaces (string.gmatch line
                                                                            "(%s?)%[(%u)%](%s*)")]
                        [leading-spaces boxes spaces]))
        stacks-by-line (lume.filter box-matches #(< 0 (length $1)))
        move-matches (icollect [_ line (ipairs (utils.get-contents :./solutions/inputs/5.txt))]
                       (icollect [count start end (string.gmatch line
                                                                 "move (%d+) from (%d+) to (%d+)")]
                         [(tonumber count) (tonumber start) (tonumber end)]))
        moves (lume.filter move-matches #(< 0 (length $1)))
        stacks (get-stacks stacks-by-line)]
    {: moves : stacks}))

(fn move [stacks move] 
  (for [i 1 (. move 1)] 
    (let [crate (table.remove (. stacks (. move 2)))
          ]
      (table.insert (. stacks (. move 3)) crate))))

(fn run-moves [input] 
  (each [_ v (ipairs (. input :moves))]  (move (. input :stacks) (. v 1)))
  input)

(fn move-2 [stacks move] 
  (var temp-stack [])
  (for [i 1 (. move 1)] 
    (let [crate (table.remove (. stacks (. move 2)))
          ]
      (table.insert temp-stack crate)))
  (for [i 1 (. move 1)] 
    (let [crate (table.remove temp-stack)
          ]
      (table.insert (. stacks (. move 3)) crate))))

(fn run-moves-2 [input] 
  (each [_ v (ipairs (. input :moves))]  (move-2 (. input :stacks) (. v 1)))
  input)

(fn part-1 []
  (fennel.view (->> (read-input) (run-moves))))

(fn part-2 []
  (fennel.view (->> (read-input) (run-moves-2))))

{: part-1 : part-2}
