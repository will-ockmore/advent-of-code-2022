(local fennel (require :lib.fennel))
(local lume (require :lib.lume))

(local utils (require :solutions.utils))

(fn set-at-path [tbl path v]
  (let [path-copy (lume.clone path)
        elem (table.remove path-copy 1)]
    (if (= 0 (length path-copy)) (tset tbl elem v)
        (set-at-path (. tbl elem) path-copy v))))

(fn read-input []
  (var current-dir-path [])
  (let [filesystem {}]
    (each [line (io.lines :./solutions/inputs/7.txt)]
      (let [cd-arg (string.match line "%$%scd%s(.+)")
            dir (string.match line "dir%s(%a+)")
            (filesize filename) (string.match line "(%d+)%s(.+)")]
        (if cd-arg
            (match cd-arg
              "/" (set current-dir-path [])
              ".." (table.remove current-dir-path)
              _ (table.insert current-dir-path cd-arg))
            dir
            (set-at-path filesystem (lume.concat current-dir-path [dir]) {})
            filename
            (set-at-path filesystem (lume.concat current-dir-path [filename])
                         {:file true : filename : filesize}))))
    filesystem))

(fn count-directory-sizes [filesystem]
  (var directory-sizes {})

  (fn count-dir [path dir]
    (let [size (accumulate [sum 0 k v (pairs dir)]
                 (+ sum
                    (if (. v :file) (. v :filesize)
                        (count-dir (lume.concat path [k]) v))))]
      (tset directory-sizes (lume.serialize path) size)
      size))

  (count-dir [] filesystem)
  directory-sizes)

(fn find-dir-to-remove [dir-sizes]
  (let [sizes-array (icollect [_ v (pairs (lume.clone dir-sizes))]
                      v)
        total (-> (lume.clone sizes-array)
                  (lume.sort)
                  (. (length sizes-array)))
        current-unused-space (- 70000000 total)
        required-unused-space (- 30000000 current-unused-space)]
    (-> (lume.filter dir-sizes #(< required-unused-space $1))
        (lume.reduce #(if (< $2 $1) $2 $1)))))

(fn part-1 []
  (fennel.view (-> (read-input) (count-directory-sizes)
                   (lume.filter #(> 100000 $1)) (lume.reduce #(+ $1 $2)))))

(fn part-2 []
  (fennel.view (-> (read-input) (count-directory-sizes) (find-dir-to-remove))))

{: part-1 : part-2}
