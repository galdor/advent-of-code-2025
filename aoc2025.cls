(defsystem "aoc2025"
  :description "Solutions for the Advent of Code 2025 event."
  :author "Nicolas Martyanoff <nicolas@n16f.net>"
  :license "ISC"
  :dependencies
  ("tungsten")
  :components
  (("src/"
    ("utils.lisp"
     "day-01.lisp"
     "day-02.lisp"
     "day-03.lisp"))))
