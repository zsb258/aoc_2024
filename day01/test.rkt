#lang racket

(require "day01.rkt")

(module+ test
  (require rackunit)

  (define example (parse-file "example"))
  (check-equal? (part1 example) 11)
  (check-equal? (part2 example) 31)

  (define input (parse-file "input"))
  (check-equal? (part1 input) 2192892)
  (check-equal? (part2 input) 22962826)

  #||#)
