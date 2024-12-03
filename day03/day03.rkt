#! /usr/bin/env racket
#lang racket

(define MUL_PATTERN
  #px"mul\\(([\\d]+),([\\d]+)\\)")

(define (parse-file fname)
  (string-join (string-split (file->string fname)
                             "\n")))
(define (extract-nums str)
  (map (curry map string->number)
       (regexp-match* MUL_PATTERN
                      str
                      #:match-select
                      (curryr take-right 2)))
  #||#)

; HACK: assumes input does not start with "n't"
(define (preprocess str)
  (string-join
   (filter-not (curryr string-prefix? "n't")
               (string-split str "do")))
  #||#)

(define (part1 input)
  (for/sum ([p (extract-nums input)]) (apply * p))
  #||#)

(define (part2 input)
  (for/sum ([p (extract-nums (preprocess input))])
           (apply * p))
  #||#)

(module+ test
  (require rackunit)

  (define example1 (parse-file "example1"))
  (check-equal? (part1 example1) 161)

  (define example2 (parse-file "example2"))
  (check-equal? (part2 example2) 48)

  (define input (parse-file "input"))
  (check-equal? (part1 input) 173517243)
  (check-equal? (part2 input) 100450138)

  #||#)
