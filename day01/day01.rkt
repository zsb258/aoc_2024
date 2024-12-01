#! /usr/bin/env racket
#lang racket

(define (line->nums line)
  (map string->number (string-split line)))

(define (parse-file fname)
  (map line->nums
       (string-split (file->string fname) "\n")))

(define (abs-diff a b)
  (abs (- a b)))

(define (part1 input)
  (define lefts (sort (map first input) <))
  (define rights (sort (map last input) <))

  (foldl + 0 (map abs-diff lefts rights)))

(define (appearance-count n lst)
  (count (lambda (x) (= x n)) lst))

(define (part2 input)
  (define lefts (map first input))
  (define rights (map last input))

  (foldl +
         0
         (map (lambda (n)
                (* n (appearance-count n rights)))
              lefts)))

(module+ test
  (require rackunit)

  (define example (parse-file "example"))
  (check-equal? (part1 example) 11)
  (check-equal? (part2 example) 31)

  (define input (parse-file "input"))
  (check-equal? (part1 input) 2192892)
  (check-equal? (part2 input) 22962826)

  #||#)
