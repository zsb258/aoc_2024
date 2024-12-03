#! /usr/bin/env racket
#lang racket

(define (line->nums line)
  (map string->number (string-split line)))

(define (parse-file fname)
  (map line->nums
       (string-split (file->string fname) "\n")))

(define (pairwise-diffs lst)
  (define n (sub1 (length lst)))
  (map - (take lst n) (take-right lst n)))

(define (same-sign? diffs)
  (or (andmap positive-integer? diffs)
      (andmap negative-integer? diffs)))

; 1 <= |x| <= 3
(define (safe-diff? x)
  ((compose (curryr member (range 1 4)) abs) x))

(define (safe-diffs? diffs)
  (andmap safe-diff? diffs))

(define (safe-1? lst)
  (define diffs (pairwise-diffs lst))
  (and (same-sign? diffs) (safe-diffs? diffs)))

(define (remove-at lst i)
  (append (take lst i)
          (take-right lst (- (length lst) i 1)))
  #||#)

(define (safe-2? lst)
  (or (safe-1? lst)
      (ormap safe-1?
             (map (curry remove-at lst)
                  (range (length lst))))
      #||#)
  #||#)

(define (part1 input)
  (count safe-1? input))

(define (part2 input)
  (count safe-2? input))

(module+ test
  (require rackunit)

  (define example (parse-file "example"))
  (check-equal? (part1 example) 2)
  (check-equal? (part2 example) 4)

  (define input (parse-file "input"))
  (check-equal? (part1 input) 326)
  (check-equal? (part2 input) 381)

  #||#)
