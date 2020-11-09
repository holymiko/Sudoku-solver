#lang racket

(define square
  '((1 2 3)
    (4 5 6)
    (7 8 9)))

(define (nth list index)
  (if (null? list)
      null
      (if (= index 0)
          (car list)
          (nth (cdr list) (- index 1)))))

(define (nth-column matrix index)
  (map (lambda (row) (nth row index)) matrix))