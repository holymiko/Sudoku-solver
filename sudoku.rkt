#lang racket

(define square
  '((1 2 3)
    (4 5 6)
    (7 8 9)))

(define square1
  '((1 2 3 4)
    (4 5 6 5)
    (7 8 9 6)))

(define square2
  '((1 2 3 2 6)
    (4 5 6 5 6)
    (7 8 9 5 6)))

(define square3
  '((1 2 3 1 2 3 1 2 3)
    (4 5 6 4 5 6 4 5 6)
    (7 8 9 7 8 9 7 8 9)))

(define (nth list index)
  (if (null? list)
      null
      (if (= index 0)
          (car list)
          (nth (cdr list) (- index 1)))))

(define (nth-column matrix index)
  (map (lambda (row) (nth row index)) matrix))

#|Checks size of the first row|#
(define (rowSize matrix)
  (if (integer? (sqrt (length (car matrix))))
      (if (< (sqrt (length (car matrix))) 2)
          (error "Too small")
          true)
      (error "Illegal size"))
  )
