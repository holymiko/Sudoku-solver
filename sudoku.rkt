#lang racket

(define square
  '((1 2 3 4)
    (4 5 6 5)
    (7 8 9 6)
    (7 8 9 6)))

(define square1
  '((1 2 3 4)
    (4 5 6 5)
    (7 8 9 6)
    (7 8 9 4)))

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
  (define sizeSqrt (sqrt (length (car matrix))))
  (if (integer? sizeSqrt)
      (if (< sizeSqrt 2)
          (error "Too small")
          true)
      (error "Illegal size"))
  )

(define (rowsCheck matrix n acc)
  (if (= acc 0)
      (if (null? matrix)
          true
          (error "Too many rows"))
      (if (null? matrix)
          (error "Not enought rows")
          (if (= (length (car matrix)) n)
              (rowsCheck (cdr matrix) n (- acc 1))
              (error "Wrong size of row")
          )
      )
  )
)

#|Checks size of matrix|#
(define (matrixSizeCheck matrix)
  (rowSize matrix)
  (rowsCheck matrix (length (car matrix)) (length (car matrix)))
  )






