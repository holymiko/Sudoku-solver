#lang racket

(define square0
  '((1 2 3 4)
    (4 3 6 5)
    (7 8 9 9)
    (7 8 9 6)))

(define square
  '((1 2 3 4)
    (4 3 6 5)
    (0 8 0 0)
    (0 7 9 6)))

(define square1
  '((1 2 3 4)
    (4 3 6 5)
    (7 8 9 6)
    (5 7 1 2)))

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

#|Returns n-th column|#
(define (nth-column matrix index)
  (map (lambda (row) (nth row index)) matrix))


(define (rowSize matrix)
  (define sizeSqrt (sqrt (length (car matrix))))
  (if (integer? sizeSqrt)
      (if (< sizeSqrt 2)
          (error "Too small")
          true )
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
              (error "Wrong size of row")))
  )
)

#|Checks size of matrix|#
(define (matrixSizeCheck matrix)
  (rowSize matrix)
  (rowsCheck matrix (length (car matrix)) (length (car matrix)))
  )

(define (rowsDuplicateCheck matrix)
  (if (null? matrix)
      true
      (if (check-duplicates (car matrix))
           (if (= (check-duplicates (car matrix)) 0)
               (rowsDuplicateCheck (cdr matrix))
               (error "Duplicate in row"))
           (rowsDuplicateCheck (cdr matrix)))
      )
  )

(define (columnCheck matrix acc)
  (if (= acc 0)
      true
      (if (check-duplicates (nth-column matrix acc))
          (if (= (check-duplicates (nth-column matrix acc)) 0)
               (columnCheck matrix (- acc 1))
               (error "Duplicate in column"))
          (columnCheck matrix (- acc 1))
          )
      )
  )

(define (columnDuplicateCheck matrix)
  (columnCheck matrix (- (length (car matrix)) 1))
  )

#|Checks matrix duplicities (excluding 0)|#
#|Box check is missing|#
(define (matrixDuplicateCheck matrix)
  (rowsDuplicateCheck matrix)
  (columnDuplicateCheck matrix)
  )
          



