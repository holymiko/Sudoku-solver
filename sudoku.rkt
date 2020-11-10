#lang racket

(define s0
  '((1 2 3 4)
    (4 3 6 5)
    (0 8 0 0)
    (0 7 9 6)))

(define s1
  '((1 2 3 4)
    (4 3 2 0)
    (2 0 0 1)
    (0 0 4 2)))

(define s2
  '((0 0 0 0 0 0 0 0 0)
    (0 0 0 0 0 0 0 0 0)
    (0 0 0 0 0 0 0 0 0)
    (0 0 0 0 0 0 0 0 0)
    (0 0 0 0 0 0 0 0 0)
    (0 0 0 0 0 0 0 0 0)
    (0 0 0 0 0 0 0 0 0)
    (0 0 0 0 0 0 0 0 0)
    (0 0 0 0 0 0 0 0 0)))

(define s3
  '((1 0 0  0 2 0  0 0 3)
    (4 5 6  4 5 6  4 5 6)
    (7 8 9  7 8 9  7 8 9)
    
    (0 0 0  1 2 3  1 2 3)
    (4 5 6  0 0 0  4 5 6)
    (7 8 9  7 8 9  0 0 0)
    
    (9 0 0  1 2 3  1 2 3)
    (4 5 6  0 0 0  0 0 0)
    (0 0 0  7 8 9  7 8 9)))

(define s4
  '((0 0 0 0  0 0 0 0  0 0 0 0  0 0 0 0)
    (0 0 0 0  0 0 0 0  0 0 0 0  0 0 0 0)
    (0 0 0 0  0 0 0 0  0 0 0 0  0 0 0 0)
    (0 0 0 0  0 0 0 0  0 0 0 0  0 0 0 0)

    (0 0 0 0  0 0 0 0  0 0 0 0  0 0 0 0)
    (0 0 0 0  0 0 0 0  0 0 0 0  0 0 0 0)
    (0 0 0 0  0 0 0 0  0 0 0 0  0 0 0 0)
    (0 0 0 0  0 0 0 0  0 0 0 0  0 0 0 0)
    
    (0 0 0 0  0 0 0 0  0 0 0 0  0 0 0 0)
    (0 0 0 0  0 0 0 0  0 0 0 0  0 0 0 0)
    (0 0 0 0  0 0 0 0  1 0 1 0  0 0 0 0)
    (0 0 0 0  0 0 0 0  0 0 0 0  0 0 0 0)

    (0 0 0 0  0 0 0 0  0 0 0 0  0 0 0 0)
    (0 0 0 0  0 0 0 0  0 0 0 0  0 0 0 0)
    (0 0 0 0  0 0 0 0  0 1 0 0  0 0 0 1)
    (0 0 0 0  0 0 0 0  0 0 0 0  0 0 1 0)))

(define (nth list index)
  (if (null? list)
      null
      (if (= index 0)
          (car list)
          (nth (cdr list) (- index 1)))))

#|Returns n-th column|#
(define (nth-column matrix index)
  (map (lambda (row) (nth row index)) matrix))

#|\\\\\\\\\\\\\\SIZE CHECK//////////////////////////////////////////|#

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

#|\\\\\\\\\\\\\\NUMBER CHECK//////////////////////////////////////////|#

(define (numberCheck matrix max)
  (if (null? matrix)
      true
      (if (> (car matrix) max)
          (error "Number out of range")
          (if (< (car matrix) 0)
              (error "Number < 0")
              (numberCheck (cdr matrix) max)))))
      

(define (matrixNumberCheck matrix)
  (if (null? matrix)
      true
      (numberCheck (car matrix) (length (car matrix))))
  (if (null? (cdr matrix))
      true
      (matrixNumberCheck (cdr matrix)))) 

#|\\\\\\\\\\\\\\DUPLICITY CHECK//////////////////////////////////////////|#

(define (rowsDuplicateCheck matrix)
  (if (null? matrix)
      true
      (if (check-duplicates (car matrix))
           (if (= (check-duplicates (car matrix)) 0)  #| This doesnt work 100% '(0 0 1 1) |#
               (rowsDuplicateCheck (cdr matrix))
               (error "Duplicate in row"))
           (rowsDuplicateCheck (cdr matrix)))
      ))

(define (columnCheck matrix acc)
  (if (= acc 0)
      true
      (if (check-duplicates (nth-column matrix acc))
          (if (= (check-duplicates (nth-column matrix acc)) 0)
               (columnCheck matrix (- acc 1))
               (error "Duplicate in column"))
          (columnCheck matrix (- acc 1))
          )))

(define (columnDuplicateCheck matrix)
  (columnCheck matrix (- (length (car matrix)) 1)))


(define (boxCheck matrix count)
  (if (> count 0)
      (if (check-duplicates (getBox matrix count))
           (if (= (check-duplicates (getBox matrix count)) 0)
               (boxCheck matrix (- count 1))
               (error "Duplicate in box"))
           (boxCheck matrix (- count 1)))
      true))

(define (boxDuplicateCheck matrix)
  (boxCheck matrix (- (length (car matrix)) 1)))

#|\\\\\\\\\\\\\\BOX EXTRACTING//////////////////////////////////////////|#

#| cdr N times |#
(define (matrixCdr matrix times)
  (if (null? (cdr matrix))
      (error "matrixCdr")
      (if (<= times 0)
          matrix
          (matrixCdr (cdr matrix) (- times 1)))))

(define (cutter matrix number)
  (define sizeSqrt (sqrt (length (car matrix))))
  (if (>= number sizeSqrt)
      (cutter (matrixCdr matrix sizeSqrt) (- number sizeSqrt))
      (extractBox (matrixCdr2 (reverse matrix) sizeSqrt) sizeSqrt '() number)))

#| cdr until only box rows left|#
(define (matrixCdr2 matrix sizeSqrt)
  (if (> (length matrix ) sizeSqrt )
      (matrixCdr2 (matrixCdr matrix sizeSqrt) sizeSqrt)
      (reverse matrix)))

#| Exctract box from given rows |#
(define (extractBox matrix sizeSqrt new number)
  (if (null? matrix)
      new
      (extractBox (cdr matrix)
               sizeSqrt
               (append new (reverse (list-tail
                                     (reverse (list-tail (car matrix) (* number sizeSqrt)))
                                     (- (length (list-tail (car matrix) (* number sizeSqrt))) sizeSqrt )
                                     )))
               number)))

(define (getBox matrix number)
  (if (< number 0)
      (error "Looking for box smaller then zero")
      (if (>= number (length (car matrix)))
          (error "Looking for box out of range")
          (cutter matrix number))))

#|Checks matrix size, numbers and duplicities (excluding 0)|#
(define (matrixCheck matrix)
  (rowSize matrix)
  (rowsCheck matrix (length (car matrix)) (length (car matrix)))
  (matrixNumberCheck matrix)
  (rowsDuplicateCheck matrix)
  (columnDuplicateCheck matrix)
  (boxDuplicateCheck matrix)
  )
          



