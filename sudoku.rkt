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

(define s2                #| Legit sudoku from web |#
  '((1 0 5  0 0 2  0 9 0)
    (0 2 0  5 4 9  0 6 3)
    (0 4 0  0 3 0  0 2 0)
    
    (0 6 0  0 1 0  0 3 0)
    (4 0 0  9 2 8  0 0 1)
    (5 0 1  0 6 0  0 0 9)
    
    (0 9 0  0 0 0  0 0 0)
    (8 0 0  3 9 0  4 7 5)
    (3 5 7  2 8 0  9 0 0)))

(define s3
  '((1 0 0  0 2 0  0 0 3)
    (4 0 6  4 5 6  4 5 6)
    (7 0 9  6 8 9  5 8 9)
    
    (0 6 0  0 2 3  6 2 3)
    (4 7 6  5 0 0  0 5 6)
    (7 8 9  7 8 9  0 0 0)
    
    (9 0 0  8 2 3  9 2 3)
    (4 9 6  0 0 0  0 0 0)
    (0 0 0  0 8 9  7 8 9)))

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
    (0 0 0 0  0 0 0 0  0 0 1 0  0 0 0 0)
    (0 0 0 0  0 0 0 0  0 0 0 0  0 0 0 2)

    (0 0 0 0  0 0 0 0  0 0 0 0  2 0 0 0)
    (0 0 0 0  0 0 0 0  0 0 0 0  0 0 0 0)
    (0 0 0 0  0 0 0 0  0 0 0 0  0 0 0 1)
    (0 0 0 0  0 0 0 0  0 0 0 0  0 0 0 0)))

(define (nth list index)
  (if (null? list)
      null
      (if (= index 0)
          (car list)
          (nth (cdr list) (- index 1)))))

#|Returns n-th column|#
(define (getColumn matrix index)
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
      (if (check-duplicates (remove* (list 0) (car matrix)))
           (error "Duplicate in row")
           (rowsDuplicateCheck (cdr matrix))
      )))

(define (columnCheck matrix acc)
  (if (= acc 0)
      true
      (if (check-duplicates (remove* (list 0) (getColumn matrix acc)))
          (error "Duplicate in column")
          (columnCheck matrix (- acc 1))
          )))

(define (columnDuplicateCheck matrix)
  (columnCheck matrix (- (length (car matrix)) 1)))


(define (boxCheck matrix count)
  (if (> count 0)
      (if (check-duplicates (remove* (list 0) (getBox matrix count)))
           (error "Duplicate in box")
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

#| Numbers that can be putted instead of 0 on this line |#
(define (rowPossible line)
  (remove* line (range 1 (+ (length line) 1))))

#| Numbers that can be putted instead of 0 on this position |#        #| Doesnt include box numbers |#
(define (positionPossible column rowPossible)
  (remove* column rowPossible))
 
#| Returns new line with first possible 0 filled with number |#
(define (newRow matrix line zeroIndex)
  (define rowPos (rowPossible line))
  (if (null? zeroIndex)
      null
      (if (null? (positionPossible (getColumn matrix (car zeroIndex)) rowPos))
          (error "Fatal error - Position of 0 has no filling possibilities")
          (if (> (length (positionPossible (getColumn matrix (car zeroIndex)) rowPos)) 1)
              (newRow matrix line (cdr zeroIndex))
              (list-set line (car zeroIndex) (car (positionPossible (getColumn matrix (car zeroIndex)) rowPos))) ))))

#| Repeats rowIter from begging always when any row is overwritten (0 -> number) |#
(define (rowIter fullMatrix n)
  (if (>= n (length fullMatrix))
      fullMatrix
      (if (null? (newRow fullMatrix (list-ref fullMatrix n) (indexes-where (list-ref fullMatrix n) zero?)) )
          (rowIter fullMatrix (+ n 1))
          (rowIter (list-set
                            fullMatrix #| Matrix to be editted |#
                            n          #| Position |#
                            (newRow    #| New line |#
                                   fullMatrix 
                                   (list-ref fullMatrix n) #| Line |#
                                   (indexes-where (list-ref fullMatrix n) zero?)  #| List of zero indexes |#
                            )
                    ) 0 )
       )))

(define (rowFill matrix)
  (rowIter matrix 0))







