#lang racket

(require compatibility/defmacro)
                   #| https://www.minisudoku.com/ |#

(define small0        #| Normal |#       
  '((0 2 0 4)
    (0 4 3 2)
    (2 0 4 0)
    (0 0 0 3)))

(define small1        #| Normal |#
  '((0 2 0 0)
    (0 0 4 0)
    (1 3 2 4)
    (0 0 1 3)))

(define small2        #| Hard |#
  '((0 0 2 3)
    (0 2 4 0)
    (0 4 0 0)
    (0 3 0 0)))

(define small3        #| Wrong |#
  '((0 0 2 3)
    (0 2 4 0 0)
    (0 4 0 0)
    (0 3 0 0)))

(define regular0                #| Normal |#
  '((1 0 5  0 0 2  0 9 0)
    (0 2 0  5 4 9  0 6 3)
    (0 4 0  0 3 0  0 2 0)
    
    (0 6 0  0 1 0  0 3 0)
    (4 0 0  9 2 8  0 0 1)
    (5 0 1  0 6 0  0 0 9)
    
    (0 9 0  0 0 0  0 0 0)
    (8 0 0  3 9 0  4 7 5)
    (3 5 7  2 8 0  9 0 0)))

(define regular1                #| Normal |#
  '((0 4 0  2 0 7  6 8 0)
    (9 3 0  6 0 0  0 0 7)
    (0 6 0  0 0 5  0 0 0)
    
    (3 0 0  0 0 4  5 0 0)
    (8 0 4  0 0 0  0 0 0)
    (0 0 0  5 8 0  0 0 3)
    
    (0 0 0  0 0 6  0 2 0)
    (4 2 5  3 0 9  0 7 6)
    (0 0 7  4 0 2  0 0 0)))

(define regular2                #| Hard |#
  '((0 0 0  0 0 0  0 0 3)
    (0 4 0  0 0 7  0 6 0)
    (0 5 9  6 8 0  0 0 0)
    
    (0 8 3  0 4 0  0 0 0)
    (0 9 2  0 0 0  0 3 0)
    (4 0 7  0 0 2  0 9 1)
    
    (9 0 0  8 1 0  5 0 2)
    (2 0 0  0 3 9  0 0 0)
    (0 0 8  0 7 0  0 4 0)))

(define regular3                #| Hard |#
  '((0 0 0  5 8 4  0 6 0)
    (6 0 5  0 0 0  0 4 0)
    (7 0 0  0 0 0  0 0 3)
    
    (4 0 0  0 0 8  6 0 0)
    (0 0 0  0 0 9  0 7 0)
    (0 7 0  4 0 2  9 5 0)
    
    (9 0 0  8 4 0  0 0 0)
    (0 2 3  9 0 5  0 0 0)
    (5 6 0  0 1 0  0 0 8)))

(define big0                                             #| https://puzzlemadness.co.uk/16by16giantsudoku/easy/2020/11/11 |#
  '(( 5  0  0  0   0  0  7  1   0  3 14  0   4  0  2  0)
    (11 14 10  0   2 12  4  0   0  6  0 16   0  9  0  0)
    ( 3  0  6  4   0  0 14  0   2  0  0  0  10  5  7  0)
    ( 0  0  0 15   0 10  0  0   8  0  5  0   0  1 11  0)

    ( 0  4  0  2   0  0  0  6   9 13  0  0  14  8 12  1)
    ( 9 11  1  0  14  0  0 13   0  0  0  4   2  7  5  0)
    ( 8 12  0  0   4  1  0  0   0 10 15  7  16  3  0  9)
    ( 6  0  0  0   0  0 16  0   1  2  8 14   0  0 13  0)
    
    ( 0  8  0 16   0  0 12 11   0  5  1  0   0  0 15  0)
    ( 0  0  0  0  15  0  0  7   0  0  2  3   0 13  4  0)
    ( 2 10  3  0   9  0  1  0   4  0 13  6   7 12  8  0)
    ( 7  0 12  0   3  4 13  8  11  0 16  0   0  0  0  0)

    (15  0  2 13   7 11  6  0   0  1  4  5  12  0  9  0)
    ( 0  9  0  0   0 14  2  4   3  0 11  0   0  0  0  0)
    ( 0  0  0  7  13  0  8  0   0  0  0  0  11 15 14  0)
    ( 0  0  0  6   1 15  9 12  13  7 10  8   3  0 16  2)))

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

(define (numberCheck column max)
  (if (null? column)
      true
      (if (> (car column) max)
          (error "Number out of range")
          (if (< (car column) 0)
              (error "Number < 0")
              (numberCheck (cdr column) max)))))
      

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

#| cdr over reversed matrix, until only box rows left|#
(define (matrixCdr2 matrix sizeSqrt)
  (if (> (length matrix) sizeSqrt )
      (matrixCdr2 (matrixCdr matrix sizeSqrt) sizeSqrt)
      (reverse matrix)))

#| Exctract box from given rows |#
(define (extractBox matrix sizeSqrt new number)
  (if (null? matrix)
      new
      (extractBox
               (cdr matrix)
               sizeSqrt
               (append new
                      (reverse (list-tail
                                    (reverse (list-tail (car matrix) (* number sizeSqrt)))
                                    (- (length (list-tail (car matrix) (* number sizeSqrt))) sizeSqrt )
                                 )))
               number)))

#| matrixCdr -> matrixCdr2 -> extractBox |#
(define (cutter matrix number)
  (define sizeSqrt (sqrt (length (car matrix))))
  (if (>= number sizeSqrt)
      (cutter (matrixCdr matrix sizeSqrt) (- number sizeSqrt))
      (extractBox (matrixCdr2 (reverse matrix) sizeSqrt) sizeSqrt '() number)))

(define (getBox matrix number)
  (if (< number 0)
      (error "Looking for box smaller then zero")
      (if (>= number (length (car matrix)))
          (error "Looking for box out of range")
          (cutter matrix number))))

#|\\\\\\\\\\\\\\GETTING BOX NUMBER//////////////////////////////////////////|#

#| Boxes located on this line |#
(define (options matrix lineNum)
  (define sizeSqrt (sqrt (length (car matrix))))
  (range (* (quotient lineNum sizeSqrt) sizeSqrt)  (* (+(quotient lineNum sizeSqrt)1) sizeSqrt) ))
  
#| Boxe located on position |#
(define (boxNum matrix n pos option)
  (define sizeSqrt (sqrt (length (car matrix))))
  (if (>= n (length (car matrix)))
      (error "Too big n")
      (if (>= pos (length (car matrix)))
          (error "Too big pos")
          (if (>= pos sizeSqrt)
              (boxNum matrix n (- pos sizeSqrt) (cdr option))
              (car option))))
     )

(define (boxNumber matrix lineNum pos)
  (boxNum matrix lineNum pos (options matrix lineNum)))

#|\\\\\\\\\\\\\\ROW/COLUMN FILLING//////////////////////////////////////////|#

#| Numbers that can be putted instead of 0 on this line |#
(define (rowPossible line)
  (remove* line (range 1 (+ (length line) 1))))

#| Numbers that can be putted instead of 0 on this position |#     
(define (positionPossible column rowPossible box)
  (remove* box (remove* column rowPossible)) )
 
#| Returns new line with first possible 0 filled with number |#
(define (newRow matrix line lineNum zeroIndex)
  (if (null? zeroIndex)
      null
      (if (null? (positionPossible
                      (getColumn matrix (car zeroIndex))  #| Column |#
                      (rowPossible line)                  #| Row possibilities |#
                      (getBox                             #| Box |#
                             matrix
                            (boxNumber matrix lineNum (car zeroIndex))) )
                 )
          (error "Fatal error - Position of 0 has no filling possibilities")
          (if (> (length (positionPossible (getColumn matrix (car zeroIndex)) (rowPossible line) (getBox matrix (boxNumber matrix lineNum (car zeroIndex)))) ) 1) 
              (newRow matrix line lineNum (cdr zeroIndex))   #| Too many options for one position -> Go on |#
              (list-set                                      #| Only one option -> return modified line |#
                       line                       #| Line to be editted |#
                       (car zeroIndex)            #| Position to be editted |#
                       (car (positionPossible     #| New value |#
                                   (getColumn matrix (car zeroIndex))
                                   (rowPossible line)
                                   (getBox matrix (boxNumber matrix lineNum (car zeroIndex))) ))) ))
      ))

#| Repeats rowIter from begging always when any row is overwritten (0 -> number) |#
(define (rowIter fullMatrix lineNum)
  (if (>= lineNum (length fullMatrix))
      fullMatrix
      (if (null? (newRow
                        fullMatrix
                        (list-ref fullMatrix lineNum)
                        lineNum
                        (indexes-where (list-ref fullMatrix lineNum) zero?)) )
          (rowIter fullMatrix (+ lineNum 1))
          (rowIter (list-set     #| Makes matrix with new row (row where 0 was filled) |#
                            fullMatrix #| Matrix to be editted |#
                            lineNum    #| Position |#
                            (newRow    #| New line |#
                                   fullMatrix 
                                   (list-ref fullMatrix lineNum) #| Line |#
                                   lineNum
                                   (indexes-where (list-ref fullMatrix lineNum) zero?)  #| List of zero indexes |#
                            )
                    ) 0 )
       )))

#|\\\\\\\\\\\\\\FINAL//////////////////////////////////////////|#

#|Checks matrix size, numbers and duplicities (excluding 0)|#
(define (matrixCheck matrix)
  (rowSize matrix)
  (rowsCheck matrix (length (car matrix)) (length (car matrix)))
  (matrixNumberCheck matrix)
  (rowsDuplicateCheck matrix)
  (columnDuplicateCheck matrix)
  (boxDuplicateCheck matrix)
  )

(define (fill matrix)
  (rowIter matrix 0))
  
(define (solver matrix)
  (let ((solved (gensym)))   #| Generate random identifier |#
  (let ((solved matrix))  #| Fill matrix called only once |#
  (matrixCheck solved)
  solved
  )))

(define (solve matrix)
  (matrixCheck matrix)
  (solver (fill matrix))
  )

   