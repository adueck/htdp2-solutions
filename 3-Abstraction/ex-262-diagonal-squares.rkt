;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex-262-diagonal-squares) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(define sqw 10)

; BinaryNum is one of the following
; - 0
; - 1

; IMatrix is a [NEList-of IMatrixRow]

; IMatrixRow is a [NEList-of BinaryNum]

; Nat -> IMatrix
; creates an identity matrix square n wide
(check-expect (identityM 1) (list (list 1)))
(check-expect (identityM 3)
              (list (list 1 0 0) (list 0 1 0) (list 0 0 1)))
(define (identityM n)
  (local
    ((define width n)
     ; Nat Nat -> IMatrixRow
     ; creates one row in an idendity matrix
     ; with a square represented at 1
     (define (make-row n w)
       (local
         ((define pos n)
          (define (fill-num i p) (if (= i p) 1 0)) 
          (define (build-row i)
            (cond
              [(= (sub1 i) 0) (cons (fill-num i pos) '())]
              [else (cons (fill-num i pos)
                          (build-row (sub1 i)))])))
         (build-row w)))
     (define (build-square n)
       (cond
         [(= n 0) '()]
         [else (cons (make-row n width)
                     (build-square (sub1 n)))])))
    (build-square n)))

; IMatrix -> Image
; draws an identity matrix from a IMatrix data representation
(check-expect (draw-matrix (identityM 1))
              (square sqw "solid" "black"))
(check-expect (draw-matrix (identityM 3))
              (above
               (beside
                (square sqw "solid" "black")
                (square sqw "outline" "black")
                (square sqw "outline" "black"))
               (beside
                (square sqw "outline" "black")
                (square sqw "solid" "black")
                (square sqw "outline" "black"))
               (beside
                (square sqw "outline" "black")
                (square sqw "outline" "black")
                (square sqw "solid" "black"))))
(define (draw-matrix m)
  (local
    (; IMatrixRow -> Image
     ; draws a row from an identity matrix
     (define (draw-row r)
       (local
         ((define current-square (first r))
          ; BinaryNum -> Image
          ; Draws an empty (0) or full (1) square
          (define (draw-square b)
            (square sqw
                    (cond
                      [(= b 0) "outline"]
                      [(= b 1) "solid"])
                    "black")))
         (cond
           [(empty? (rest r)) (draw-square current-square)]
           [else (beside
                  (draw-square current-square)
                  (draw-row (rest r)))]))))
  (cond
    [(empty? (rest m)) (draw-row (first m))]
    [else (above (draw-row (first m))
                 (draw-matrix (rest m)))])))
