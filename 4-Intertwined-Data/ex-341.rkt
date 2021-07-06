;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname ex-341) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
(require htdp/dir)

(define pp (create-dir "C:\\Users\\clay\\Desktop"))

; Dir -> Number
; returns the total size of a given Dir
(define (du d)
  (local
    (; [List-of Dir] -> Number
     (define (for-dirs drs)
       (foldl
        (lambda (dr total) (+ (du dr) total))
        0
        drs))
     ; [List-of File] -> Number
     (define (for-files fls)
       (foldl
        (lambda (fl total) (+ (file-size fl) total))
        0
        fls)))
    (+
     1
     (for-dirs (dir-dirs d))
     (for-files (dir-files d)))))



















             
