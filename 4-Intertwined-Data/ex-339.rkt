;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname ex-339) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
(require htdp/dir)

(define pp (create-dir "C:\\Users\\clay\\Desktop"))

; Dir -> Number
; determines how many files a given Dir.v3 contains
(define (how-many d)
  (local (; Dir* -> Number
          (define (for-dirs ds)
            (cond
              [(empty? ds) 0]
              [else (+
                     (how-many (first ds))
                     (for-dirs (rest ds)))]))
          ; File* -> Number
          (define (for-files fs)
            (cond
              [(empty? fs) 0]
              [else (add1 (for-files (rest fs)))])))      
   (+ (for-dirs (dir-dirs d))
      (for-files (dir-files d)))))

; Dir -> Boolean
; determines whether or note a file with name occurs in d
(define (find? d name)
  (local
    (; Dir -> Boolean
     (define (for-dir d)
       (find? d name))
     ; File -> Boolean
     (define (for-file f)
       (string=? (file-name f) name)))
  (or 
   (ormap for-dir (dir-dirs d))
   (ormap for-file (dir-files d)))))










             
