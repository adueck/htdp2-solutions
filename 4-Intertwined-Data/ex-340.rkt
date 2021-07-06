;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname ex-340) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
(require htdp/dir)

(define pp (create-dir "C:\\Users\\clay\\Desktop"))

; Dir -> [List-of String]
; returns a list of all the names of files and directories in d
(define (ls d)
  (local
    (; [List-of Dirs] -> [List-of String]
     (define (for-dirs drs)
       (foldl
        (lambda (dr lon) (append (ls dr) lon))
        '()
        drs))
     ; [List-of Files] -> [List-of String]
     (define (for-files fls)
       (map
        (lambda (f) (file-name f))
        fls)))
   (append (list (dir-name d))
           (for-dirs (dir-dirs d))
           (for-files (dir-files d))))) 















             
