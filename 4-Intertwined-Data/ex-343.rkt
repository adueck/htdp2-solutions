;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname ex-343) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
(require htdp/dir)

(define pp (create-dir "C:\\Users\\clay\\Desktop"))

; A Path is [List-of String].
; interpretation directions into a directory tree

; Dir -> [List-of Path]
; Lists the paths to all files in d
(define (ls-R d)
  (local
    (; [List-of Dirs] -> [List-of Path]
     (define (look-in-dirs ds)
       (foldr
        (lambda (d l) (append l (ls-R d)))
        '()
        ds))
     ; String [List-of File] -> [List-of Path]
     (define (add-path name fs)
       (map
        (lambda (f) (list name (file-name f)))
        fs)))
   (append
     ; list of paths for files in current directory
     (add-path (dir-name d) (dir-files d))
     ; list of paths for files in sub directories
     (look-in-dirs (dir-dirs d)))))











             
