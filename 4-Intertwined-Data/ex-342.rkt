;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname ex-342) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
(require htdp/dir)

(define pp (create-dir "C:\\Users\\clay\\Desktop"))

; A Path is [List-of String].
; interpretation directions into a directory tree


; Dir String -> Path | #false
; if a file is present, returns the path, otherwise returns false
(define (find d f)
  (if (find? d f)
      (cond
        [(in-files? (dir-files d) f)
         (append (list (dir-name d)) (list f))]
        [else
         (for-dirs (dir-dirs d) f)])
       #false))

; [List-of Files] String -> Boolean
; returns true if a file with a filename f is present in a list of files
(define (in-files? fls f)
  (cond
    [(empty? fls) #false]
    [else (if (string=? (file-name (first fls)) f)
              #true
              (in-files? (rest fls) f))]))

; [List-of Dirs] String -> Path | #false
(define (for-dirs drs f)
  (cond
    [(empty? drs) #false]
    [else (if (find? (first drs) f)
              (find (first drs) f)
              (for-dirs (rest drs) f))]))

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



















             
