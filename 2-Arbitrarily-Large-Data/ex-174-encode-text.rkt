;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex-174-encode-text) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/batch-io)

; 1String -> String
; converts the given 1String to a 3-letter numeric String
(check-expect (encode-letter "z") (code1 "z"))
(check-expect (encode-letter "\t")
              (string-append "00" (code1 "\t")))
(check-expect (encode-letter "a")
              (string-append "0" (code1 "a")))
(define (encode-letter s)
  (cond
    [(>= (string->int s) 100) (code1 s)]
    [(< (string->int s) 10)
     (string-append "00" (code1 s))]
    [(< (string->int s) 100)
     (string-append "0" (code1 s))]))
 
; 1String -> String
; converts the given 1String into a String
(check-expect (code1 "z") "122")
(define (code1 c)
  (number->string (string->int c)))

; String -> null
; encodes a file numerically
(define (encode-file f)
  (write-file
   (string-append "encoded-" f)
   (collapse
    (process-file (read-words/line f)))))

; List-of-list-of-strings -> List-of-list-of-strings
; encodes all lines of the files numerically
(check-expect (process-file '()) '())
(check-expect (process-file
               (cons
                (cons "foo" '())
                (cons
                 (cons "bar" '()) '())))
               (cons
                (process-line (cons "foo" '()))
                (cons
                 (process-line (cons "bar" '()))
                 '())))
(define (process-file lols)
  (cond
    [(empty? lols) '()]
    [(cons? lols) (cons
                  (process-line (first lols))
                  (process-file (rest lols)))]))

; List-of-strings -> List-of-strings
; encodes a line numerically
(check-expect (process-line '()) '())
(check-expect (process-line
               (cons "ab" '()))
              (cons
               (string-append (encode-letter "a")
                              (encode-letter "b"))
               '()))
(define (process-line los)
  (cond
    [(empty? los) '()]
    [else (cons (encode-string (explode (first los)))
                (process-line (rest los)))]))

; List-of-1Strings -> String
; encodes a string numerically
(check-expect (encode-string '()) "")
(check-expect (encode-string (cons "a" (cons "b" '())))
              (string-append
               (encode-letter "a")
               (encode-letter "b")))
(define (encode-string loos)
  (cond
    [(empty? loos) ""]
    [(cons? loos) (string-append
                   (encode-letter (first loos))
                   (encode-string (rest loos)))]))

; List-of-list-of-strings -> String
; converts a list of lines into a string with \n for
; linebreaks
(check-expect (collapse '()) "")
(check-expect (collapse
               (cons
                (cons "first" (cons "line" '()))
                (cons
                 (cons "last" (cons "line" '())) '())))
              "first line\nlast line")
(check-expect (collapse
               (cons (cons "One" '()) (cons '() '())))
              "One\n")
(define (collapse lols)
  (cond
    [(empty? lols) ""]
    [(cons? lols)
     (string-append
      (make-line (first lols))
      (if (empty? (rest lols)) "" "\n")
      (collapse (rest lols)))]))

; List-of-strings -> String
; turns a list of words into a line of text
(check-expect (make-line '()) "")
(check-expect (make-line (cons "apple" (cons "bear" '())))
              "apple bear")
(define (make-line los)
  (cond
    [(empty? los) ""]
    [(cons? los)
     (string-append
      (first los)
      (if (empty? (rest los)) "" " ")
      (make-line (rest los)))]))