;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex-173-remove-articles) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/batch-io)

; String -> null
; reads a file and creates a version with no articles
(define (remove-articles f)
  (write-file
   (string-append "no-articles-" f)
   (collapse
    (process-file (read-words/line f)))))

; List-of-list-of-strings -> List-of-list-of-strings
; removes all the articles from text file data
(check-expect (process-file '()) '())
(check-expect (process-file (cons
                             (cons "the" (cons "cool" '()))
                             (cons
                              (cons "a" (cons "an" '())) '())))
                            (cons
                             (cons "cool" '())
                             (cons
                              '() '())))
(define (process-file lols)
  (cond
    [(empty? lols) '()]
    [(cons? lols) (cons
                   (process-line (first lols))
                   (process-file (rest lols)))]))

; List-of-strings -> List-of-strings
; removes any articles from a list of strings
(check-expect (process-line '()) '())
(check-expect (process-line (cons
                             "the" (cons "an"
                                         (cons "a"
                                               (cons "stay" '())))))
              (cons "stay" '()))
(define (process-line los)
  (cond
    [(empty? los) '()]
    [else (if
           (is-article? (first los))
             (process-line (rest los))
             (cons (first los) (process-line (rest los))))]))

; String -> String
; checks if a string is an article
(check-expect (is-article? "foo") #false)
(check-expect (is-article? "the") #true)
(check-expect (is-article? "a") #true)
(check-expect (is-article? "an") #true)
(define (is-article? s)
  (cond
    [(string=? s "the") #t]
    [(string=? s "a") #t]
    [(string=? s "an") #t]
    [else #false]))

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