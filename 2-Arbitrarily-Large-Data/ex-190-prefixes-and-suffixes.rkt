;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex-190-prefixes-and-suffixes) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; List-of-1Strings -> List-of-list-of-1Strings
; produces a list of all the prefixes of a given List-of-1Strings
(check-expect (prefixes '()) '())
(check-expect (prefixes (list "x")) (list (list "x")))
(check-expect (prefixes (list "a" "b" "c"))
              (list
               (list "a")
               (list "a" "b")
               (list "a" "b" "c")))

(define (prefixes lo1s)
  (cond
    [(empty? lo1s) '()]
    [(cons? lo1s) (cons (list (first lo1s))
                   (prepend-all
                    (first lo1s)
                    (prefixes (rest lo1s))))]))

; List-of-1Strings -> List-of-list-of-1Strings
; produces a list of all the suffixes of a given List-of-1Strings
(check-expect (suffixes '()) '())
(check-expect (suffixes (list "x")) (list (list "x")))
(check-expect (suffixes (list "a" "b" "c"))
              (list
               (list "a" "b" "c")
               (list "b" "c")
               (list "c")))
(define (suffixes lo1s)
  (cond
    [(empty? lo1s) '()]
    [else (cons lo1s (suffixes (rest lo1s)))]))

; 1String -> List-of-list-of-1String
; prepends a single 1String to each list-of-1String
(check-expect (prepend-all "x" (list '())) (list (list "x")))
(check-expect (prepend-all "x" (list
                                (list "a" "b")
                                '()
                                (list "c")))
              (list
               (list "x" "a" "b")
               (list "x")
               (list "x" "c")))
(check-expect (prepend-all "x" (list
                                (list "a" "b" "c")
                                (list "d" "e" "f")))
              (list
               (list "x" "a" "b" "c")
               (list "x" "d" "e" "f")))
(define (prepend-all s lol1s)
  (cond
    [(empty? lol1s) '()]
    [else (cons
           (cons s (first lol1s))
           (prepend-all s (rest lol1s)))]))
