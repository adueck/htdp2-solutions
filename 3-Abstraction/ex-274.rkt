;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex-274) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; Prefixes is a [List-of [List-of 1Strings]]

; [List-of 1String] -> Prefixes
; produces a list of all the prefixes of a given List-of-1Strings
(check-expect (prefixes '()) '())
(check-expect (prefixes (list "x")) (list (list "x")))
(check-expect (prefixes (list "a" "b" "c"))
              (list
               (list "a")
               (list "a" "b")
               (list "a" "b" "c")))

(define (prefixes lo1s)
  (local (; 1String Prefixes -> [List-of 1String]
          (define (add-prefix-set s prefs)
            (local ((define last-line (last prefs))
                    (define to-add (append last-line (list s))))
              (append prefs (list to-add)))))
  ; using foldl because it fits the signature with desired output
  ; [1String Prefixes -> Prefixes] Prefixes [List-of 1String] -> Prefixes
  (foldl add-prefix-set '() lo1s)))

; returns the last item in a list
; [X] [List-of X] -> X
(check-expect (last (list 2 4 5 6)) 6)
(check-expect (last '()) '())
(define (last li)
  (cond
    [(empty? li) '()]
    [(empty? (rest li)) (first li)]
    [else (last (rest li))]))

; returns the first item in a list
(check-expect (myfirst (list 2 4 5 6)) 2)
(check-expect (myfirst '()) '())
(define (myfirst li)
  (cond
    [(empty? li) '()]
    [else (first li)]))

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
  (local (; 1String Prefixes -> [List-of 1String]
          (define (add-suffix-set s prefs)
            (local ((define first-line (myfirst prefs))
                    (define new-line (cons s first-line)))
              (cons new-line prefs))))
  (foldr add-suffix-set '() lo1s)))

