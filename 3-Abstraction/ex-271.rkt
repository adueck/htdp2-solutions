;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex-271) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; a Name is a String

; Name [List-of Name] -> Boolean
; determines whether any of the names on the list
; are equal to or an extension of nm
(check-expect (find-name "bob" (list "bobby" "jim")) #true)
(check-expect (find-name "frank" (list "tom" "joe")) #false)
(check-expect (find-name "ray" (list "ray")) #true)
(define (find-name nm lonm)
  (local (; Name -> Boolean
          ; checks if the given name matches or is a derivative
          ; of nm
          (define (matches? x)
            (cond
              [(> (string-length nm) (string-length x)) #f]
              [else (string=?
                     (substring x 0 (string-length nm)) nm)])))
    (ormap matches? lonm)))

; [List-of Name] -> Boolean
; checks whether all names on a list start with "a"
(check-expect (all-with-a?
               (list "art" "albert" "ace"))
              #t)
(check-expect (all-with-a? (list "bill" "abe")) #f)
(define (all-with-a? lonm)
  (local (; Name -> Boolean
          (define (starts-w-a? x)
            (string=? (string-ith x 0) "a")))
    (andmap starts-w-a? lonm)))

; We could use you use either ormap or andmap to define a function that
; ensures that no name on some list exceeds a given width

; with ormap - see if there's one item that exceeds the givin width
; with andmap - seef if all items are within the given width

; It would seem, however that ormap would be more efficient, so let's
; implement it with that

; Nat [List-of Name] -> Boolean
; ensures that no name on some list exceeds a given width
(check-expect (all-short-enough? 3 (list "bill" "bob")) #f)
(check-expect (all-short-enough? 5 (list "frank" "tom")) #t)
(define (all-short-enough? width lonm)
  (local (; Name -> Boolean
          ; checks if the name has exceeded the given length
          (define (is-too-long? x)
            (> (string-length x) width)))
    (not (ormap is-too-long? lonm))))