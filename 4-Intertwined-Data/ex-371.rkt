;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname ex-371) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
; An Attribute is a list of two items:
;   (cons Symbol (cons String '()))

; An Xexpr is a list
; - (cons Symbol XInfo)

; An XInfo is one of the following
; - Body
; - (cons [List-of Attribute] Body)
; where Body is one of
; - '()
; - (cons Xexpr Body)
; - (cons XWord Body)

; An XWord is '(word ((text String))).
(define w1 '(word ((text "foo"))))
(define w2 '(word ((text "bar"))))
(define w3 '(word ((text "hello"))))

; Any -> Boolean
; Checks whether a ISL+ value is a XWord
(check-expect (word? #true) #false)
(check-expect (word? 23) #false)
(check-expect (word? "foo") #false)
(check-expect (word? '(2 3)) #false)
(check-expect (word? '(widget ((text "cool")))) #false)
(check-expect (word? '(word ((tekst "hello")))) #false)
(check-expect (word? '(word ((text "hello")))) #true)
(define (word? v)
  (cond
    [(not (cons? v)) #false]
    [else
     (local
       ((define ws (first v))
        (define atts (second v)))
       (and
        (symbol? ws)
        (symbol=? ws 'word)
        (cons? atts)
        (list-of-attributes? atts)
        (string? (find-attr atts 'text))))]))

; XWord -> String
; extracts the value of the only attribute in an XWord
(check-expect (word-text w1) "foo")
(check-expect (word-text w2) "bar")
(define (word-text w)
  (local
    ((define loa (second w)))
    (second (first loa))))
 
; Xexpr -> [List-of Attribute]
; retrieves the list of attributes of xe
(define (xexpr-attr xe)
  (local ((define x-info (rest xe)))
    (cond
      [(empty? x-info) '()]
      [else (if
             (list-of-attributes? (first x-info))
             (first x-info)
             '())])))

; LOA-or-X -> boolean
; determines whether x is an element of [List-of Attribute]
; #false otherwise
(define (list-of-attributes? x)
  (cond
    [(empty? x) #true]
    [else (local
            ((define neloa-or-x (first x)))
            (cons? neloa-or-x))]))

; Xexpr -> Symbol
; returns the name of an Xexpr
(define (xexpr-name x)
  (first x))

; Xexpr -> [List-of Xexpr]
; returns the content of an Xexpr
(define (xexpr-content x)
  (local
    ((define x-info (rest x)))
    (cond
      [(empty? x-info) '()]
      [else
       (if (list-of-attributes? (first x-info))
           (rest x-info)
           x-info)])))

; [List-of Attributes] Symbol -> String or #false
; If the attributes list associates the symbol with a string,
; the function retrieves this string; otherwise it returns #false
(define (find-attr loa s)
  (local
    ((define res (assq s loa)))
    (cond
      [(false? res) #false]
      [else (second res)])))


  

