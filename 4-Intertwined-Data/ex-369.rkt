;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname ex-369) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
; An Attribute is a list of two items:
;   (cons Symbol (cons String '()))
(define a0 '((initial "X")))

; An Xexpr is a list
; - (cons Symbol XInfo)

; An XInfo is one of the following
; - Body
; - (cons [List-of Attribute] Body)
; where Body is one of
; - '()
; - (cons Xexpr Body)
 
(define e0 '(machine))
(define e1 `(machine ,a0))
(define e2 '(machine (action)))
(define e3 '(machine () (action)))
(define e4 `(machine ,a0 (action) (action)))
(define e5 '(single))
(define e6 '(parent (child)))
(define e7 '(parent (child ((eyes "blue")))))
(define e8 '(parent ((eyes "brown"))
                    (child ((eyes "blue"))
                           (grandchild))))

; Xexpr -> [List-of Attribute]
; retrieves the list of attributes of xe
(check-expect (xexpr-attr e0) '())
(check-expect (xexpr-attr e1) '((initial "X")))
(check-expect (xexpr-attr e2) '())
(check-expect (xexpr-attr e3) '())
(check-expect (xexpr-attr e4) '((initial "X")))
(check-expect (xexpr-attr e5) '())
(check-expect (xexpr-attr e8) '((eyes "brown")))
(define (xexpr-attr xe)
  (local ((define x-info (rest xe)))
    (cond
      [(empty? x-info) '()]
      [else (if
             (list-of-attributes? (first x-info))
             (first x-info)
             '())])))

; A LOA-or-X is one of the following
; - [List-of Attribute]
; - Xexpr

; LOA-or-X -> boolean
; determines whether x is an element of [List-of Attribute]
; #false otherwise
(check-expect (list-of-attributes? '(('color "red")))
              #true)
(check-expect (list-of-attributes? '(machine stand))
              #false)
(define (list-of-attributes? x)
  (cond
    [(empty? x) #true]
    [else (local
            ((define neloa-or-x (first x)))
            (cons? neloa-or-x))]))

; Xexpr -> Symbol
; returns the name of an Xexpr
(check-expect (xexpr-name e0) 'machine)
(check-expect (xexpr-name e2) 'machine)
(check-expect (xexpr-name e8) 'parent)
(define (xexpr-name x)
  (first x))

; Xexpr -> [List-of Xexpr]
; returns the content of an Xexpr
(check-expect (xexpr-content e0) '())
(check-expect (xexpr-content e1) '())
(check-expect (xexpr-content e2) '((action)))
(check-expect (xexpr-content e3) '((action)))
(check-expect (xexpr-content e4) '((action) (action)))
(check-expect (xexpr-content e5) '())
(check-expect (xexpr-content e6) '((child)))
(check-expect (xexpr-content e7) '((child ((eyes "blue")))))
(check-expect (xexpr-content e8) '((child ((eyes "blue"))
                                           (grandchild))))
(define (xexpr-content x)
  (local
    ((define x-info (rest x)))
    (cond
      [(empty? x-info) '()]
      [else
       (if (list-of-attributes? (first x-info))
           (rest x-info)
           x-info)])))

; [List-of Attributes] Symbol -> 'Symbol or #false
; If the attributes list associates the symbol with a string,
; the function retrieves this string; otherwise it returns #false
(check-expect (find-attr
               '((name "bob") (age "23"))
               'name)
              "bob")
(check-expect (find-attr
               '((name "bob") (age "23"))
               'naam)
              #false)
(check-expect (find-attr
               '()
               'name)
              #false)
(define (find-attr loa s)
  (local
    ((define res (assq s loa)))
    (cond
      [(false? res) #false]
      [else (second res)])))

; Bonus I just made
; Xexpr -> [List-of Attributes]
; Returns a list of ALL the attributes in a given Xexpr and it's children
(check-expect (all-attr e6) '())
(check-expect (all-attr e7) '((eyes "blue")))
(check-expect (all-attr e8) '((eyes "blue")
                              (eyes "brown")))
(define (all-attr x)
  (local
    ((define init (xexpr-attr x))
     (define lox (xexpr-content x)))
  (foldl
   (lambda (x attrs)
     (append (all-attr x) attrs))
   init
   lox)))
  

