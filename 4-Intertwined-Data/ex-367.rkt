;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname ex-367) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
; An Attribute is a list of two items:
;   (const Symbol (cons String '()))
(define a0 '((initial "X")))

; An Xexpr.v2 is a list
; - (cons Symbol XInfo)
; where XInfo is
; - (cons Body)
; - (cons [List-of Attribute] Body)
; where Body is one of
; - '()
; - '(cons Xexpr.v2 '())

; <machine />
(define e0 '(machine))
; <machine initial="X" />
(define e1 `(machine ,a0))
; <machine><action /><machine>
(define e2 '(machine (action)))
; <machine><action /><machine>
(define e3 '(machine () (action)))
; <machine initial="X"><action /><action /></machine>
(define e4 `(machine ,a0 (action) (action)))
; <single />
(define e5 '(single))
; <parent><child /></parent>
(define e6 '(parent (child)))
;<parent><child eyes="blue" /></parent>
(define e7 '(parent (child ((eyes "blue")))))
; <parent eyes="brown">
;  <child eyes="blue">
;   <grandchild />
;  </child>
; </parent>
(define e8 '(parent ((eyes "brown"))
                    (child (eyes "blue")
                           (grandchild))))

; Xexpr.v2 -> Symbol
; returns the name of an xe
(check-expect (xexpr-name e5) 'single)
(check-expect (xexpr-name e7) 'parent)
(check-expect (xexpr-name e1) 'machine)
(define (xexpr-name xe)
  (first xe))

; Xexpr.v2 -> [List-of Xexpr.v2]
; returns the content of an xe
(check-expect (exexpr-content e0) '())
(check-expect (exexpr-content e1) '())
(check-expect (exexpr-content e2) '((action)))
(check-expect (exexpr-content e3) '((action)))
(check-expect (exexpr-content e4) '((action) (action)))
(check-expect (exexpr-content e5) '())
(check-expect (exexpr-content e6) '((child)))
(check-expect (exexpr-content e7) '((child ((eyes "blue")))))
(define (exexpr-content xe)
  (local ((define xinfo (rest xe)))
    (cond
      [(empty? xinfo) '()]
      [(list-of-attributes? (first xinfo)) (rest xinfo)]
      [else (list (first xinfo))])))

; Xexpr.v2 -> [List-of Attribute]
; retrieves the list of attributes of xe
(check-expect (xexpr-attr e0) '())
(check-expect (xexpr-attr e1) '((initial "X")))
(check-expect (xexpr-attr e2) '())
(check-expect (xexpr-attr e3) '())
(check-expect (xexpr-attr e4) '((initial "X")))
(check-expect (xexpr-attr e5) '())
(check-expect (xexpr-attr e8) '((eyes "brown")))
(define (xexpr-attr xe)
  (local ((define xinfo (rest xe)))
    (cond
      [(empty? xinfo) '()]
      [else (if
             (list-of-attributes? (first xinfo))
             (first xinfo)
             '())])))

;(define (xexpr-attr.v2 xe)
;  (local ((define xinfo (rest xe)))
;    (cond
;      [(empty? xinfo) '()]
;      [else (local
;             ((define loa (if (list-of-attributes? (first xinfo)) (first xinfo) '()))
;              (define body (if (list-of-attributes? (first xinfo)) (rest xinfo) (first xinfo))))
;             ( ... loa ...
;               ... (xexpr-attr.v2 (first body)) ... (rest body) ...))]))
; The self-referential definition in the template is not necessary
; because we're only looking at the top parent element and it's attributes
; we don't need to traverse down the tree and look at the attributes of the
; children

; [List-of Attribute] or Xexpr.v2 -> Boolean
; determines whether x is an element of [List-of Attribute]
; #false otherwise
(check-expect (list-of-attributes? '()) #t)
(check-expect (list-of-attributes? '((eyes "brown"))) #t)
(check-expect (list-of-attributes? '(child)) #f)
(define (list-of-attributes? x)
  (cond
    [(empty? x) #true]
    [(cons? (first x)) #true]
    ; can't be a list of attributes because it starts with a symbol
    [(symbol? (first x)) #false]))
