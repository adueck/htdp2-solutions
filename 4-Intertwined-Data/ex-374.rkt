;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname ex-374) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
(require 2htdp/image)
(define SIZE 12) ; font size 
(define COLOR "black") ; font color 
(define BT ; a graphical constant 
  (beside (circle 1 'solid 'black) (text " " SIZE COLOR)))
; An Attribute is a list of two items:
;   (cons Symbol (cons String '()))

; An Xexpr is one of the following
; - (cons Symbol Body)
; - (cons Symbol [List-of Attributes] Body)

; where Body is one of
; - '()
; - (cons Xexpr Body)
; - (cons XWord Body)

; An XItem.v2 is one of: 
; – (cons 'li (cons XWord '()))
; – (cons 'li (cons [List-of Attribute] (cons XWord '())))
; – (cons 'li (cons XEnum.v2 '()))
; – (cons 'li (cons [List-of Attribute] (cons XEnum.v2 '())))
; An XEnum.v2 is one of:
; – (cons 'ul (cons Items)
; – (cons 'ul (cons [List-of Attribute] Items))
; An Items is one of
; - '()
; - (cons XItem.v2 '())


(define e0
  '(ul
    (li (word ((text "one"))))
    (li (word ((text "two"))))
    (li (word ((text "three"))))))
(define e1
  '(ul
    (li (word ((text "one"))))
    (li (word ((text "two"))))
    (li (ul
         (li (word ((text "a"))))
         (li (word ((text "b"))))))))
(define e0-rendered
  (above/align
   'left
   (beside/align 'center BT (text "one" 12 'black))
   (beside/align 'center BT (text "two" 12 'black))))

; An XWord is '(word ((text String))).
(define w1 '(word ((text "foo"))))
(define w2 '(word ((text "bar"))))
(define w3 '(word ((text "hello"))))

; Image -> Image
; marks item with bullet
(check-expect (bulletize (text "foo" 12 'black))
              (beside/align 'center
                            BT
                            (text "foo" 12 'black)))
(define (bulletize item)
  (beside/align 'center BT item))

; XEnum.v2 -> Image
; renders an XEnum.v2 as an image
(check-expect (render-enum e0)
              (above/align 'left
                           (render-item '(li (word ((text "one")))))
                           (above/align 'left
                                        (render-item '(li (word ((text "two")))))
                                        (above/align 'left
                                                     (render-item '(li (word ((text "three")))))
                                                     empty-image))))                                             
(define (render-enum xe)
  (local
    ((define items (xexpr-content xe))
     ; Items -> Image
     (define (render-items is)
       (cond
         [(empty? is) empty-image]
         [else (above/align 'left
                            (render-item (first is))
                            (render-items (rest is)))])))
    (render-items items)))

; XItem.v2 -> Image
; renders one XItem.v2 as an image
(check-expect (render-item '(li (word ((text "one")))))
              (bulletize (text "one" SIZE 'black)))
(check-expect (render-item '(li (ul
                                 (li (word ((text "a")))))))
              (bulletize (render-enum '(ul (li (word ((text "a"))))))))
(define (render-item an-item)
  (local ((define content (first (xexpr-content an-item))))
    (bulletize
      (cond
        [(word? content)
         (text (word-text content) SIZE 'black)]
        [else (render-enum content)]))))

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


  

