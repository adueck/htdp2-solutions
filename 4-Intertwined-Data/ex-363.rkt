;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname ex-363) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
; An Xexpr.v0 (short for X-expression) is a one-item list:
;   (cons Symbol '())
; <machine />
'(machine)

; An Xexpr.v1 is a list:
;   (cons Symbol [List-of Xexpr.v1])
; <machine>
;  <part /><part /><part />
; <machine>
'(machine (part) (part) (part))
; <machine>
;  <part>
;   <inner />
;  </part>
;  <part />
; </machine
'(machine (part (inner)) (part))

; An Attribute is a list of two items:
;   (cons Symbol (cons String '()))

; An Xexpr.v2 is a list
; - (cons Symbol XInfo)

; An XInfo is one of the following
; - (cons Body)
; - (cons [List-of Attribute] Body)
; where Body is one of
; - '()
; - (cons Xexpr.v2 Body)

'(single)
'(parent (child))
'(parent (child ((eyes "blue"))))
'(parent ((eyes "brown"))
         (child ((eyes "blue"))
                (grandchild)))