;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex-148) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; In some cases it seems better to work better with data definitions
; that accomodate for emtpy lists. For instance, with how-many, you
; might want to be able to identify that there were zero items given
; in counting an empty list.
; However in other cases, it's better not to allow for empty lists,
; beacues doing so breaks the logic of the problem. For instance, if
; you are asking "Is every item in this list true?" then you need to
; have at least one item, otherwise the question becomes impossible.
; Given an empty list, will you say #true or #false to the question of
; "is every item in this list true?" ... It is not clear what the answer
; should be.