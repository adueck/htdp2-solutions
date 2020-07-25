;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex-054) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; it would be incorrect to use (string=? "resting" x) because
; this expression will error if x is not a string. The function
; string=? is only used for comparing strings, not for checking
; that variables are strings. Instead, we need to first check
; if x is a string using (string? x). In fact, this is the only
; check we need to do, because according to the data definition
; of LRCD, the only string that x can be is "resting"

; this is unnecessary because if x is a string,
; it can only be "resting"
; (and (string? x) (string=? "resting"))

; with our current data definition, this will suffice and
; does not introduce dangerous complexity or unreachable states
; (string? x)
