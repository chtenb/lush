#lang racket/base

;; ─── control characters 0-31,127 (ordered) ────────────────────────
(define NUL   0)   ; Null           Ctrl-@
(define SOH   1)   ; Start of Heading        Ctrl-A
(define STX   2)   ; Start of Text           Ctrl-B
(define ETX   3)   ; End of Text   (Ctrl-C, interrupt)
(define EOT   4)   ; End of Transmission     Ctrl-D
(define ENQ   5)   ; Enquiry                 Ctrl-E
(define ACK   6)   ; Acknowledge             Ctrl-F
(define BEL   7)   ; Bell / Alert            Ctrl-G
(define BS    8)   ; Backspace (Unix ^H)     Ctrl-H
(define TAB   9)   ; Horizontal Tab          Ctrl-I
(define LF   10)   ; Line Feed (newline)     Ctrl-J
(define VT   11)   ; Vertical Tab            Ctrl-K
(define FF   12)   ; Form Feed               Ctrl-L
(define CR   13)   ; Carriage Return         Ctrl-M
(define SO   14)   ; Shift Out               Ctrl-N
(define SI   15)   ; Shift In                Ctrl-O
(define DLE  16)   ; Data Link Escape        Ctrl-P
(define DC1  17)   ; Device Control 1 (XON)  Ctrl-Q
(define DC2  18)   ; Device Control 2        Ctrl-R
(define DC3  19)   ; Device Control 3 (XOFF) Ctrl-S
(define DC4  20)   ; Device Control 4        Ctrl-T
(define NAK  21)   ; Negative Acknowledge    Ctrl-U
(define SYN  22)   ; Synchronous Idle        Ctrl-V
(define ETB  23)   ; End of Transmission Blk Ctrl-W
(define CAN  24)   ; Cancel                  Ctrl-X
(define EM   25)   ; End of Medium           Ctrl-Y
(define SUB  26)   ; Substitute (Win EOF)    Ctrl-Z
(define ESC  27)   ; Escape
(define FS   28)   ; File Separator
(define GS   29)   ; Group Separator
(define RS   30)   ; Record Separator
(define US   31)   ; Unit Separator
(define DEL 127)   ; Delete (Unix Backspace)

;; ─── Friendly names often used in shell editors ───────────────────
(define CTRL-C ETX)
(define CTRL-D EOT)
(define CTRL-Z SUB)


;; ─── predicates (cross-platform tests) ────────────────────────────
(define (control? byte)
  (or (< byte 32) (= byte DEL)))

(define (printable? byte)
  (and (>= byte 32) (< byte 127)))

;; Returns #t for *both* common Backspace bytes:
;; 8   (Ctrl-H) – what Windows consoles send in raw mode
;; 127 (DEL)  – what most Unix terminals send
(define (backspace? byte)
  (or (= byte BS) (= byte DEL)))

;; Enter in raw mode can arrive as CR, LF, or (CR then LF).
;; This predicate is #t for either byte value.
(define (enter? byte)
  (or (= byte CR) (= byte LF)))

(define (ctrl-c? byte) (= byte CTRL-C))
(define (ctrl-d? byte) (= byte CTRL-D))
(define (ctrl-z? byte) (= byte CTRL-Z))
(define (esc? byte) (= byte ESC))




(provide (all-defined-out))
