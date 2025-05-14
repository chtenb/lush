#lang racket
(require soup-lib)
(require "ansi.rkt")
(require "term.rkt")
(require "keycodes.rkt")

;; ------------------------------------------------------------------
;; helpers (manual ANSI ⎯ no extra libs)
;; ------------------------------------------------------------------
(define prompt (string-append (styled  "λ" '(fg 208)) " "))

;; ------------------------------------------------------------------
;; mutable buffer
;; ------------------------------------------------------------------
(struct buf (vec pos len) #:transparent #:mutable)
(define (make-buf size) (buf (make-vector size #\space) 0 0))

(define (redraw b)
  (define visible
    (list->string
     (vector->list (vector-copy (buf-vec b) 0 (buf-len b)))))

  (define out
    (string-append
     cursor-hide
     "\r"
     prompt
     visible
     clear-eol
     (cursor-left (- (buf-len b) (buf-pos b)))
     cursor-show))

  (display out)
  (flush-output))


;; insert printable char
(define (insert! b ch)
  (when (< (buf-len b) (vector-length (buf-vec b)))
    (for ([i (in-range (buf-len b) (buf-pos b) -1)])
      (vector-set! (buf-vec b) (add1 i) (vector-ref (buf-vec b) i)))
    (vector-set! (buf-vec b) (buf-pos b) ch)
    (set-buf-len! b (add1 (buf-len b)))
    (set-buf-pos! b (add1 (buf-pos b)))))

;; backspace
(define (backspace! b)
  (when (> (buf-pos b) 0)
    (for ([i (in-range (sub1 (buf-pos b)) (sub1 (buf-len b)))])
      (vector-set! (buf-vec b) i (vector-ref (buf-vec b) (add1 i))))
    (set-buf-pos! b (sub1 (buf-pos b)))
    (set-buf-len! b (sub1 (buf-len b)))))

;; ------------------------------------------------------------------
;; key codes
;; ------------------------------------------------------------------
(define BS-W  8)   (define BS-U 127)
(define ESC  27)   (define CR   13) (define LF 10)
(define LEFT 68)   (define RIGHT 67)
(define CTRL-C 3)

;; ------------------------------------------------------------------
;; read a line in raw mode
;; ------------------------------------------------------------------
(define (read-line-raw)
  (with-raw
      (define b (make-buf 1024))
    (let loop ()
      (redraw b)
      (define byte (read-byte))
      (cond
        ;; Quit
        [(or (= byte CTRL-C))]

        ;; Enter
        [(or (= byte CR) (= byte LF))
         (newline)
         (vector->string
          (vector-copy (buf-vec b) 0 (buf-len b)))]

        ;; Backspace
        [(or (= byte BS-W) (= byte BS-U))
         (backspace! b)
         (loop)]

        ;; Arrow keys: ESC [ C / ESC [ D
        [(= byte ESC)
         (when (= (read-byte) 91)            ; '['
           (define final (read-byte))
           (cond [(= final LEFT)
                  (when (> (buf-pos b) 0)
                    (set-buf-pos! b (sub1 (buf-pos b))))]
                 [(= final RIGHT)
                  (when (< (buf-pos b) (buf-len b))
                    (set-buf-pos! b (add1 (buf-pos b))))]))
         (loop)]        ;; Arrow keys

        ;; printable ASCII
        [(and (>= byte 32) (< byte 127))
         (insert! b (integer->char byte))
         (loop)]

        ;; ignore others
        [else (loop)]))))


;; ------------------------------------------------------------------
;; demo loop
;; ------------------------------------------------------------------
(define (repl)
  (define line (read-line-raw))
  (cond [(string=? line "quit") (void)]
        [else (printf "You typed: ~a\n" line) (repl)]))

(repl)
