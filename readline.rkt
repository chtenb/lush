#lang racket
(require soup-lib)
(require racket/port)
(require racket/async-channel)
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

(define (move-left b n)
  (when (> (buf-pos b) 0)
    (set-buf-pos! b (sub1 (buf-pos b)))))

(define (move-right b n)
   (when (< (buf-pos b) (buf-len b))
     (set-buf-pos! b (add1 (buf-pos b)))))

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

; This does not work, because read-byte blocks the entire OS-thread on windows
;; read one byte, but return #f if nothing arrives within N ms
; (define (read-byte/timeout ms [in (current-input-port)])
;   (define ch (make-channel))
;   (thread
;     (λ ()
;       (define b (read-byte in))
;       (channel-put ch b)))
;   (sync/timeout (/ ms 1000.0) ch))

; This implementation polls every 1ms
;; read one byte, but return #f if nothing arrives within N ms
(define (read-byte/timeout ms [in (current-input-port)])
  (define deadline (+ (current-inexact-milliseconds) ms))
  (let loop ()
    (cond
      [(byte-ready? in) (read-byte in)]
      [(> (current-inexact-milliseconds) deadline) #f]
      [else (sleep 0.001) (loop)])))

(define esc-timeout 25.0)

;; ------------------------------------------------------------------
;; read a line in raw mode
;; ------------------------------------------------------------------
(define (read-line-raw)
  (with-raw
    (define b (make-buf 1024))
      
    (define (loop)
      (redraw b)
      (define byte (read-byte))
      (interpret-byte byte))

    (define (interpret-byte byte)
      (cond
        ;; Quit
        [(ctrl-c? byte) (void)]
        [(ctrl-d? byte) (void)]

        ;; Enter
        [(enter? byte)
         (newline)
         (vector->string
          (vector-copy (buf-vec b) 0 (buf-len b)))]

        ;; Backspace
        [(backspace? byte)
         (backspace! b)
         (loop)]

        ;; Arrow keys: ESC [ C / ESC [ D
        [(esc? byte)
         (define second (read-byte/timeout esc-timeout))
         (cond
           [(not second) (loop)] ; no next byte within timeout: treat as bare Esc
           [(= second 91) ; [ (we now commit to parsing escape sequence)
            (define third (read-byte/timeout esc-timeout))
            (cond
              [(not third) (loop)] ; no next byte within 25 ms, ignore invalid escape sequence
              [(= third 68) (move-left b 1) (loop)] ; D
              [(= third 67) (move-right b 1) (loop)] ; C
              [else (loop)])]
           [else (interpret-byte second)])] ; not an escape sequence, proceed interpreting second as normal

        ;; printable ASCII
        [(printable? byte)
         (insert! b (integer->char byte))
         (loop)]

        ;; ignore others
        [else (loop)]))
    
    (loop)))


;; ------------------------------------------------------------------
;; demo loop
;; ------------------------------------------------------------------
(define (main)
  (define line (read-line-raw))
  (cond [(void? line) (void)]
        [else (printf "You typed: ~a\n" line) (main)]))

(module+ main
  (main))
