#lang racket
(require termconfig)   ; gives us (with-raw ...) cross‑platform
(require soup-lib)
(require racket/format)

;; ------------------------------------------------------------------
;; helpers (manual ANSI ⎯ no extra libs)
;; ------------------------------------------------------------------
(define (fg256 n) (format "\e[38;5;~am" n))
(define reset "\e[0m")
(define prompt (string-append (fg256 208) "λ " reset))

(define (cursor-left n)
  (if (> n 0) (format "\e[~aD" n) (quote "")))
(define (cursor-right n)
  (if (> n 0) (format "\e[~aC" n) (quote "")))


;; ------------------------------------------------------------------
;; mutable buffer
;; ------------------------------------------------------------------
(struct buf (vec pos len) #:transparent #:mutable)
(define (make-buf size) (buf (make-vector size #\space) 0 0))

;; prompt is something like "\e[38;5;208mλ \e[0m"
(define (redraw b)
  (define visible
    (list->string
      (vector->list (vector-copy (buf-vec b) 0 (buf-len b)))))

  (define out
    (string-append
      "\e[?25l"                       ; 1. hide cursor
      "\r" prompt                     ; 2. return & prompt
      visible                         ; 3. user text
      "\e[K"                          ; 4. clear to EOL (important when shrinking)
      (cursor-left (- (buf-len b) (buf-pos b))) ; 5. back‑up to cursor
      "\e[?25h"))                     ; 6. show cursor again

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
  (with-raw                            ; termconfig toggles raw mode
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
