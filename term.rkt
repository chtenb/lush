#lang racket/base
(require termconfig)

;; The given with-raw doesn't seem to be robust w.r.t. exceptions
(define-syntax-rule
  (with-raw body ...)
  (dynamic-wind enable-raw (λ () body ...) disable-raw))

(provide (all-defined-out))
