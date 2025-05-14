#lang racket/base

(define ansi-styles
  '((reset         . "\e[0m")

    ;; Turn on styles
    (bold          . "\e[1m")
    (dim           . "\e[2m")
    (italic        . "\e[3m")
    (underline     . "\e[4m")
    (blink         . "\e[5m")
    (rapid-blink   . "\e[6m")
    (reverse       . "\e[7m")
    (hidden        . "\e[8m")
    (strikethrough . "\e[9m")

    ;; Turn off styles
    (bold-off           . "\e[22m")
    (dim-off            . "\e[22m") ; shared code
    (italic-off         . "\e[23m")
    (underline-off      . "\e[24m")
    (blink-off          . "\e[25m")
    (reverse-off        . "\e[27m")
    (hidden-off         . "\e[28m")
    (strikethrough-off  . "\e[29m")

    ;; Foreground colors
    (black   . "\e[30m")
    (red     . "\e[31m")
    (green   . "\e[32m")
    (yellow  . "\e[33m")
    (blue    . "\e[34m")
    (magenta . "\e[35m")
    (cyan    . "\e[36m")
    (white   . "\e[37m")
    (default . "\e[39m")

    ;; Bright foreground colors
    (bright-black   . "\e[90m")
    (bright-red     . "\e[91m")
    (bright-green   . "\e[92m")
    (bright-yellow  . "\e[93m")
    (bright-blue    . "\e[94m")
    (bright-magenta . "\e[95m")
    (bright-cyan    . "\e[96m")
    (bright-white   . "\e[97m")

    ;; Background colors
    (bg-black   . "\e[40m")
    (bg-red     . "\e[41m")
    (bg-green   . "\e[42m")
    (bg-yellow  . "\e[43m")
    (bg-blue    . "\e[44m")
    (bg-magenta . "\e[45m")
    (bg-cyan    . "\e[46m")
    (bg-white   . "\e[47m")
    (bg-default . "\e[49m")

    ;; Bright background colors
    (bg-bright-black   . "\e[100m")
    (bg-bright-red     . "\e[101m")
    (bg-bright-green   . "\e[102m")
    (bg-bright-yellow  . "\e[103m")
    (bg-bright-blue    . "\e[104m")
    (bg-bright-magenta . "\e[105m")
    (bg-bright-cyan    . "\e[106m")
    (bg-bright-white   . "\e[107m")))

(define (ansi-style spec)
  (cond
    ;; Simple symbol: lookup in ansi-styles (assumed to be complete)
    [(symbol? spec)
     (let ([entry (assoc spec ansi-styles)])
       (if entry
         (cdr entry)
         (error "Unknown ANSI style symbol" spec)))]

    ;; Tagged list: (fg value) or (bg value)
    [(and (list? spec) (member (car spec) '(fg bg)))
     (let ([mode (car spec)]
           [value (cadr spec)])
       (cond
         ;; 256-color index: e.g. (fg 208)
         [(integer? value)
          (format (if (eq? mode 'bg)
                      "\e[48;5;~am"
                      "\e[38;5;~am")
                  value)]

         ;; RGB color: e.g. (bg (255 100 0))
         [(and (list? value) (= (length value) 3) (andmap integer? value))
          (apply
           (lambda (r g b)
             (format (if (eq? mode 'bg)
                         "\e[48;2;~a;~a;~am"
                         "\e[38;2;~a;~a;~am")
                     r g b))
           value)]

         [else
          (error "Invalid tagged style value" spec)]))]

    [else
     (error "Invalid ANSI style specifier" spec)]))


(define (styled str . styles)
  (string-append (apply string-append (map ansi-style styles)) str (ansi-style 'reset)))


(display (styled "Hello world" 'bold 'yellow 'underline))
(display (styled "Hello world" '(bg 0 99 99) 'reverse))
(newline)
(display (styled "█████████████████████████████" 'blink 'bg-red 'yellow))


