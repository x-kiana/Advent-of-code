#lang racket
(define (port->lines port)
  (let ([x (read-line port 'any)])
    (cond [(eof-object? x) empty]
          [else
           (cons x (port->lines port))])))

(define input (port->lines (open-input-file "aoc-23-1.txt")))

(define (extract-digits s)
  (list->string (filter char-numeric? (string->list s))))

(define (calib s)
  (string-append (substring s 0 1) (substring s (- (string-length s) 1))))

(define (sol1 input)
  (foldr + 0
         (map string->number
              (map calib
                   (map extract-digits input)))))

(define ans1 (sol1 input))

(define (replace s)
  (string-replace
   (string-replace
    (string-replace
     (string-replace
      (string-replace
       (string-replace
        (string-replace
         (string-replace
          (string-replace
           s
           "one" "one1one")
          "two" "two2two")
         "three" "three3three")
        "four" "four4four")
       "five" "five5five")
      "six" "six6six")
     "seven" "seven7seven")
    "eight" "eight8eight")
   "nine" "nine9nine"))
     
  
(define (sol2 input)
  (sol1 (map replace input)))

(define ans2 (sol2 input))