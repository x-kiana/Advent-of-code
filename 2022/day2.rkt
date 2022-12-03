#lang racket

(define (port->lines port)
  (let ([x (read-line port 'any)])
    (cond [(eof-object? x) empty]
          [else
           (cons x (port->lines port))])))

(define rounds (port->lines (open-input-file "day2")))

(define (score_one x)
  (cond [(string=? x "A X") 4]
        [(string=? x "A Y") 8]
        [(string=? x "A Z") 3]
        [(string=? x "B X") 1]
        [(string=? x "B Y") 5]
        [(string=? x "B Z") 9]
        [(string=? x "C X") 7]
        [(string=? x "C Y") 2]
        [(string=? x "C Z") 6]))

(define (score_two x)
  (cond [(string=? x "A X") 3]
        [(string=? x "A Y") 4]
        [(string=? x "A Z") 8]
        [(string=? x "B X") 1]
        [(string=? x "B Y") 5]
        [(string=? x "B Z") 9]
        [(string=? x "C X") 2]
        [(string=? x "C Y") 6]
        [(string=? x "C Z") 7]))

(define solution1 (foldr + 0 (map score_one rounds)))
(define solution2 (foldr + 0 (map score_two rounds)))

