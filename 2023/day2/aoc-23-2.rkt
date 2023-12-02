#lang racket
(define (port->lines port)
  (let ([x (read-line port 'any)])
    (cond [(eof-object? x) empty]
          [else
           (cons x (port->lines port))])))

(define input (port->lines (open-input-file "aoc-23-2.txt")))

(define (break-down g)
  (let [(game (map (λ (y) (string-replace y " " ""))
                   (flatten (map (λ (x) (string-split x ",")) g))))]
    (append (string-split (first game) ":") (rest game))))

(define (valid-move? m)
  (cond
    [(string-suffix? m "red")
     (<= (string->number (substring m 0 (- (string-length m) 3))) 12)]
    [(string-suffix? m "blue")
     (<= (string->number (substring m 0 (- (string-length m) 4))) 14)]
    [(string-suffix? m "green")
     (<= (string->number (substring m 0 (- (string-length m) 5))) 13)]
    [else #t]))

(define (valid-game-n g)
  (if (andmap valid-move? g)
      (string->number (substring (first g) 4))
      0))

(define (sol1 input)
  (foldr + 0
         (map valid-game-n
              (map break-down
                   (map (λ (x) (string-split x ";")) input)))))

(define ans1 (sol1 input))

(define (red-n m)
  (cond
    [(string-suffix? m "red")
     (string->number (substring m 0 (- (string-length m) 3)))]
    [else 0]))

(define (blue-n m)
  (cond
    [(string-suffix? m "blue")
     (string->number (substring m 0 (- (string-length m) 4)))]
    [else 0]))

(define (green-n m)
  (cond
    [(string-suffix? m "green")
     (string->number (substring m 0 (- (string-length m) 5)))]
    [else 0]))

(define (find-power g)
  (local
    [(define (max a b)
       (if (> a b) a b))]
    (* (foldr max 0 (map red-n g))
       (foldr max 0 (map blue-n g))
       (foldr max 0 (map green-n g)))))

(define (sol2 input)
  (foldr + 0
         (map find-power
              (map break-down
                   (map (λ (x) (string-split x ";")) input)))))

(define ans2 (sol2 input))