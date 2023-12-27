#lang racket
(define (port->lines port)
  (let ([x (read-line port 'any)])
    (cond [(eof-object? x) empty]
          [else
           (cons x (port->lines port))])))

(define input (port->lines (open-input-file "aoc-23-8.txt")))

(define (zero-diff? l)
  (cond
    [(or (empty? l)
         (= (length l) 1)) #t]
    [else
     (if
      (= (car l) (cadr l))
      (zero-diff? (cdr l))
      #f)]))

(define (subtract-pred l x)
  (cond
    [(or (empty? l)
         (= (length l) 1))
     (reverse x)]
    [else
     (subtract-pred (cdr l) (cons (- (cadr l) (car l)) x))]))

(define (predict l)
  (cond
    [(empty? l) 0]
    [else
     (if (zero-diff? l)
         (last l)
         (+ (predict (subtract-pred l '())) (last l)))]))    

(define (process-1 input)
  (foldr + 0
         (map predict
              (map (λ (y) (map string->number y))
                   (map (λ (x) (string-split x " ")) input)))))


(define (extrapolate l)
  (cond
    [(empty? l) 0]
    [else
     (if (zero-diff? l)
         (car l)
         (- (car l) (extrapolate (subtract-pred l '()))))]))

(define (process-2 input)
  (foldr + 0
         (map extrapolate
              (map (λ (y) (map string->number y))
                   (map (λ (x) (string-split x " ")) input)))))