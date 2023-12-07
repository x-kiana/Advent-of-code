#lang racket
(define (port->lines port)
  (let ([x (read-line port 'any)])
    (cond [(eof-object? x) empty]
          [else
           (cons x (port->lines port))])))

(define input (port->lines (open-input-file "aoc-23-6.txt")))

(define (beats t0 dist)
  (local
    [(define (beats-t t dist res)
       (cond
         [(zero? t) res]
         [else
          (if (> (* (- t0 t) t) dist)
              (beats-t (sub1 t) dist (add1 res))
              (beats-t (sub1 t) dist res))]))]
    (beats-t t0 dist 0)))

(define (beats-score l1 l2)
  (cond
    [(or (empty? l1) (empty? l2)) 1]
    [else
     (* (beats (first l1) (first l2))
        (beats-score (rest l1) (rest l2)))]))

(define processed
  (map (λ (z) (map string->number z))
       (map (λ (y) (rest (filter non-empty-string? y)))
            (map (λ (x) (string-split x " ")) input))))

(define ans1 (beats-score (car processed) (cadr processed)))

(define processed-pt2
  (map string->number
       (map list->string
            (map flatten
                 (map (λ (z) (map string->list z))
                      (map (λ (y) (rest (filter non-empty-string? y)))
                           (map (λ (x) (string-split x " ")) input)))))))

(define ans2 (beats (car processed-pt2) (cadr processed-pt2)))

