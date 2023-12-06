#lang racket
(define (port->lines port)
  (let ([x (read-line port 'any)])
    (cond [(eof-object? x) empty]
          [else
           (cons x (port->lines port))])))

(define input (port->lines (open-input-file "aoc-23-4.txt")))

(define (find-matching-nums l1 l2)
  (cond
    [(or (empty? l1) (empty? l2)) empty]
    [else
     (if (member (first l1) l2)
         (cons (first l1)
               (find-matching-nums (rest l1) l2))
         (find-matching-nums (rest l1) l2))]))

(define ans1
  (foldr
   + 0
   (map
    (λ (d) (expt 2 (- (length d) 1)))
    (filter
     (λ (c) (not (empty? c)))
     (map
      (λ (b) (find-matching-nums (car b) (cadr b)))
      (map
       (λ (a) (let-values ([(x y) (split-at a 10)]) (cons x (list y))))
       (map
        (λ (z) (filter non-empty-string? z))
        (map
         (λ (y) (string-split y " "))
         (flatten
          (map
           rest
           (map
            (λ (x) (string-split x ":")) input)))))))))))


;; chester taught me how to pipe :)

(define (pipe . funs)
  (match funs
    ['() (λ (x) x)]
    [(cons f funs)
     (λ (x) ((apply pipe funs) (f x)))]))

((pipe
  (curry map (λ (x) (string-split x ":")))
  (curry map rest)
  flatten
  (curry map (λ (y) (string-split y " ")))
  (curry map (λ (z) (filter non-empty-string? z)))
  (curry map (λ (a) (let-values ([(x y) (split-at a 10)]) (cons x (list y)))))
  (curry map (λ (b) (find-matching-nums (car b) (cadr b))))
  (curry filter (λ (c) (not (empty? c))))
  (curry map (λ (d) (expt 2 (- (length d) 1))))
  (curry foldr + 0))
 input)