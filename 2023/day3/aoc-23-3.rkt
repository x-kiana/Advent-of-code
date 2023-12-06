#lang racket
(define (port->lines port)
  (let ([x (read-line port 'any)])
    (cond [(eof-object? x) empty]
          [else
           (cons x (port->lines port))])))

(define input (port->lines (open-input-file "aoc-23-3.txt")))

(define DOT +inf.0)
  
(define (calc-coordinates s y)
  (local [(define (calc s x y l)
            ;; l is (list (list number number number) (listof char))
            (cond
              [(empty? s) l]
              [(char=? #\. (car s))
               (calc (cdr s) (+ 1 x) y (cons (list DOT DOT 0) l))]
              [(char-numeric? (car s))
               (if (char-numeric? (cadr s))
                   (if (char-numeric? (caddr s))
                       (calc (cdddr s) (+ 3 x) y
                             (cons (list (list x y 3) (take s 3)) l))
                       (calc (cddr s) (+ 2 x) y
                             (cons (list (list x y 2) (take s 2)) l)))
                   (calc (cdr s) (+ 1 x) y
                         (cons (list (list x y 1) (take s 1)) l)))]
              [else
               (calc (cdr s)
                     (+ 1 x) y
                     (cons (list (list x y 1) '()) l))]))]
    (calc s 0 y empty)))

(define (calc-all in y)
  (cond
    [(empty? in) empty]
    [else
     (cons
      (calc-coordinates (car in) y)
      (calc-all (cdr in) (+ 1 y)))]))

(define (sym-in-surroundings? x l)
  (let ([x-coord (caar x)]
        [y-coord (cadar x)])  
    (cond
      [(= 1 (length (cadr x)))
       (or (member (list (list x-coord (- y-coord 1) 1) '()) l)
           (member (list (list (- x-coord 1) (- y-coord 1) 1) '()) l)
           (member (list (list (+ x-coord 1) (- y-coord 1) 1) '()) l)
           (member (list (list x-coord (+ y-coord 1) 1) '()) l)
           (member (list (list (- x-coord 1) (+ y-coord 1) 1) '()) l)
           (member (list (list (+ x-coord 1) (+ y-coord 1) 1) '()) l)
           (member (list (list (- x-coord 1) y-coord 1) '()) l)
           (member (list (list (+ x-coord 1) y-coord 1) '()) l))]
      [(= 2 (length (cadr x)))
       (or (member (list (list x-coord (- y-coord 1) 1) '()) l)
           (member (list (list (- x-coord 1) (- y-coord 1) 1) '()) l)
           (member (list (list (+ x-coord 1) (- y-coord 1) 1) '()) l)
           (member (list (list (+ x-coord 2) (- y-coord 1) 1) '()) l)
           (member (list (list x-coord (+ y-coord 1) 1) '()) l)
           (member (list (list (- x-coord 1) (+ y-coord 1) 1) '()) l)
           (member (list (list (+ x-coord 1) (+ y-coord 1) 1) '()) l)
           (member (list (list (+ x-coord 2) (+ y-coord 1) 1) '()) l)
           (member (list (list (- x-coord 1) y-coord 1) '()) l)
           (member (list (list (+ x-coord 2) y-coord 1) '()) l))]
      [(= 3 (length (cadr x)))
       (or (member (list (list x-coord (- y-coord 1) 1) '()) l)
           (member (list (list (- x-coord 1) (- y-coord 1) 1) '()) l)
           (member (list (list (+ x-coord 1) (- y-coord 1) 1) '()) l)
           (member (list (list (+ x-coord 2) (- y-coord 1) 1) '()) l)
           (member (list (list (+ x-coord 3) (- y-coord 1) 1) '()) l)
           (member (list (list x-coord (+ y-coord 1) 1) '()) l)
           (member (list (list (- x-coord 1) (+ y-coord 1) 1) '()) l)
           (member (list (list (+ x-coord 1) (+ y-coord 1) 1) '()) l)
           (member (list (list (+ x-coord 2) (+ y-coord 1) 1) '()) l)
           (member (list (list (+ x-coord 3) (+ y-coord 1) 1) '()) l)
           (member (list (list (- x-coord 1) y-coord 1) '()) l)
           (member (list (list (+ x-coord 3) y-coord 1) '()) l))])))

(define (check-in-all-syms? x syms)
  (cond
    [(empty? syms) #f]
    [else
     (if (sym-in-surroundings? x (first syms))
         #t
         (check-in-all-syms? x (rest syms)))]))

(define (sol1 input)
  (let* ([processed
          (map (λ (a) (filter (λ (x) (not (equal? x (list DOT DOT 0)))) a))
               (calc-all (map string->list input) 0))]
         [nums (map (λ (y)
                      (filter (λ (x) (not (empty? (cadr x)))) y)) processed)]
         [syms (map (λ (y)
                      (filter (λ (x) (empty? (cadr x))) y)) processed)])
    (foldr + 0
           (flatten
            (map (λ (y)
                   (map (λ (x) (string->number (list->string (cadr x)))) y))
                 (map (λ (y)
                        (filter (λ (x) (check-in-all-syms? x syms)) y))
                      nums))))))

(define ans1 (sol1 input))
