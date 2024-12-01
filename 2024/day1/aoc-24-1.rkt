#lang racket
(define (port->lines port)
  (let ([x (read-line port 'any)])
    (cond [(eof-object? x) empty]
          [else
           (cons x (port->lines port))])))

(define input (port->lines (open-input-file "input-1.txt")))

(define list1 (sort
               (map string->number
                    (map (lambda (x) (substring x 0 5)) input))
               <))

(define list2 (sort
               (map string->number
                    (map (lambda (x) (substring x 8)) input))
               <))

(define soln1 (foldr + 0
                     (map (lambda (x y) (abs (- x y)))
                          list1 list2)))

(define soln2 (foldr + 0
       (map (lambda (x)
              (* x (length (filter (lambda (y) (= y x)) list2))))
            list1)))