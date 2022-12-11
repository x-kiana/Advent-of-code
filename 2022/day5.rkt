#lang racket
(define (port->lines port)
  (let ([x (read-line port 'any)])
    (cond [(eof-object? x) empty]
          [else
           (cons x (port->lines port))])))

(define input (port->lines (open-input-file "day5")))
(define non-transposed-boxes (map string->list
                                  (take input (index-of input ""))))
(define instructions (drop input (+ 1 (index-of input ""))))

(define (last-elm-is-number? list)
  (char-numeric? (car (list-tail list (- (length list) 1)))))

(define (parse-instruction x)
  (map string->number
       (list (list-ref x 1)
             (list-ref x 3)
             (list-ref x 5))))

(define (summarize instructions)
  (map parse-instruction
       (map string-split instructions)))

(define transposed-boxes (filter last-elm-is-number?
                                 (apply map list non-transposed-boxes)))

(define boxes (map (lambda (x)
                     (filter (lambda (x)
                               (not (or (= (char->integer x) 32)
                                        (char-numeric? x)))) x))
                   transposed-boxes))

(define (perform-instruction fn ins boxes)
  (let* ([from-column (list-ref boxes (- (second ins) 1))]
         [new-from-column
          (drop (list-ref boxes (- (second ins) 1)) (first ins))]
         [to-column (list-ref boxes (- (third ins) 1))]
         [new-to-column (append (fn (take from-column
                                               (first ins))) to-column)])
    (list-set
     (list-set boxes (- (second ins) 1) new-from-column)
     (- (third ins) 1)
     new-to-column)))

(define (perform-instructions fn instructions boxes)
  (if (empty? instructions)
      boxes
      (perform-instructions
       fn
       (rest instructions)
       (fn (car instructions) boxes))))

(define (perform-instruction1 ins boxes)
  (perform-instruction reverse ins boxes))

(define (perform-instruction2 ins boxes)
  (perform-instruction identity ins boxes))

(define (perform-instructions1 ins boxes)
  (perform-instructions perform-instruction1 ins boxes))

(define (perform-instructions2 ins boxes)
  (perform-instructions perform-instruction2 ins boxes))

(define solution1 (perform-instructions1 (summarize instructions) boxes))
                
(define solution2 (perform-instructions2 (summarize instructions) boxes))