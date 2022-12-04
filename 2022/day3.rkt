#lang racket
(define (port->lines port)
  (let ([x (read-line port 'any)])
    (cond [(eof-object? x) empty]
          [else
           (cons x (port->lines port))])))

(define rucksacks (port->lines (open-input-file "day3")))

(define compartments
  (map (lambda (x) (list (substring x 0 (/ (string-length x) 2))
                         (substring x (/ (string-length x) 2)
                                    (string-length x))))
       rucksacks))

(define items (map (lambda (x) (list (string->list (car x))
                                     (string->list (cadr x))))
                   compartments))

(define (in-both x l1 l2)
  (if (and (and (member x l1) #t) (and (member x l2) #t))
      x
      #f))

(define (get-shared l1 l2)
  (cond [(or (empty? l1) (empty? l2)) empty]
        [else
         (if (in-both (car l1) l1 l2) 
             (cons (car l1) (get-shared (rest l1) l2))
             (get-shared (rest l1) l2))]))

(define same-items (map (lambda (x) (get-shared (car x) (cadr x))) items))

(define (score x)
  (if (char-lower-case? x)
      (- (char->integer x) 96)
      (- (char->integer x) 38)))

(define (list-score l)
  (if (empty? l)
      0
      (score (car l))))

(define solution1 (foldr + 0 (map list-score same-items)))

(define (group l)
  (cond [(empty? l) empty]
        [else
         (cons (list (car l) (cadr l) (caddr l)) (group (cdddr l)))]))

(define elf-groups (group (map string->list rucksacks)))

(define (get-shared-2 l1 l2 l3)
  (cond [(or (empty? l1) (empty? l2) (empty? l3)) empty]
        [else
         (if (and (in-both (car l1) l1 l2) (in-both (car l1) l1 l3))
             (cons (car l1) (get-shared-2 (rest l1) l2 l3))
             (get-shared-2 (rest l1) l2 l3))]))

(define badge-numbers (map (lambda (x)
                                  (get-shared-2
                                   (car x)
                                   (cadr x)
                                   (caddr x))) elf-groups))

(define solution2 (foldr + 0 (map list-score badge-numbers)))

