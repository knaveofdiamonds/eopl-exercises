;; usage : Int x SchemeVal -> Listof(SchemeVal)
(define duple
  (lambda (n object)
    (if (eq? n 0) '()
        (cons object (duple (- n 1) object)))))

;; invert : Listof(Pair) -> Listof(Pair)
(define invert
  (lambda (lop)
    (if (null? lop) '()
        (cons (invert-pair (car lop)) (invert (cdr lop))))))

;; invert-pair : Pair -> Pair
(define invert-pair
  (lambda (pair)
    (cons (cadr pair) (cons (car pair) ()))))

;; down : Listof(SchemeVal) -> Listof(Listof(SchemeVal))
(define down
  (lambda (lst)
    (if (null? lst) '()
        (cons (cons (car lst) ()) (down (cdr lst))))))

;; swapper : SchemeVal x ScemeVal x Listof(SchemeVal) -> Listof(SchemeVal)
(define swapper
  (lambda (a b lst)
    (if (null? lst) '()
        (cond
          ((list? (car lst)) (cons (swapper a b (car lst)) (swapper a b (cdr lst))))
          ((eq? a (car lst)) (cons b (swapper a b (cdr lst))))
          ((eq? b (car lst)) (cons a (swapper a b (cdr lst))))
          (else
           (cons (car lst) (swapper a b (cdr lst))))))))

;; list-set : Listof(SchemeVal) x Int x SchemeVal -> Listof(SchemeVal)
(define list-set
  (lambda (lst n obj)
    (cond
      ((null? lst) '())
      ((eq? n 0) (cons obj (cdr lst)))
      (else (cons (car lst) (list-set (cdr lst) (- n 1) obj))))))

;; count-occurrences : SchemeVal x Listof(SchemeVal) -> Int
(define count-occurrences
  (lambda (obj lst)
    (cond
      ((null? lst) 0)
      ((list? (car lst)) (+ (count-occurrences obj (car lst)) (count-occurrences obj (cdr lst))))
      ((eq? obj (car lst)) (+ 1 (count-occurrences obj (cdr lst))))
      (else (count-occurrences obj (cdr lst))))))
                       
                       
          