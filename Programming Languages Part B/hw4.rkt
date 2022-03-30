#lang racket

(provide (all-defined-out))

(define (sequence low high stride)
  (if (> low high)
      null
      (cons low (sequence (+ low stride) high stride))))


(define (string-append-map xs suffix)
  (map (lambda (x) (string-append x suffix)) xs))


(define (list-nth-mod xs n)
  (cond [(negative? n) (error "list-nth-mod: negative number")]
        [(null? xs) (error "list-nth-mod: empty list")]
        [#t (letrec ([tgt (remainder n (length xs))]
                     [f (lambda (i) (if (= tgt i) (car (list-tail xs i)) (f (+ i 1))))])
            (f 0))]))


(define (stream-for-n-steps s n)
  (letrec ([f (lambda (s i) (if (= i 0) null (cons (car (s)) (f (cdr (s)) (- i 1)))))])
    (f s n)))


(define funny-number-stream
  (letrec ([f (lambda (x)
                (let ([n (if (= (remainder x 5) 0) (- x) x)])
                  (cons n (lambda () (f (+ x 1))))))])
    (lambda () (f 1))))


(define dan-then-dog
  (letrec ([reverse-pair (lambda (p) (let ([x (car p)][y (cdr p)]) (cons y x)))]
           [f (lambda (p) (cons (car p) (lambda () (f (reverse-pair p)))))])
    (lambda () (f (cons "dan.jpg" "dog.jpg")))))
                    
                
(define (stream-add-zero s)
  (letrec ([f (lambda (x) (cons (cons 0 (car (x))) (lambda() (f (cdr(x))))))])
    (lambda () (f s))))


(define (cycle-lists xs ys)
  (letrec ([f (lambda (n)
                (cons (cons (list-nth-mod xs n) (list-nth-mod ys n)) (lambda () (f (+ n 1)))))])
    (lambda () (f 0))))


(define (vector-assoc v vec)
 (letrec ([f (lambda (i)
                 (if (= i (vector-length vec))
                     #f
                     (let ([p (vector-ref vec i)])
                       (cond [(not (pair? p)) (f (+ i 1))]
                             [(equal? (car p) v) p]
                             [#t (f (+ i 1))]))))])
    (f 0)))


(define (cached-assoc xs n)
  (letrec ([cache (make-vector n)]
           [index 0])
    (lambda (v)
      (let ([p (vector-assoc v cache)])
        (if p p
            (begin (set! p (assoc v xs))
                   (vector-set! cache (remainder index n) p)
                   (set! index (remainder (+ index 1) n))
                   p))))))    


(define-syntax while-less
  (syntax-rules (do)
    [(while-less e1 do e2)
     (letrec ([i e1]
              [loop (lambda ()
                      (if (>= e2 i)
                      #t
                      (loop)))])
       (loop))]))


          
        
    


  