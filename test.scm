(define test
  (lambda (l)
   (cond
     ((null? l) l)
     ((pair? (car l)) (+ (test (car l)) (test (cdr l))))
     (else (+ (car l) (test (cdr l)))))))

(define studd
  (lambda (f)
    (f f)))

(define foo
  (lambda (a)
    (Mergesort a)))

(define merge
  (lambda (l r)
    (cond
      ((null? l) r)
      ((null? r) l)
      ((< (car l) (car r)) (cons (car l) (merge (cdr l) r)))
      (else (cons (car r) (merge l (cdr r)))))))

(define evens
  (lambda (ev l)
    (cond
      ((null? l) l)
      (ev (evens (not ev) (cdr l)))
      (else (cons (car l) (evens (not ev) (cdr l)))))))

(define Mergesort
  (lambda (l)
    (if (or (null? l) (null? (cdr l))) l
        (merge (Mergesort (evens #t l)) (Mergesort (evens #f l))))))

(define stuff
  (lambda (e)
    (cond
      ((null? e) '())
      ((eq? (car (car e)) 'a) (stuff (cdr e)))
      (else ((car (cdr (car e))) (cdr e))))))

(define stuff0
  (lambda (e f)
    ((car f) (cdr e))))

(define stuff1
  (lambda (e)
    ((car (cdr (car e))) (cdr e))))

(define rmap
  (lambda (l v)
    (if (null? l) '()
      (cons ((car l) v) (rmap (cdr l) v)))))
