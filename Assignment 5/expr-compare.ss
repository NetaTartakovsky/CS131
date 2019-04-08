#lang scheme

(define lambda-symbol (string->symbol "\u03BB"))

; checks length of x
(define (lngth x)
  (cond ((null? x) 0)
        ((or (list? x) (pair? x)) (+ 1 (lngth (cdr x))))
        (else 1)))

; checks if x matches one of the keywords
(define (keyword x) (if (or (or (or (or (equal? x 'if) (equal? x 'quote))
                                    (equal? x 'lambda))
                                (equal? x lambda-symbol))
                            (equal? x 'let)) #t #f))

; returns symbol x!y
(define (combine x y) (string->symbol (string-append (symbol->string x) "!" (symbol->string y))))

(define (lambda-check x y)
  (cond ((or (empty? x) (empty? y)) empty)
        ((symbol? x) (if (equal? x y) x (combine x y)))
        ((equal? (car x) (car y)) (cons (car x) (lambda-check (cdr x) (cdr y))))
        (else
         (let ([r (combine (car x) (car y))])
          (cons r (lambda-check (cdr x) (cdr y)))))))

; replaces all elements in lst that match e with r
(define (update r e lst)
  (cond ((empty? lst) empty)
        ((< (lngth lst) 2) (if (symbol? lst)
                               (if (equal? lst e) r lst)
                               (if (equal? lst (list e)) (list r) lst)))
        ((equal? (car lst) e) (cons r (update r e (cdr lst))))
        (else (cons (car lst) (update r e (cdr lst))))))

; uses new arg list to update the lambda body
(define (lambda-update f1 f2 b)
  (cond ((empty? f1) b)
        ((symbol? f1) (if (equal? f1 f2) b (update f2 f1 b)))
        ((not (equal? (car f1) (car f2))) (lambda-update (cdr f1) (cdr f2) (update (car f2) (car f1) b)))
        (else (lambda-update (cdr f1) (cdr f2) b))))

; splits up arg list and body, deals with each accordingly
(define (lambda-helper x y)
  (cond ((not (equal? (lngth x) (lngth y))) (quasiquote (if % (unquote x) (unquote y))))
        (else (let ([r (lambda-check (car x) (car y))])
         (list r (expr-compare (lambda-update (car x) r (car (cdr x))) (lambda-update (car y) r (car (cdr y)))))))))

; checks arg lists and lambda formats
(define (lambda-function x y)
  (cond ((not (equal? (lngth (car (cdr x))) (lngth (car (cdr y))))) (quasiquote (if % (unquote x) (unquote y))))
        ((or (and (not (list? (car (cdr x)))) (list? (car (cdr y)))) (and (not (list? (car (cdr y)))) (list? (car (cdr x)))))
         (quasiquote (if % (unquote x) (unquote y))))
        ((equal? (car x) (car y)) (if (equal? (car x) 'lambda)
                                      (cons 'lambda (lambda-helper (cdr x) (cdr y)))
                                      (cons lambda-symbol (lambda-helper (cdr x) (cdr y)))))
        (else (cons lambda-symbol (lambda-helper (cdr x) (cdr y))))))

; sorts through list and deals with each type accordingly
(define (helper x y)
  (cond ((equal? x y) x)
        ((equal? x #t) (equal? y #f) '%)
        ((equal? x #f) (equal? y #t) '(not %))
        ((not (equal? (lngth x) (lngth y))) (quasiquote (if % (unquote x) (unquote y))))
        ((and (list? x) (list? y))
         (if (equal? (car x) (car y))
             (cond ((equal? (car x) 'quote) (quasiquote (if % (unquote x) (unquote y))))
                   ((equal? (car x) 'lambda) (lambda-function x y))
                   ((equal? (car x) lambda-symbol) (lambda-function x y))
                   (else (cons (helper (car x) (car y)) (helper (cdr x) (cdr y)))))
             (cond ((and (equal? (car x) 'lambda) (equal? (car y) lambda-symbol)) (lambda-function x y))
                   ((and (equal? (car x) lambda-symbol) (equal? (car y) 'lambda)) (lambda-function x y))
                   ((or (keyword (car x)) (keyword (car y)))
                    (quasiquote (if % (unquote x) (unquote y))))
                   (else (cons (helper (car x) (car y)) (helper (cdr x) (cdr y)))))))
        (else (quasiquote (if % (unquote x) (unquote y))))))
 
(define (expr-compare x y)
  (cond ((equal? x y) x)
        (else (helper x y))))

; test expressions for test-compare-expr
(define test-expr-x '(1 #f (lambda (a b c) (f a d)) (eq? + '(lambda a a) (if x y z))))
(define test-expr-y '(2 #t (λ (d b a) (f d e)) (eq? + '(lambda b b) (g x y z))))

(define-namespace-anchor a)
(define ns (namespace-anchor->namespace a))

(define (replace b z)
  (cond ((empty? z) empty)
        ((list? (car z)) (cons (replace b (car z)) (replace b (cdr z))))
        ((equal? (car z) '%) (cons b (replace b (cdr z))))
        ((equal? (car z) lambda-symbol) (cons 'lambda (replace b (cdr z))))
        (else (cons (car z) (replace b (cdr z))))))

(define (test-expr-compare x y)
  (and (equal? (eval x ns) (eval (replace #t x) ns))
       (equal? (eval y ns) (eval (replace #f y) ns))))
