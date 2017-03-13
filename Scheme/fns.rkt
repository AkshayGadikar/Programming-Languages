#lang racket
;;-*- mode: scheme; -*-
;; :set filetype=scheme

;;Return a 2-element list containing the 2 roots of the quadratic
;;equation a*x^2 + b*x + c = 0 using the classical formula for the
;;roots of a quadratic equation.  The first element in the returned list
;;should use the positive square-root of the discriminant, the second
;;element should use the negative square-root of the discriminant.

;evaluate1 evaluates the square root term
;evaluate2 evaluates the (-b) term
;evaluate3 evaluates the denominator term

(define (quadratic-roots a b c)
  (letrec
      ([evaluate1
        (lambda (a b c)
          (sqrt (- (* b b)(* 4 a c))))])
    (letrec
        ([evaluate2
          (lambda (b)
            (* b -1))])
      (letrec
          ([evaluate3
            (lambda (a)
              (* 2 a))])
        
        (let([term1 (evaluate1 a b c)])
          (let([term2 (evaluate2 b)])
            (let([term3 (evaluate3 a)])
              
              (if(equal? a 0)
                 0
                 (list   (/ (+ term2 term1) term3)
                         (/ (- term2 term1) term3)))))))))) 


;;Return the list resulting by multiplying each element of `list` by `x`.

;mul-aux mulitplies the list , and using accumulator provides list with each element multiplied.
;since acc provides list in reverse order, we apply reverse function ot get in correct order.

(define (mul-list lists x)
  (letrec
      ([mul-aux
        (lambda (acc list  x)
          (if(null? list)
             acc
             (mul-aux (cons (* (car list) x) acc) (cdr list) x)
             ))])
    (letrec
        ([my-reverse
          (lambda (ls)
            (if(null? ls)
               ls
               (append (my-reverse (cdr ls))
                       (list (car ls)))
               ))])
      
      (if(list? lists)
         (my-reverse(mul-aux '() lists x))
         0))))


;;Given a proper-list list of proper-lists, return the sum of the
;;lengths of all the contained lists.

(define (sum-lengths list)
  (if (list? list)
      (if (pair? list)
          (+ (length (car list)) (sum-lengths (cdr list)))
          0)0))


;;Evaluate polynomial with list of coefficients coeffs (highest-order
;;first) at x.  The computation should reflect the traditional
;;representation of the polynomial.

;lenght -1 gives highest order power, which is used as exponential to x, multiplied by first element.
;every iteration provides value and is added explicitly.


(define (poly-eval coeffs x)
  (cond
    [(null? coeffs) 0]
    [else (+(* (expt x (- (length coeffs) 1)) (car coeffs)) (poly-eval (cdr coeffs) x))]))


;;Evaluate polynomial with list of coefficients coeffs (highest-order
;;first) at x using Horner's method.

;evaluate using form (1st coefficient *x)+ 2nd coefficent

(define (poly-eval-horner coeffs x)
  (letrec
      ([poly-eval-horner-aux
        (lambda (acc list x)
          (if (null? list)
              acc
              (poly-eval-horner-aux (+ (* x acc) (car list)) (cdr list) x)))])
    
    (poly-eval-horner-aux 0 coeffs x)))




;;Return count of occurrences equal? to x in exp


(define (count-occurrences exp x)
  (cond 
    [(null? exp) 0]
    [(and (list? (car exp)) (equal? x (car exp))) (+ 1 (count-occurrences (cdr exp) x))]
    [(list? (car exp))(+ (count-occurrences (car exp) x)(count-occurrences (cdr exp) x))]
    [(equal? x (car exp)) (+ 1 (count-occurrences (cdr exp) x))]
    [else (count-occurrences (cdr exp) x)] 
    ))


;;Return result of evaluating arith expression over Scheme numbers
;;with fully parenthesized prefix binary operators 'add, 'sub, 'mul
;;and 'div.

;check for case and implement accordingly

(define (eval-arith exp)
  (letrec
      ([evaluate
        (lambda (exp)
          (cond
            [(list? exp) (eval-arith exp)]
            [else exp]))])
    
    (if(null? exp)
       0
       (begin
         (cond
           
           [(equal? (car exp) 'add)( + (evaluate (cadr exp)) (evaluate (caddr exp)))]
           [(equal? (car exp) 'sub)( - (evaluate (cadr exp)) (evaluate (caddr exp)))]
           [(equal? (car exp) 'mul)( * (evaluate (cadr exp)) (evaluate (caddr exp)))]
           [(equal? (car exp) 'div)( / (evaluate (cadr exp)) (evaluate (caddr exp)))]
           [else 0]))))
  )


;;Given a proper-list list of proper-lists, return sum of lengths of
;;all the contained lists.  Must be tail-recursive.

(define (sum-lengths-tr list)
  (letrec
      ([sum-lengths-tr-aux
        (lambda (acc list)
          (cond
            [(null? list) acc]
            [else
             (sum-lengths-tr-aux (+ (length (car list)) acc) (cdr list))]))]) 
    
    (if (list? list)
        (sum-lengths-tr-aux 0 list)
        0)))


;;Evaluate polynomial with list of coefficients coeffs (highest-order
;;first) at x.  Must be tail-recursive.

(define (poly-eval-tr coeffs x)
  (letrec
      ([aux-poly-eval
        (lambda (acc coeffs  x)
          (if (null? coeffs)
              acc
              (aux-poly-eval (+ (* (expt x (- (length coeffs) 1)) (car coeffs)) acc) (cdr coeffs) x)))])
    
    (if (null? coeffs)
        0
        (aux-poly-eval 0 coeffs x))))


;;Return the list resulting by multiplying each element of `list` by `x`.
;;Cannot use recursion, can use one or more of `map`, `foldl`, or `foldr`.
(define (mul-list-2 list x)
  (if(list? list)
     (begin
       (map (lambda (y) (* x y))
            list))
     0))


;;Given a proper-list list of proper-lists, return the sum of the
;;lengths of all the contained lists.  Cannot use recursion, can use
;;one or more of `map`, `foldl`, or `foldr`.

(define (sum-lengths-2  list)
  
  (if (list? list) 
      (foldl + 0(map (lambda (y) (length y))
                     list))
      0))
