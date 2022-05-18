;Team members: MD Hossain, Eftekher Husain

;Problem: 2
;Change the representation of bindings in the system to explicit pairs of the form (name value)


;Design Idea:
;we have seen that an entry is pair of lists
;where the first list is set
;we called then names and values to different them

;our goal is change the representation of bindings in the system to explicit pairs of the form (name value) without affecting the working of the TLS interpreter

;specifically our changed subsystem must also satify the specifications.

;previously when a call like '((lambda (x y z) (* x y z)) 1 2 3) results in a entry ((x y z)(1 2 3)) where x is name1, 1 is value1
;y is name2, 2 is value2 and z is name3, 3 is value3

;what we want is to make some changes so that the result of entry is of the form ((x 1) (y 2) (z 3)) instea of the form ((x y z)(1 2 3))


;idea
;since names is a set and vals are list of values where
;(car names), name corresponds of (car vals), value, we can
;modify the the entry to be in the form
;(cons (car names) (cons (car values) '()) which would create a pair

;(cons (cons (car names) (cons (car my-values) '())) '())
;or simply (build (car names) (car values))
;and at each call we cdr down names and values at the same time

;once we have an entry with the form ((x 1) (y 2) (z 3)), call it modifier-entry we could use the helper function firsts to extact all the names in a list and seconds to exacts all the values in a
; a list

;we than make some changes to lookup-in-entry where old names
;become (names (modified-entry entry))
;(vals (modified-entry entry)) where are now working with the names and values of the modified-entry

;keep in mind we have now change the workings of our interpreter.


; auxiliary functions

(define build
  (lambda (s1 s2)
    (cons s1 (cons s2 (quote ())))))

(define first car)

(define second cadr)

(define third caddr)

(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))



; environments implemented as tables


(define modified-entry
  (lambda (entry)
    (modified-entry-help (first entry)(second entry))))

(define (modified-entry-help names values)
  (cond
    ((null? names) '())
    (else (cons (build (first names) (first values))
                (modified-entry-help (cdr names) (cdr values))))))
;testing
;(modified-entry '((x y z) (1 2 3)))
;(define modified-entry1 (modified-entry '((x y z) (1 2 3))))

(define (firsts l)
  (cond ((null? l) '())
        (else (cons (caar l) (firsts (cdr l))))))

;(firsts (modified-entry '((x y z) (1 2 3))))


(define (seconds l)
  (cond ((null? l) '())
        (else (cons (car (cdr (car l))) (seconds (cdr l))))))

;(seconds (modified-entry '((x y z) (1 2 3))))



(define entry1 '((x y z)(1 2 3)))
(define entry2 '((a b c)(4 5 6)))
;entry1

;(define table1 (list entry1 entry2))
;table1

(define lookup-in-table
  (lambda (name table table-f)
    (cond 
      ((null? table) (table-f name))
      (else (lookup-in-entry name
                             (car table)
                             (lambda (name)
                               (lookup-in-table name
                                                (cdr table)
                                                table-f)))))))

(define extend-table cons)



(define lookup-in-entry
  (lambda (name entry entry-f)
    (lookup-in-entry-help name
                          (names (modified-entry entry))
                          (vals (modified-entry entry))
                          entry-f)))



(define lookup-in-entry-help
  (lambda (name names vals entry-f)
    (cond
      ((null? names) (entry-f name))
      ((eq? (car names) name) (car vals))
      (else (lookup-in-entry-help name
                                  (cdr names)
                                  (cdr vals)
                                  entry-f)))))




(define new-entry build)

(define names
  (lambda (entry) (firsts entry)))

(define vals
  (lambda (entry) (seconds entry)))

;testing
;(names modified-entry1)
;(vals modified-entry1)

;(lookup-in-entry 'z entry1 "error")

; the top level of the interpreter

(define value
  (lambda (e)
    (meaning e (quote () ))))


(define meaning
  (lambda (e table)
    ((expression-to-action e) e table)))


; supporting functions for the intepeter

; syntax-directed dispatch on expression

(define expression-to-action
  (lambda (e)
    (cond 
      ((atom? e) (atom-to-action e))
      (else (list-to-action e)))))

(define atom-to-action
  (lambda (e)
    (cond
      ((number? e) *const)
      ((eq? e #t) *const)
      ((eq? e #f) *const)
      ((eq? e (quote cons)) *const)
      ((eq? e (quote car)) *const)
      ((eq? e (quote cdr)) *const)
      ((eq? e (quote null?)) *const)
      ((eq? e (quote eq?)) *const)
      ((eq? e (quote atom?)) *const)
      ((eq? e (quote zero?)) *const)
      ((eq? e (quote add1)) *const)
      ((eq? e (quote mul)) *const)
      ((eq? e (quote sub1)) *const)
      ((eq? e (quote number?)) *const)
      (else *identifier))))


(define list-to-action
  (lambda (e)
    (cond
      ((atom? (car e))
       (cond 
         ((eq? (car e) (quote quote))
          *quote)
         ((eq? (car e) (quote lambda))
          *lambda)
         ((eq? (car e) (quote cond))
          *cond)
         (else *application)))
      (else *application))))


; operational semantics -- the definitions of the action functions

(define *const
  (lambda (e table)
    (cond 
      ((number? e) e)
      ((eq? e #t) #t)
      ((eq? e #f) #f)
      (else (build (quote primitive) e)))))


(define *quote
  (lambda (e table)
    (text-of e)))

(define text-of second)




(define *identifier
  (lambda (e table)
    (lookup-in-table e table initial-table)))


; note that as (car (quote ())) throws an error, this definition
; amounts to saying that looking anything up in the initial table
; is impossible.
(define initial-table
  (lambda (name)
    (car (quote ()))))


(define *lambda
  (lambda (e table)
    (build (quote non-primitive)
           (cons table (cdr e)))))

(define table-of first)

(define formals-of second)

(define body-of third)


; cond is a special form that takes any number of 
; cond-lines ...  if it sees an else-line, it treats
; that cond-line as if its question part were true.

(define evcon
  (lambda (lines table)
    (cond 
      ((else? (question-of (car lines)))
       (meaning (answer-of (car lines))
                table))
      ((meaning (question-of (car lines))
                table)
       (meaning (answer-of (car lines))
                table))
      (else (evcon (cdr lines) table)))))


(define else?
  (lambda (x)
    (cond 
      ((atom? x) (eq? x (quote else)))
      (else #f))))

(define question-of first)

(define answer-of second)



(define *cond 
  (lambda (e table)
    (evcon (cond-lines-of e) table)))

(define cond-lines-of cdr)



(define evlis
  (lambda (args table)
    (cond 
      ((null? args) (quote ()))
      (else
       (cons (meaning (car args) table)
             (evlis (cdr args) table))))))



(define *application
  (lambda (e table)
    (myapply
     (meaning (function-of e) table)
     (evlis (arguments-of e) table))))

(define function-of car)

(define arguments-of cdr)




(define primitive?
  (lambda (l)
    (eq? (first l) (quote primitive))))

(define non-primitive?
  (lambda (l)
    (eq? (first l) (quote non-primitive))))



(define myapply
  (lambda (fun vals)
    (cond
      ((primitive? fun)
       (myapply-primitive
        (second fun) vals))
      ((non-primitive? fun)
       (myapply-closure
        (second fun) vals)))))


(define myapply-primitive
  (lambda (name vals)
    (cond
      ((eq? name (quote cons))
       (cons (first vals) (second vals)))
      ((eq? name (quote car))
       (car (first vals)))
      ((eq? name (quote cdr))
       (cdr (first vals)))
      ((eq? name (quote null?))
       (null? (first vals)))
      ((eq? name (quote eq?))
       (eq? (first vals) (second vals)))
      ((eq? name (quote atom?))
       (:atom? (first vals)))
      ((eq? name (quote zero?))
       (zero? (first vals)))
      ((eq? name (quote add1))
       ((lambda (x) (+ x 1)) (first vals)))
      ((eq? name (quote mul))
       (* (first vals) (second vals)))
      ((eq? name (quote sub1))
       ((lambda (x) (- x 1)) (first vals)))
      ((eq? name (quote number?))
       (number? (first vals))))))


(define :atom?
  (lambda (x)
    (cond 
      ((atom? x) #t)
      ((null? x) #f)
      ((eq? (car x) (quote primitive))
       #t)
      ((eq? (car x) (quote non-primitive))
       #t)
      (else #f))))

(define myapply-closure
  (lambda (closure vals)
    (meaning (body-of closure)
             (extend-table
              (new-entry
               (formals-of closure)
               vals)
              (table-of closure)))))



(value 2)
;test value from tls(adjusted)
(value (*cond '(cond (coffee klatsch) (else party))
       '(( (coffee) ( #t )) ((klatsch party) ((mul 15 4) (6))))))

;tls stills works as advertised even with changed representation of bindings in the system to explicit pairs of the form (name value)
;(y-->2)(name-->2)

;
; trying out our Scheme interpreter!
;

(value '(add1 6))                           ; 7
(value '(quote (a b c)))                    ; '(a b c)
(value '(car (quote (a b c))))              ; 'a
(value '(cdr (quote (a b c))))              ; '(b c)
(value
  '((lambda (x)
      (cons x (quote ())))
    (quote (foo bar baz))))                 ; '((foo bar baz))
(value
  '((lambda (x)
      (cond
        (x (quote true))
        (else
          (quote false))))
    #t))                                    ; 'true

(value '((lambda (x) (add1 x)) 3))
