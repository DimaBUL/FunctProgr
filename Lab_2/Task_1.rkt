;Task 1


;(display "Task 1 \n\n") 


;(define (fact n)

    
;(if (= n 0)   ;Cond

;1             ; 1 branch
 
;(* n (fact(- n 1))))) ; 2 branch



;Обчислення значення функції

(define (solve x)   

  (define (fact n)

    
(if (= n 0)   ;Cond

1             ; 1 branch
 
(* n (fact(- n 1))))) ; 2 branch

  ;Ряд Тейлора експоненти
  
  (define (e x)                                                     ;
  (+ 1 (+ x (+ (/ (* x x) (fact 2)) (/ (* x(* x x)) (fact 3))))))   ;
  

(cond((> 0 x) (display "Помилка\n")) ;Якщо x більше 0 , для експоненти

     ((and (<= 0 x) (<= x 2)) (+(e (- x)) (e (-(* 2 x)))))

     ((> x 2)(/ 1(e (-(+ x 5))))))

  )

;Визначення похибки

(define (poh x)

  (if (and (<= 0 x) (<= x 2)) ;Умова
      (-(+ (exp (- x)) (exp (- (* 2 x))))(solve x)) ;Branch 1
      (display "Помилка \n"))) ;Branch 2



(display "Значення функції \n\n") 

(solve -2)
(solve -1.5)
(solve -1)
(solve -0.5)
(solve 0)
(solve 0.5)
(solve 1)
(solve 1.5)
(solve 2)


(display "\nПохибки \n\n") 

(poh -2)
(poh -1.5)
(poh -1)
(poh -0.5)
(poh 0)
(poh 0.5)
(poh 1)
(poh 1.5)
(poh 2)



(display "\n Task 2 \n")


;Task 2
(define (pi x)

  (define (re x y z)

(if(= x z) (+ 6.0 0.0) (+ 6 (/ (* y y) (re x (+ y 2) (+ z 1))))))

(+ 3 (/ 1 (re x 3 1)))

  )

(pi 2)
(pi 15)
(pi 100)