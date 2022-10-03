;Завдання 1
(define (Acker m n) ;визначаємо функцію
  
    (cond((= m 0) (+ n 1))  ;умова , якщо m = 0 , то n + 1

         ((> m 0) (cond(  (= n 0) (Acker (- m 1) 1)) ;умова, якщо m > 0, та
                                                  ;n = 0 , то вик. певна дія

                          ((> n 0) (Acker (- m 1) (Acker m (- n 1))))
                          ; умова, якщо m > 0, та n > 0 , то вик. певна дія

    ))))
;(Acker 2 2)


;Завдання 2

(define (Adv n)  ;визначення функції
  (define (rec n m) 
    (cond ((= 1 n)) ((= 0 (modulo n m)) (display m) (display " ") (rec (/ n m) 2 ))
    (else(rec n (+ m 1))))) ;перевірка на умову, якщо n = 1 , то потім модуль числа n
                            ;перевіряється на модуль числа m, відображає дільники
  (rec n 2)
  )

(display "Please enter N > 0: ") ;Ввід числа більше 0
(define n (read))
(newline)
 
(Adv n)
