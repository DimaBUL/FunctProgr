#lang racket

(define var 0)
(define main-list '())
(define drv-variable 0)
(define even-amount 0)
(define avrg 0)


;Вирішуємо, що робити
(define (program-menu)
  (begin
(printf "\n 1 - Додаємо елемент в список
 2 - Ввести значення елемента на позицію
 3 - Змінити всі парні значення на середнє значення всіх елементів у списку
 4 - Скинути список")
(printf "\n Ваш вибір: ")
(set! var (read))
(start-variant var)
        )
)

;Гілки з визовом функцій
(define (start-variant v)
  (begin
    (cond
      
   ;Тут ми можемо відправити функцію як параметри для виклику їх пізніше
      ;(Оскільки ми маємо однакові структури функцій, але різні функціональні можливості)
   [(= v 1) (add-element-into-list set-at)]
   [(= v 2) (add-element-into-list insert-into-list)]
   ;--------------------------------------------------------------------------
   
   [(= v 3) (change-all-even-values 0)]
   [(= v 4) (reset-list-all-values-hng)])
    (display-list-parameters)
   (program-menu))
)

;Вставлення значення в список у визначену позицію
(define (insert-into-list at val)
  (begin
    (define lst1 (take-right main-list (- (length main-list) at)))
    (define lst2 (drop-right main-list (- (length main-list) at)))
    (set! main-list (append lst2 (list val) lst1)))
         )

;змінити всі парні значення на середнє значення
(define (change-all-even-values curr)
  (begin
    (if (< curr (length main-list))
  (if (is-even (list-ref main-list curr))
      (begin
        (set-at curr avrg)
        (change-all-even-values (+ curr 1)))
      (change-all-even-values (+ curr 1)))
  even-amount)
         )
  )

;Знайти середнє значення
(define (average l)
  (if (= (length main-list) 0)
      0
      (/ (foldr (lambda (x y) (+ x y)) 0 l) 
     (length l))))

;Скинути всі значення в списку

(define (reset-list-all-values-hng)
  (set! main-list '()))

;додати елемент у значення (або змінити старе значення на нове)
(define (add-element-into-list func)
  (begin
    (printf "\nВведіть елемент: ")
    (define val (read))
    (printf "\nВведіть позицію: ")
    (define pos (read))
    (func pos val)
    ))

;відобразити всі поточні параметри списку
(define (display-list-parameters)
  (begin
        (printf "\nВаш лист: ")
    (display main-list)
    (printf "\nКількість значень: ")
    (display (length main-list))
    (printf "\nКількість парних значень: ")
    (display (count-all-even-values 0)))
    (printf "\nСереднє значення: ")
    (display (average main-list))
    (set! even-amount 0)
    (set! avrg (average main-list))
  )

;встановити значення на визначену позицію в списку
(define (set-at at val)
  (begin
  (if (< (length main-list) (+ at 1))
      (begin
        (set! main-list (append main-list (list 0)))
        (set-at at val)
                     )
      (set! main-list (list-set main-list at val))))
)


;Кількість всіх парних чисел всередині списку
(define (count-all-even-values curr)
  (if (< curr (length main-list))
  (if (is-even (list-ref main-list curr))
      (begin
        (set! even-amount (+ even-amount 1))
        (count-all-even-values (+ curr 1)))
      (count-all-even-values (+ curr 1)))
  even-amount)
)

;Є поточне значення парним чи ні (включаючи десяткове)
(define (is-even x)
  (if (integer? x)
  (if (equal? (remainder x 2) 0)
      #t
      #f)
  #f))

(program-menu)