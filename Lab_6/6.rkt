; Aldokhin Volodymyr IPZ-41 Lab 6 Variant 1

#lang racket

(define (displayAll . vs)
  (for-each display vs))

; Завдання 1

(define (getFirst v) (vector-ref v 0))

(define (getTale v) (vector-take-right v (- (vector-length v) 1)))

(define (getVectorElements v isEvenElement)
  (cond ((and (> (vector-length v) 0) (= isEvenElement 1)) (list* (getFirst v) (getVectorElements (getTale v) 0)))
        ((and (> (vector-length v) 0) (= isEvenElement 0))                     (getVectorElements (getTale v) 1))
        (else (list))))


(define (minElement v) (inexact->exact (for/fold ([m +inf.0]) ([x (in-vector v)]) (min m x))))
(define (maxElement v) (inexact->exact (for/fold ([m -inf.0]) ([x (in-vector v)]) (max m x))))

(define vf (vector 143 22 231 23 -15 41 52))

(define vfOdd (list->vector (getVectorElements vf 1)))
(define vfEven (list->vector (getVectorElements vf 0)))

(displayAll "Мінімальне значення непарного числа: " (minElement vfOdd) "\n")
(displayAll "Максимальне значення непарного числа: " (maxElement vfOdd) "\n")

(displayAll "Мінімальне значення парного числа: " (minElement vfEven) "\n")
(displayAll "Максимальне значення парного числа: " (maxElement vfEven) "\n")

; Завдання 2

(define (getStack s) (reverse s))

(define (getStackWithCondition s a)
  (cond ((and (> (vector-length s) 0) (> (modulo (getFirst s) a) 0)) (list* (getFirst s) (getStackWithCondition (getTale s) a)))
        ((and (> (vector-length s) 0) (= (modulo (getFirst s) a) 0)) (                    getStackWithCondition (getTale s) a))
        (else (list))))

(define stack (list 14 31 23 6 45 76 32 51 4 12 42 123 32 43))
(displayAll "Створений стек: " (getStack stack) "\n")

(printf "\nВведіть значення: ")
(define a (read))

(displayAll "Обернений стек без елементів кратних " a ": " (getStackWithCondition (list->vector stack) a))