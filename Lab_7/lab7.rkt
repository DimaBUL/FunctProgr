#lang racket
 
(define (creating) ;побудова файлу з реченнями.
  (let ((port (open-output-file "input.txt" #:exists 'replace)))
    (write "i like an pie " port)
    (write "but people like apple " port)
    (write "however, i prefer orange juice " port)
    (close-output-port port))
  )
 
(define (outputing) ; вивід відсортованих речень у файл
  (let ((port (open-output-file "result.txt" #:exists 'replace)))
    (write (tostring (bubble-sort-str 4 (str->words str1 (list ) 0 0)) "" 0) port)
    (write (tostring (bubble-sort-str 4 (str->words str2 (list ) 0 0)) "" 0) port)
    (write (tostring (bubble-sort-str 4 (str->words str3 (list ) 0 0)) "" 0) port)
    (close-output-port port))
  )
 
(define (str->words str word-list lsp n) ; побудова списку слів з речення
  (cond ((> (string-length str) n)
         (cond ((or (eq? (string-ref str n) #\space)
                    (= n (- (string-length str) 1)))
                (str->words str (append word-list (list (substring str lsp (+ n 1)))) (+ n 1) (+ n 1) )
                )
               (else (str->words str word-list lsp (+ n 1)))
               ))
        (else word-list ))
  )
 
(define (tostring l st n) ; створення речення зі списку слів
  (cond ((> (length l) n)
         (tostring l (string-append st (list-ref l n)) (+ n 1))
         )
        (else
         st)
        )
  )
 
(define (bubble-str L) ; однаразове сортування бульбошкою
  (if (null? (cdr L))
      L
      (if (string>? (car L) (cadr L))
          (cons (car L) (bubble-str (cdr L)))
          (cons (cadr L) (bubble-str (cons (car L) (cddr L))))
          )
      ))
 
(define (bubble-sort-str N L) ; сортування бульбашкою
  (cond ((= N 1) (bubble-str L))
        (else (bubble-sort-str (- N 1) (bubble-str L)))))
 
(newline)
 
(creating)
 
;==============ВИВЕДЕННЯ файлу на екран =================
 
(define in (open-input-file "input.txt"))
(define str1 (read in))
(define str2 (read in))
(define str3 (read in))
(close-input-port in)
 
(newline)
(display "*** Start file ***")
(newline)
(newline)
(display str1)
(newline)
(display str2)
(newline)
(display str3)
(newline)
(newline)
(display "*** Sorted file ***")
(newline)
(newline)

(display (tostring (bubble-sort-str (expt (length (str->words str1 (list ) 0 0)) 2) (str->words str1 (list ) 0 0)) "" 0))
(newline)
(display (tostring (bubble-sort-str (expt (length (str->words str2 (list ) 0 0)) 2) (str->words str2 (list ) 0 0)) "" 0))
(newline)
(display (tostring (bubble-sort-str (expt (length (str->words str3 (list ) 0 0)) 2) (str->words str3 (list ) 0 0)) "" 0))
(newline)
 
(outputing)