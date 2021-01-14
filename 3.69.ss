;(load "D:/AAAprograms_codes/learnSICP/3.69.ss")
(define-syntax cons_stream
     (syntax-rules ()
         [(_ a b) (cons a (delay b))]))

(define (force parameters)
    ; (display "(force p)")
    ; (display parameters)
    (parameters))
(define (stream_car stream)
    (car stream))

(define (stream_cdr stream)
    (force (cdr stream)))

(define (stream_ref s n)
    (if (= n 0)
        (stream_car s)
        (stream_ref (stream_cdr s) (- n 1))))

(define (stream_null? parameters)
    (null? parameters))

(define the_empty_stream '() )

(define (stream_for_each proc s)
    (if (stream_null? s)
        (begin (newline) 'done)
        (begin (proc (stream_car s))
                (stream_for_each proc (stream_cdr s)))))


(define (display_stream s)
    (stream_for_each display_line s))

(define (display_line x)
    (newline)
    (sleep (make-time 'time-duration 1000000 0))
    (display x))


(define (show x)
    (display_line x) x)

(define (stream_enumerate_interval low high)
    (if (> low high)
        the_empty_stream
        (cons_stream
         low
         (stream_enumerate_interval (+ low 1) high))))

(define (stream_filter pred stream)
    (cond ((stream_null? stream) the_empty_stream)
          ((pred (stream_car stream))
            ;   (display (stream_car stream))
            ;   (display "filterok")
            ;   (sleep (make-time 'time-duration 1000000 0))
              (cons_stream (stream_car stream)
                           (stream_filter pred (stream_cdr stream))))
          (else
                ; (display (stream_car stream))
                ; (display "filter")
                    ; (sleep (make-time 'time-duration 1000000 0))
                (stream_filter pred (stream_cdr stream)))))


(define (smallest_divisor n)
    (find_divisior n 2))

(define (find_divisior n test_divisior)
    (cond ((> (square test_divisior) n) n)
            ((divides? test_divisior n) test_divisior)
            (else (find_divisior n (+ test_divisior 1)))))

(define (divides? a b)
    (= (remainder b a) 0))

(define (square a)
    (* a a))

(define (stream_map proc . argstreams)
    (if (stream_null? (car argstreams))
        the_empty_stream
        (cons_stream
            (apply proc (map stream_car argstreams))
            (apply stream_map
                    (cons proc (map stream_cdr argstreams))))))

(define (integers_starting_from n)
         (cons_stream n (integers_starting_from(+ 1 n))))

(define (add_streams s1 s2)
    (stream_map + s1 s2))
(define ones (cons_stream 1 ones))
(define integers
    (cons_stream 1 (add_streams ones integers)))


;no_seven 返回序对
(define no_seven
    (stream_filter (lambda (x)  (not (divides?  7 x)))
                   integers))

(define sevens
    (stream_filter (lambda (x) (divides?  7 x))
                   integers))

(define (interleave s1 s2)
    (if (stream_null? s1)
        s2
        (cons_stream (stream_car s1)
                     (interleave s2 (stream_cdr s1)))))

(define (pairs s t)
    (cons_stream (list (stream_car s) (stream_car t))
                 (interleave (stream_map (lambda (x) (list (stream_car s) x))
                                         (stream_cdr t))
                             (pairs (stream_cdr s) (stream_cdr t)))))


(define (triples s t u)
        (cons_stream (list (stream_car s) (stream_car t) (stream_car u))
                       (interleave
                        (stream_map (lambda (x) (cons (car s) x)) (stream_cdr (pairs t u)))
                        (triples (stream_cdr s)
                                (stream_cdr t)
                                (stream_cdr u)))))


(define tnumbers (triples integers integers integers))
(define pfilter (lambda (x) (= (square (caddr x))
                                           (+ (square (car x)) (square (cadr x))))))

; (define testfilter (lambda (x) (= (car x) 1)))
(define phythagorean_numbers
            (stream_filter pfilter
                           tnumbers))
