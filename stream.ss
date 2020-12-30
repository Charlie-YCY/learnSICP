;(load "D:/AAAprograms_codes/learnSICP/stream.ss")
(define (memo_proc proc)
    (let ((already-run? false) (result false))
        (lambda ()
            (if (not already-run?)
                (begin (set! result (proc))
                        (set! already-run? true)
                        result)
            result
        )))
   )

(define-syntax cons_stream
     (syntax-rules ()
         [(_ a b) (cons a (delay b))]))
; (define (force obj)
;   (obj))


; (define (cons_stream a b)
;     (cons a (delay b)))

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

(define (stream_map proc s)
    (if (stream_null? s)
        the_empty_stream
        (cons_stream (proc (stream_car s))
                    (stream_map proc (stream_cdr s)))))

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
    (display  x))

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
            (cons_stream (stream_car stream)
                (stream_filter pred (stream_cdr stream))))
        (else (stream_filter pred (stream_cdr stream)))))


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

; (define (prime? n)
;     (= n (smallest_divisor n)))

(stream_car (stream_cdr
        (stream_filter prime?
            (stream_enumerate_interval 10000 100000))))

(define (stream_map proc . argstreams)
    (if (stream_null? (car argstreams))
        the_empty_stream
        (cons_stream
            (apply proc (map stream_car argstreams))
            (apply stream_map
                    (cons proc (map stream_cdr argstreams))))))

(define (integers_starting_from n)
         (cons_stream n (integers_starting_from(+ 1 n))))
;(define integers
;    (integers_starting_from 1))

;(define x (stream_map show (stream_enumerate_interval 0 10)))

(define (add_streams s1 s2)
    (stream_map + s1 s2))
(define ones (cons_stream 1 ones))
(define integers
    (cons_stream 1 (add_streams ones integers)))

(define (mul_streams s1 s2)
    (stream_map * s1 s2))

; (define factorials (cons_stream 1 (mul_streams factorials (stream_cdr factorials))))

; (define factorials
;    (cons-stream 1
;                 (mul-streams factorials (stream-cdr integers))))

(define (scale_stream  stream factor)
    (stream_map (lambda (x) (* x factor)) stream))

; (define (partial_sums s)
;     (cons_stream (stream_car s) (add_streams (stream_cdr s) (partial_sums s))))

 (define (partial-sums s)
   (define ps (add-streams s (cons-stream 0 ps)))
   ps)
; Must have self-reference to avoid recalculation:

(define (merge_streams s1 s2)
    (cond ((stream_null? s1) s2)
          ((stream_null? s2) s1)
          (else
            (let ((s1car (stream_car s1))
                  (s2car (stream_car s2)))
               (cond ((< s1car s2car)
                       (cons_stream s1car (merge_streams (stream_cdr s1) s2)))
                     ((> s1car s2car)
                       (cons_stream s2car (merge_streams s1 (stream_cdr s2))))
                     (else
                        (cons_stream s1car
                                        (merge_streams (stream_cdr s1)
                                                (stream_cdr s2)))))))))
(define Hamming_int (cons_stream 1 (merge_streams (scale_stream integers 5)
                                                  (merge_streams (scale_stream integers 2) (scale_stream integers 3)))))

(define primes
    (cons_stream
        2
            (stream_filter prime? (integers_starting_from 3))))
(define (prime? n)
    (define (iter ps)
        (cond ((> (square (stream_car ps)) n) #t)
            ((divides? (stream_car ps) n) #f)
            (else (iter (stream_cdr ps)))))
    (iter primes))

(define integrate_series ())