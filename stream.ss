;(load "D:/AAAprograms_codes/learnSICP/stream.ss")
(define (memo_proc proc)
    (let ((already-run? #f) (result #f))
        (lambda ()
            ; (display "(memo_proc p)")
            ; (put-datum (current-output-port) proc)
            (if (not already-run?)
                (begin (set! result (proc))
                        (set! already-run? #t)
                        result)
            result
        )))
   )

(define-syntax cons_stream
     (syntax-rules ()
         [(_ a b) (cons a (delay b))]))


; (define-syntax delay
;     (syntax-rules ()
;         [(_ exp) (memo_proc (lambda () exp))]))

(define-syntax delay
    (syntax-rules ()
        [(_ exp)  (lambda () exp)]))

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
    (stream_for_each display_separate s))

(define (display_line x)
    (newline)
    (sleep (make-time 'time-duration 1000000 0))
    (display x))

(define (display_separate x)
    (display "//\\\\")
    (sleep (make-time 'time-duration 200000 0))
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

; (stream_car (stream_cdr
;         (stream_filter prime?
;             (stream_enumerate_interval 10000 100000))))

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

(define no_seven
    (stream_filter (lambda (x)  (not (divides?  7 x)))
                   integers))

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
;                                                              ~~~~~~~~~~~~~

 (define (partial_sums s)
   (define ps (add_streams s (cons_stream 0 ps)))
   ps)
; Must have self-reference to avoid recalculation!!!:

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

(define (integrate_series s)
    (stream_map * (stream_map (lambda (x) (/ 1 x)) integers) s))

(define exp_series
    (cons_stream 1 (integrate_series exp_series)))

; (define (integrate-series s)
;               (stream-map /  s integers))


; 错误
; (define cosine_series
;     (cons_stream 1 (integrate_series (integrate_series cosine_series))))

 (define cosine_series
     (cons_stream 1 (stream_map  - (integrate_series sine_series))))
 (define sine_series
     (cons_stream 0 (integrate_series cosine_series)))


(define (mul-series s1 s2)
  (cons-stream
    (* (stream_car s1) (stream_car s2))
    (add_streams (add_streams (scale-stream (stream-cdr s1) (stream_car s2))
                              (scale-stream (stream-cdr s2) (stream_car s1)))
                 (cons-stream 0 (mul_series (stream-cdr s1) (stream-cdr s2))))))

; (define (reciprocal_series s)
;     (cons_stream 1 ((stream_map (lambda (x) (- 0 x))
;                                             (mul_series (stream_cdr s) ())))))

 (define (invert-unit-series series)
   (define inverted-unit-series
     (cons-stream 1 (scale-stream (mul-streams (stream-cdr series)
                                               inverted-unit-series)
                                  -1)))
   inverted-unit-series)

(define (sqrt_stream x)
    (define guesses
       (begin
        (display "guesses")
        (cons_stream 1.0
                     (stream_map (lambda (guess)
                                    (sqrt_improve guess x))
                                 guesses))))
   (display "sqrt_stream")
    guesses)
(define (sqrt_improve guess x)
    (/ (+ guess (/ x guess)) 2))

(define (louis_sqrt_stream x)
    (cons_stream 1.0
                 (stream_map (lambda (guess)
                                (sqrt_improve guess x))
                             (louis_sqrt_stream x))))

(define (pi_summands n)
    (cons_stream (/ 1.0 n)
                 (stream_map - (pi_summands (+ n 2)))))
(define pi_stream
    (scale_stream (partial_sums (pi_summands 1)) 4))

(define (euler_transform s)
    (let ((s0 (stream_ref s 0))
          (s1 (stream_ref s 1))
          (s2 (stream_ref s 2)))
        (cons_stream (- s2 (/ (square (- s2 s1))
                              (+ s0 (* -2 s1) s2)))
                     (euler_transform (stream_cdr s)))))
(define (make_tableau  transform s)
    (cons_stream s
                 (make_tableau transform
                               (transform s))))
(define (accel_sequence transform s)
    (stream_map stream_car
        (make_tableau transform s)))

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

(define (pairs2 s t)
    (cons_stream
        (list (stream_car s ) (stream_car t))
        (interleave
            (interleave
                (stream_map (lambda (x) (list (stream_car s) x))
                            (stream_cdr t))
                (stream_map (lambda (x) (list x (stream_car t)))
                            (stream_cdr s)))
            (pairs (stream_cdr s) (stream_cdr t)))))


(define (pairs3 s t)
  (cons_stream
   (list (stream_car s) (stream_car t))
   (interleave
    (stream_map (lambda (x) (list (stream_car s) x))
                (stream_cdr t))
    (pairs (stream_cdr s) t))))

(define (triples s t u)
    (cons_stream (list (stream_car s) (stream_car t) (stream_car u))
                 (interleave (stream_map (lambda (x) (list (stream_car s) (car x) (car (cdr x))))
                                         (stream_map (lambda (x) (list (stream_car t) x))
                                                     (stream_cdr u)))
                             (triples (stream_cdr s) (stream_cdr t) (stream_cdr u)))))

; (define (pythagoras_filter s)
;     (= (+ (square (car s))
;           (square (car (cdr s))))
;        (square (car (cdr (cdr s))))))

; (define pythagoras
;     (stream_filter pythagoras_filter (triples integers integers integers)))

;  (define first-of-integer-pair
;    (stre_m_map car (pairs integers integers)))

;  (define (triples s t u)
;    (let ((pairs-tu (pairs t u))) ;; compute pairs only *once*
;      (define (rec si i ptu top-i)
;        (co_s_stream
;         (cons (stream_car si) (stream_car ptu))
;         (if (= i (stream_car top-i))
;             (rec s 1 (stream_cdr ptu) (stre_m_cdr top-i))
;             ;; restart s cycle with next ptu
;             (rec (stre_m_cdr si) (1+ i) ptu top-i))))
;      (rec s 1 pairs-tu first-of-integer-pair)))

(define (pythagorean? a b c)
    (= (square c)
        (+ (square a) (square b))))

 (define triples-integers
   (triples integers integers integers))

(define pythagorean-triples
    (stream_filter
        (lambda (triple)
            (apply pythagorean? triple))
        triples-integers))

; (define (triples s t u)
;         (cons_stream (list (stream_car s) (stream_car t) (stream_car u))
;                      (interleave
;                         (stream_map (lambda (x) (cons (stream_car s) x))
;                                     (stream_cdr (pairs t u))
;                         (triples (stream_cdr s)
;                                 (stream_cdr t)
;                                 (stream_cdr u))))))
