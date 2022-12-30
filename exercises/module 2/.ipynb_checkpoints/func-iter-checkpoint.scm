

;; (define (f n) 
;;      (cond ((< n 3) n)
;;            (else (+
;;                   (f (- n 1))
;;                   (* 2 (f (- n 2)))
;;                   (* 3 (f (- n 3)))
;;                   ))))


(define (f n) 
    (define (f2 n_0 n_1 n_2) 
            (+ 
                n_2
                (* 2 n_1)
                (* 3 n_0)))
    (define (f-iter count n_0 n_1 n_2) 
            (cond ((= count 0) n_0)
                  (else (f-iter (- count 1) n_1 n_2 (f2 n_0 n_1 n_2)))))
    (f-iter n 0 1 2))

    