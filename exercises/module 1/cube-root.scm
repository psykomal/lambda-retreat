


(define (cube-root x) 
  (define (cube-iter guess x) 
        ;; (display (format "values ~a \n" guess))
        (if (good-enough? guess x) 
            guess
            (cube-iter (improve guess x) x)))
  
  (define (square n) (* n n))
  
  (define (cube n) (* n (square n)))
  
  (define (improve guess x) 
        (/ (+ (/ x (square guess)) (* 2 guess)) 3.0))
  
  (define (good-enough? guess x) 
        (< (abs (- (cube guess) x)) 0.0001))
  
  (cube-iter 1 x))
