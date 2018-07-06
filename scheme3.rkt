(load "developerMontyHall.rkt")


(define (flipCoin)
  (define ran (random 2))
  (cond ( (= ran 0) 0)
        ( (= ran 1) 1)))


