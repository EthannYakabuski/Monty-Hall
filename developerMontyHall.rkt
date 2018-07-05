(#%require (only racket/base random))

(define doors '())
(define playerChoice 0)
(define playerSwitchChoice -1)
(define carLocation 0)
(define goatLocation1 0)
(define goatLocation2 0)
(define goatToShow 0)
(define doorOpened 0)
(define score 0)
(define cars 0)
(define goats 0)
(define gamesPlayed 0)
(define doorsGraphics (cons '(1) (cons '(2) (cons '(3) '()))))

;doorChoice : 1 , 2 , 3
;switchChoice: 0: No , 1: Yes
;playTimes: amount of times to play monty-hall with inputted choices
(define (dev-MontyHall doorChoice switchChoice playTimes)
  (set! gamesPlayed (+ gamesPlayed 1))
  (if (= gamesPlayed playTimes) (done))
  (if (< gamesPlayed playTimes) (continue doorChoice switchChoice playTimes))
  )

(define (done)
  (set! playerChoice 0)
  (set! playerSwitchChoice -1)
  (set! carLocation -1)
  (set! cars 0)
  (set! goats 0)
  (set! score 0)
  (set! doors '())
  (set! goatLocation1 0)
  (set! goatLocation2 0)
  (set! goatToShow 0)
  (set! doorOpened 0)
  (set! doorsGraphics (cons '(1) (cons '(2) (cons '(3) '()))))
  (display "You are done")(newline))

(define (continue doorChoice switchChoice playTimes)
  (setDoors)
  (playerChooseDoor doorChoice)
  (revealDoor)
  (switchDoors? switchChoice)
  (showOutcome doorChoice switchChoice playTimes)
  )

;shows the outcome and determines whether or not to play again
(define (showOutcome doorChoice switchChoice playTimes)
  (cond ( (= (- playerChoice 1) carLocation) (winner))
        ( (= 1 1) (loser)))
  (display "Cars Won:")
  (display cars)(newline)

  (display "Goats Won:")
  (display goats)(newline)

  (display "Score:")
  (display score)(newline)

  (display "Games Played:")
  (display gamesPlayed)(newline)

  (define playAgain (> playTimes gamesPlayed))

  (if (eq? playAgain #t) (set! doorsGraphics (cons '(1) (cons '(2) (cons '(3) '())))))
  (cond ( (eq? playAgain #t) (dev-MontyHall doorChoice switchChoice playTimes))
  (else (closeGame)))
  
)

(define (closeGame)
  (display "Cars Won:")
  (display cars)(newline)

  (display "Goats Won:")
  (display goats)(newline)

  (display "Score:")
  (display score)(newline)

  (display "Games Played:")
  (display gamesPlayed)(newline)

  (display "Win Percentage: ")
  (define winPercentage (/ cars gamesPlayed))
  (display winPercentage)(newline)

  (display "GoodBye")(newline)
  
  )

(define (winner)
  (set! score (+ score 1))
  (set! cars (+ cars 1))
  (display "You won the vehicle, congratulations")(newline)
  )

(define (loser)
  (set! score (- score 1))
  (set! goats (+ goats 1))
  (display "You won a goat, his name is Billy")(newline)
  )

;reveals a door, does not show graphics like in the user input version
(define (revealDoor)
  (ChooseRevealDoor))

(define (ChooseRevealDoor)
   (set! goatToShow (random 2))

   (if (= goatToShow 0) (showGoat1))
   (if (= goatToShow 1) (showGoat2))
 )

(define (showGoat1)
  (cond ( (= (- playerChoice 1) goatLocation1) (showGoat2))
        ( (= 1 1) (setGraphics goatLocation1))))

(define (showGoat2)
  (cond ( (= (- playerChoice 1) goatLocation2) (showGoat1))
        ( (= 1 1) (setGraphics goatLocation2)))
  )

(define (setGraphics x)
  (if (= x 0) (set! doorsGraphics (cons 'Goat (cons '(2) (cons '(3) '())))))
  (if (= x 1) (set! doorsGraphics (cons '(1) (cons 'Goat (cons '(3) '())))))
  (if (= x 2) (set! doorsGraphics (cons '(1) (cons '(2) (cons 'Goat '())))))
  )
  
;sets the players switch choice to the inputted argument
(define (switchDoors? switchChoice)
  (set! playerSwitchChoice switchChoice)
  (if (= playerSwitchChoice 1) (switchDoors)))

(define (switchDoors)
  (cond ( (= (- playerChoice 1) carLocation) (specialCaseSwitchDoor))
        ( (= playerChoice 1)
              (if (= carLocation 1) (set! playerChoice 2))
              (if (= carLocation 2) (set! playerChoice 3)))
        ( (= playerChoice 2)
              (if (= carLocation 0) (set! playerChoice 1))
              (if (= carLocation 2) (set! playerChoice 3)))
        ( (= playerChoice 3)
              (if (= carLocation 0) (set! playerChoice 1))
              (if (= carLocation 1) (set! playerChoice 2)))
      
        ))

(define (specialCaseSwitchDoor)
  (cond ( (= playerChoice 1)
          (if (eq? (car (cdr doorsGraphics)) 'Goat) (set! playerChoice 3))
          (if (eq? (car (cdr (cdr doorsGraphics))) 'Goat) (set! playerChoice 2)))
        ( (= playerChoice 2)
          (if (eq? (car doorsGraphics) 'Goat) (set! playerChoice 3))
          (if (eq? (car (cdr (cdr doorsGraphics))) 'Goat) (set! playerChoice 1)))
        ( (= playerChoice 3)
          (if (eq? (car doorsGraphics) 'Goat) (set! playerChoice 2))
          (if (eq? (car (cdr doorsGraphics)) 'Goat) (set! playerChoice 1))))
  )

;sets the players choice of the door to the inputted argument
(define (playerChooseDoor doorChoice)
  (set! playerChoice doorChoice))
  
;sets the doors to starting position
(define (setDoors)
  (chooseCarLocation)
  (fillWithGoats)
  )

;chooses the car location
(define (chooseCarLocation)
  (set! carLocation (random 3))
  )

;sets the doors list to the appropriate outcome after deciding car location using above procedure
(define (fillWithGoats)
  (if (= carLocation 0) (set! doors (cons 1 (cons 0 (cons 0 doors)))))
  (if (= carLocation 1) (set! doors (cons 0 (cons 1 (cons 0 doors)))))
  (if (= carLocation 2) (set! doors (cons 0 (cons 0 (cons 1 doors)))))
  (setGoatLocations)
)

(define (setGoatLocations)
  (if (= carLocation 0) (setGoats0))
  (if (= carLocation 1) (setGoats1))
  (if (= carLocation 2) (setGoats2))
  )

(define (setGoats0)
  (set! goatLocation1 1)
  (set! goatLocation2 2)
  )

(define (setGoats1)
  (set! goatLocation1 0)
  (set! goatLocation2 2)
  )

(define (setGoats2)
  (set! goatLocation1 0)
  (set! goatLocation2 1)
  )


(display "Choose door one, don't switch and play five times")(newline)
(dev-MontyHall 1 0 5)

(set! gamesPlayed 0)

(display "Choose door one, switch and play five times")(newline)
(dev-MontyHall 1 1 5)

