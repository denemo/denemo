;;;; Various scripts and variables for Denemos note-generation and filter section: NotationMagick
;;; By Nils Gey 2010
;;; Needs loaded ans-7.scm

;; A global var, used by several NotationMagick functions to remember the last value used 
(define NotationMagick::NOTATIONSTRING "c' e' g'") 


;;;; Lilypond wrappers for ans-7 functions
;;;;;;;;;;;;;;;;;;;;;;

;Random Diatonic: Converter for Lilypond syntax  
(define*  (NotationMagick::RandomDiatonicLy #:optional (from "c,,,") (to "b''''"))
	(ANS-7::RandomDiatonic (ANS-7::Ly2Ans from) (ANS-7::Ly2Ans to))
)

;Random Chromatic: Converter for Lilypond syntax  
(define*  (NotationMagick::RandomChromaticLy #:optional (from "c,,,") (to "b''''"))
	(ANS-7::RandomChromatic (ANS-7::Ly2Ans from) (ANS-7::Ly2Ans to))
)

;Shuffled List Insert for Lilypond syntax
(define (NotationMagick::InsertListRandomlyLy lylist)
	(ANS-7::InsertListRandomly (map ANS-7::Ly2Ans lylist))
)

;Insert a random member of a list for Lilypond syntax
(define (NotationMagick::InsertMemberRandomlyLy lylist)
	(ANS-7::InsertMemberRandomly (map ANS-7::Ly2Ans lylist))
)

;; Ask for a new NOTATIONSTRING with an input-window, return that and remember the value. 
(define (NotationMagick::AskNewNotationstring)
(define new (d-GetUserInput "Enter Lilypond notes" "Please insert a list of lilypond notes, separated by comma." NotationMagick::NOTATIONSTRING))
(set! NotationMagick::NOTATIONSTRING new)
NotationMagick::NOTATIONSTRING
)



