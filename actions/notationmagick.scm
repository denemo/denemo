;;;; Various scripts and variables for Denemos note-generation and filter section: NotationMagick
;;; By Nils Gey 2010
;;; Needs loaded ans-7.scm

;; A global var, used by several NotationMagick functions to remember the last value used 
(define NotationMagick::NOTATIONSTRING "c' e' g'") 

;; Ask for a new NOTATIONSTRING with an input-window, return that and remember the value. 
(define (NotationMagick::AskNewNotationstring)
(define new (d-GetUserInput "Enter Lilypond notes" "Please insert a list of lilypond notes, separated by comma." NotationMagick::NOTATIONSTRING))
(set! NotationMagick::NOTATIONSTRING new)
NotationMagick::NOTATIONSTRING
)
