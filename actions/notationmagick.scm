;;;; Various scripts and variables for Denemos note-generation and filter section: NotationMagick
;;; By Nils Gey 2010
;;; Needs loaded ans.scm

;; A global var, used by several NotationMagick functions to remember the last value used 
(define NotationMagick::NOTATIONSTRING "c' e' g'") 


;;;; Lilypond wrappers for ANS functions
;;;;;;;;;;;;;;;;;;;;;;

;Random Diatonic: Wrapper for Lilypond syntax  
(define*  (NotationMagick::RandomDiatonicLy #:optional (from 'c,,,) (to 'b''''))
	(ANS::RandomDiatonic (ANS::Ly2Ans from) (ANS::Ly2Ans to)))

;Random Chromatic: Wrapper for Lilypond syntax  
(define*  (NotationMagick::RandomChromaticLy #:optional (from 'c,,,) (to 'b''''))
	(ANS::RandomChromatic (ANS::Ly2Ans from) (ANS::Ly2Ans to)))

;Shuffled List Insert for Lilypond syntax
(define (NotationMagick::InsertListRandomlyLy lylist)
	(ANS::InsertListRandomly (map ANS::Ly2Ans lylist))
)

;Insert a random member of a list for Lilypond syntax
(define (NotationMagick::InsertMemberRandomlyLy lylist)
	(ANS::InsertMemberRandomly (map ANS::Ly2Ans lylist))
)

;; Ask for a new NOTATIONSTRING with an input-window, remember the value and return it as list of symbols for ANS
(define (NotationMagick::AskNewNotationstring)
(define new (d-GetUserInput "Enter Lilypond notes" "Please insert a list of lilypond notes, separated by Space." NotationMagick::NOTATIONSTRING))
(set! NotationMagick::NOTATIONSTRING new)
(map string->symbol (string-tokenize NotationMagick::NOTATIONSTRING))
)

;; Insert a random note which is in a reasonable range according to the prevailing clef. Up to one step above/under the first Ledger line. 
;; First the prototype, then one derived version for chromatic, one for diatonic. 

(define (NotationMagick::RandomWithinClefRange proc) 
	(define currentclef (d-GetPrevailingClef))
	(ANS::InsertNotes
		(cond 
		((string-ci=? currentclef "Treble") (proc 'b 'b''))
		((string-ci=? currentclef "Bass") (proc 'd, 'd'))
		((string-ci=? currentclef "Alt") (proc 'c 'c''))
		((string-ci=? currentclef "Treble Octava bassa") (proc 'b, 'b'))
		((string-ci=? currentclef "Bass Octava bassa") (proc 'd,, 'd))
		((string-ci=? currentclef "Tenor") (proc 'a, 'a'))
		((string-ci=? currentclef "Soprano") (proc 'g 'g''))
		((string-ci=? currentclef "French") (proc 'd' 'd'''))		
		))
)
;; Usable versions for diatonic and chromatic
(define (NotationMagick::RandomWithinClefRangeDiatonic) (NotationMagick::RandomWithinClefRange NotationMagick::RandomDiatonicLy))
(define (NotationMagick::RandomWithinClefRangeChromatic) (NotationMagick::RandomWithinClefRange NotationMagick::RandomChromaticLy))

