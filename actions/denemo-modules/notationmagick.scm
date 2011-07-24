(use-modules (ice-9 optargs)) ; optional (define* ) arguments

#!(define-module (actions denemo-modules notationmagick)
	#:export (
		NotationMagick::AskNewNotationstring
		NotationMagick::AskNewAsciistring
		NotationMagick::MirrorSelection
		NotationMagick::ModifySelectedObjects
		NotationMagick::ReverseCopyBuffer
		NotationMagick::PutBinaryStringList
		NotationMagick::String->CharsAsBinary
		NotationMagick::ReverseStringsInList 
		NotationMagick::InsertMemberRandomlyLy
		NotationMagick::RandomWithinClefRangeDiatonic
		NotationMagick::RandomWithinClefRangeChromatic
		)) !#


; Various scripts and variables for Denemos note-generation and filter section: NotationMagick
;; By Nils Gey 2010

;; Global vars, used by several NotationMagick functions to remember the last value used 
(define NotationMagick::NOTATIONSTRING "c' e' g'") 
(define NotationMagick::ASCIISTRING "bach") 


;; Ask for a new NOTATIONSTRING with an input-window, remember the value and return it as list of symbols for ANS
(define (NotationMagick::AskNewNotationstring)
	(define new (d-GetUserInput "Enter Lilypond notes" "Please insert a list of lilypond notes, separated by Space." NotationMagick::NOTATIONSTRING))
	(if new
		(begin (set! NotationMagick::NOTATIONSTRING new) (map string->symbol (string-tokenize NotationMagick::NOTATIONSTRING)))
		#f))

(define (NotationMagick::AskNewAsciistring)
	(define new (d-GetUserInput "Enter ASCII chars" "Please insert any kind and number of ASCII chars " NotationMagick::ASCIISTRING))
	(if new
		(begin (set! NotationMagick::ASCIISTRING new) NotationMagick::ASCIISTRING)
		#f))


; Lilypond wrappers for ANS functions
;;;;;

;Random Diatonic: Wrapper for Lilypond syntax  
(define*  (NotationMagick::RandomDiatonicLy #:optional (from 'c,,,) (to 'b''''))
	(ANS::RandomDiatonic (ANS::Ly2Ans from) (ANS::Ly2Ans to)))

;Random Chromatic: Wrapper for Lilypond syntax  
(define*  (NotationMagick::RandomChromaticLy #:optional (from 'c,,,) (to 'b''''))
	(ANS::RandomChromatic (ANS::Ly2Ans from) (ANS::Ly2Ans to)))

;Shuffled List Insert for Lilypond syntax
(define (NotationMagick::InsertListRandomlyLy lylist)
	(ANS::InsertListRandomly (map ANS::Ly2Ans lylist)))

;Insert a random member of a list for Lilypond syntax
(define (NotationMagick::InsertMemberRandomlyLy lylist)
	(ANS::InsertMemberRandomly (map ANS::Ly2Ans lylist)))


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
		)))
		
;; Usable versions for diatonic and chromatic
(define (NotationMagick::RandomWithinClefRangeDiatonic) (NotationMagick::RandomWithinClefRange NotationMagick::RandomDiatonicLy))
(define (NotationMagick::RandomWithinClefRangeChromatic) (NotationMagick::RandomWithinClefRange NotationMagick::RandomChromaticLy))

; Generators and Cryptographic Functions
;;;;;

;Binary Rhythm Creation - Based on "Using Binary Numbers in Music" from Vi Hart. http://vihart.com
;Implementation by Nils Gey - Feb. 2011
#! From her paper: "Binary numbers used as rhythms (not to be confused with binary rhythms) can be used in music to create
a mathematical aspect to a piece which can be heard and appreciated by mathematicians, yet still be
pleasant to the ears of the uninitiated. Even someone who has never heard of binary numbers can still
understand the patterns on an intuitive level. Binary numbers, especially those affiliated with computer
science, have a structure which goes very well with the standard forms of music." 

"Binary Numbers as Rhythm. Let 1 be a note and 0 be a rest. Thus a number becomes a rhythm!
In computer science, binary numbers are usually seen in groups of eight bits (a byte), which
happens to fit perfectly into a 4/4 measure if we assign bits to eighth notes. Powers of two are involved in
the usual form of western music, where four or eight bar phrases might make up a 16 measure section of a
64 measure song. This creates a constraint on the rhythm, but leaves the composer free to choose the
melody." 

ASCII Code. 8bit. One 4/4 measure = one letter. The code for each lowercase letter used begins
with 011, and the following five digits are the position of the letter in the alphabet. Lowercase “a,” then,
is 01100001.
You can reverse the ascii/binary digits to get more variation in the measure beginning.
!#

(define (NotationMagick::LeadingZerosFiller stringy digits) 
		(define needed (- digits (string-length stringy)))
		(if (> (string-length stringy) digits)
			stringy
			(string-append (make-string needed #\0) stringy)))
		
(define (NotationMagick::Char->binary char digits)
	(NotationMagick::LeadingZerosFiller (number->string (char->integer char) 2) digits))

(define (NotationMagick::String->CharsAsBinary stringy)
	(map (lambda (x) (NotationMagick::Char->binary x 8)) (string->list stringy)))

;Digit->binary is basically (number->string n 2) which fills with leading zeros to 4 digits.
(define (NotationMagick::Digit->binary numbery)
	(define numberstring (number->string numbery 2))
	(NotationMagick::LeadingZerosFiller numberstring 4))

(define (NotationMagick::ListOfNumbers->NumbersAsBinary listy digits)
	(map (lambda (x) (NotationMagick::LeadingZerosFiller (number->string x 2) digits)) listy))

;This is of limited use because you can't access all 4-digit variations. 10-15 (decimal) are out of range.
;If you want those create your own list of numbers and use
(define (NotationMagick::Number->DigitsAsBinary numbery)
	; this is really awkward
	(define digitstringlist (map (lambda (x) (make-string 1 x)) (string->list  (number->string numbery))))
	(set! digitstringlist (map string->number digitstringlist))
	(map NotationMagick::Digit->binary digitstringlist))

(define (NotationMagick::ReverseStringsInList listy)
	(map string-reverse listy))	

; Denemo connection
(define (NotationMagick::PutBinaryString stringy)
	(define (PutBinary digit)
		(if (equal? digit #\0)
			(d-EnterRest))		
		(if (equal? digit #\1)
			(d-Enter) ; TODO: Replace this with a built-in version.
			#f))	
	(for-each PutBinary (string->list stringy)))
	
(define (NotationMagick::PutBinaryStringList listy)
	(for-each NotationMagick::PutBinaryString listy))

;TODO: Put binary as lyrics

#! examples
(d-Set3)
(NotationMagick::PutBinaryStringList (NotationMagick::String->CharsAsBinary "bach")) ; bach
(d-AddAfter)
(NotationMagick::PutBinaryStringList (reverse (NotationMagick::String->CharsAsBinary "bach"))) ;hcab
(d-AddAfter)
(NotationMagick::PutBinaryStringList (NotationMagick::ReverseStringsInList (NotationMagick::String->CharsAsBinary "bach"))) ; bach, each ascii is reverted 
(d-AddAfter)
(NotationMagick::PutBinaryStringList (reverse (NotationMagick::ReverseStringsInList (NotationMagick::String->CharsAsBinary "bach")))) ; hcab, each ascii is reverted
!#


;Modify selections of notes. Reversing, Mirror and others.
;;;;;

;Modify a selection with a given proc
;;Example to reverse the selection: (NotationMagick::ModifySelectedObjects reverse)
;;Uses SchemeCopy and SchemePaste inside. Any sophisticated edit-proc has to deal with musobj functions
(define (NotationMagick::ModifySelectedObjects proc)
  (if (d-MarkStatus)
  	(let ()
		(define position (GetPosition))
		(define listy (SchemeCopy))
		(d-DeleteSelectionLeaveEmpty)
		(SchemePaste (proc listy))
		(apply d-GoToPosition position)
		#t)
	#f)) ; no selection
	
(define (NotationMagick::MirrorSelection axis)
  (NotationMagick::ModifySelectedObjects (lambda (copybuffer)
	(ProcessSchemeCopyBufferMusObj (lambda (current)
		(set!musobj.pitch current 
			;The function to change a single ANS object which is a list/chordmembers
			(map (lambda (note) (+ note (* 2 (- axis note)))) (musobj.pitch current)))) copybuffer))))
			
(define (NotationMagick::ReverseCopyBuffer copybuffer)
	(set! copybuffer (reverse copybuffer)) ; reverse the entire list first. 
	(map (lambda (current) 
			(if (not (musobj? current))
				(cond 
			  	((equal? (car current) 'TUPCLOSE) (set-car! current 'TUPOPEN))
			  	((equal? (car current) 'TUPOPEN) (set-car! current 'TUPCLOSE)) ; Switch tuplet end and start
			  	))
			current) ; if not just return the original object
		 copybuffer))
