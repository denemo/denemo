(use-modules (srfi srfi-1))
(use-modules (srfi srfi-8))
(use-modules (srfi srfi-13))
(use-modules (ice-9 optargs))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;Abstract Note System for Denemo;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;by Nils Gey;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; It makes it easy to calculate with notation while keeping in mind that
; there are basic note ("The White Keys") as well as sharp or flat notes
; and finally enharmonic notes (gisis) .
; Each note is represented by a number on base35 which points to an
; absolute notename in Lilypond syntax This is especially desinged to 
; make diatonic shifting, real transpostion and other modifications more
; easy for script-authors. 
; System by Nils Gey 2010 (thanks to Till Hartmann for table generation)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;Converter from Decimal to Base35;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;by Nils Gey;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;(ANS::basex->dec "num-string" radix) and (ANS::dec->basex num radix);;
;;;;;;;;;;;;;;;;;;;;;;;;;Various Math Functions;;;;;;;;;;;;;;;;;;;;;;;;;
; Important: All these work with strings because there is no native
; number base35 (or even base7 or base5) system in Scheme.

; Converts a single char to a number. Either its real number equivalent
; or according to the table from A=10 to Z=35. There is no check if you
; insert a "2" into binary or something else out of range. 
(define (ANS::char->number ichar) 
	(if (string->number (string ichar)) ; funny construction. first test if the current ichar can be converted to a number which means its actually the char/string version of a number 0-9 , so it can be converted for real. 
		(string->number (string ichar))
		(cond ; if not cond through the ichar if its A-Z or invalid. 
			((eq? ichar #\a) 10)
			((eq? ichar #\b) 11)
			((eq? ichar #\c) 12)
			((eq? ichar #\d) 13)
			((eq? ichar #\e) 14)
			((eq? ichar #\f) 15)
			((eq? ichar #\g) 16)
			((eq? ichar #\h) 17)
			((eq? ichar #\i) 18)
			((eq? ichar #\j) 19)
			((eq? ichar #\k) 20)
			((eq? ichar #\l) 21)
			((eq? ichar #\m) 22)
			((eq? ichar #\n) 23)
			((eq? ichar #\o) 24)
			((eq? ichar #\p) 25)
			((eq? ichar #\q) 26)
			((eq? ichar #\r) 27)
			((eq? ichar #\s) 28)
			((eq? ichar #\t) 29)
			((eq? ichar #\u) 30)
			((eq? ichar #\v) 31)
			((eq? ichar #\w) 32)
			((eq? ichar #\x) 33)
			((eq? ichar #\y) 34)
			((eq? ichar #\z) 35)
			(else "Error: Radix not supported or wrong input")
		)		 
	) 
)

;(basex->dec) converts any number with radix up to 35 to decimal with the following algorhythm. It takes the digits from left to right and iterates with *8 (for octal) +nextdigit
(define (ANS::basex->dec istring radix) ; needs a string-number and a real number as radix
	;oct243 -> dec
	;0 * 8 = 0
	;+ 2  =  2
	;
	;2 * 8 = 16
	;16 + 4 = 20
	;
	;20 * 8 = 160
	;160 + 3 = 163

 (define length 0)
 (define (*x+d returnn nextdigit)  ; helper function to do the math: first multiply the current sum with baseX then add the next digit.
	(+ nextdigit (* returnn radix)))
 
 (set! length (- (string-length istring) 1)) ; because string-ref (see below) counts from 0 we need counter to count from 0, too. but string-length counts from 1 so we need to reduce length to match the counter.

 (let loop ((counter 0) (returnn 0))
	(if (> counter length)
		returnn 
		(loop 
		    (+ counter 1) 
			(*x+d returnn (ANS::char->number (char-downcase (string-ref istring counter)))) ; get the digits from left to right with a counter and string-ref which returns a char, make sure the char is downcase.
		)		
	)	  
  ) 
)

;; ANS::remainder is a remainder variant that outputs a string instead of number and can also handle A-Z as numbers for radix > 10, which means if you do 32 / 11 it will return remainder A
(define (ANS::remainder number radix)
	(case (remainder number radix) ; if the radix is > 10 (and < 35)  we need to convert the returned string into a letter from A to Z.
			((0) "0")
			((1) "1")
			((2) "2")
			((3) "3")
			((4) "4")
			((5) "5")
			((6) "6")
			((7) "7")	
			((8) "8")
			((9) "9")
			((10) "a")
			((11) "b")
			((12) "c")
			((13) "d")
			((14) "e")			
			((15) "f")
			((16) "g")
			((17) "h")
			((18) "i")
			((19) "j")
			((20) "k")
			((21) "l")
			((22) "m")
			((23) "n")
			((24) "o")
			((25) "p")
			((26) "q")
			((27) "r")
			((28) "s")
			((29) "t")
			((30) "u")
			((31) "v")
			((32) "w")
			((33) "x")
			((34) "y")
			((35) "z")
			(else #f)
			;(else "tamrof tupni dab ro detroppus ton xidar :rorre") ; only 0-9 and A-Z. The error message is reversed because it will get reversed later, too.
	)		
)

(define (ANS::dec->basex inumber radix) ; Wants a real number and the radix as number.
	; This algorythm is used. dec358 -> base11: 
	; 358 / 11 = 32 rest 6 
	; 32 / 11 = 2 rest 10->A
	; 2 / 11 = 0 rest 2
	; => 2A6

	(let loop ((returnstring "") (worknumber inumber))
		
		(if (= (quotient worknumber radix) 0)
			(string-reverse (string-append returnstring (ANS::remainder worknumber radix))) ; if 0 the goal is reached, just add the final missing remainder and reverse the string-order as return-value (see algorhythm above)
			(loop 
				(string-append returnstring (ANS::remainder worknumber radix)) ; parameter 1
				(quotient worknumber radix)	; parameter 2
			)
		)		
	)
)

; For calculations convert all base 35 to decimal first and before returning return them back to base35.
(define (ANS::to35 n) ;n is a number base10
	(ANS::dec->basex n 35)
)

(define (ANS::to10 n) ; n is a string which represents a number base35
	(ANS::basex->dec n 35) 
)

(define (ANS::math op nums) ;wants  strings
 (ANS::to35 (apply op (map ANS::to10 nums)))
)

(define (ANS::compare op one two) ;wants strings
  (op (ANS::to10 one) (ANS::to10 two))
)

(define (ANS::+ . nums )  (ANS::math + nums))
(define (ANS::- . nums )  (ANS::math - nums))
(define (ANS::* . nums )  (ANS::math * nums))
(define (ANS::/ . nums )  (ANS::math / nums))
(define (ANS::> one two)  (ANS::compare > one two))
(define (ANS::>= one two)  (ANS::compare >= one two))
(define (ANS::<= one two)  (ANS::compare <= one two))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;Beginning of Abstract Note System;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;by Nils Gey;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;020 - A tone humans cannot hear anymore.
;420 - "Middle" c'
;620 - Soprano-Singers high C
;820 - Goes beyond the range of a modern piano

;+10 One accidental up jumps over to the next note after cisis 
;+50 One diatonic step, preserve accidentals
;+100 One Octave

(define ANS::NoteTable (make-hash-table 315))

(hashq-set! ANS::NoteTable (string->symbol "00") "ceses,,,")
	(hashq-set! ANS::NoteTable (string->symbol "10") "ces,,,")
	(hashq-set! ANS::NoteTable (string->symbol "20") "c,,,")
	(hashq-set! ANS::NoteTable (string->symbol "30") "cis,,,")
	(hashq-set! ANS::NoteTable (string->symbol "40") "cisis,,,")
(hashq-set! ANS::NoteTable (string->symbol "50") "deses,,,")
	(hashq-set! ANS::NoteTable (string->symbol "60") "des,,,")
	(hashq-set! ANS::NoteTable (string->symbol "70") "d,,,")
	(hashq-set! ANS::NoteTable (string->symbol "80") "dis,,,")
	(hashq-set! ANS::NoteTable (string->symbol "90") "disis,,,")
(hashq-set! ANS::NoteTable (string->symbol "a0") "eeses,,,")
	(hashq-set! ANS::NoteTable (string->symbol "b0") "ees,,,")
	(hashq-set! ANS::NoteTable (string->symbol "c0") "e,,,")
	(hashq-set! ANS::NoteTable (string->symbol "d0") "eis,,,")
	(hashq-set! ANS::NoteTable (string->symbol "e0") "eisis,,,")
(hashq-set! ANS::NoteTable (string->symbol "f0") "feses,,,")
	(hashq-set! ANS::NoteTable (string->symbol "g0") "fes,,,")
	(hashq-set! ANS::NoteTable (string->symbol "h0") "f,,,")
	(hashq-set! ANS::NoteTable (string->symbol "i0") "fis,,,")
	(hashq-set! ANS::NoteTable (string->symbol "j0") "fisis,,,")
(hashq-set! ANS::NoteTable (string->symbol "k0") "geses,,,")
	(hashq-set! ANS::NoteTable (string->symbol "l0") "ges,,,")
	(hashq-set! ANS::NoteTable (string->symbol "m0") "g,,,")
	(hashq-set! ANS::NoteTable (string->symbol "n0") "gis,,,")
	(hashq-set! ANS::NoteTable (string->symbol "o0") "gisis,,,")
(hashq-set! ANS::NoteTable (string->symbol "p0") "aeses,,,")
	(hashq-set! ANS::NoteTable (string->symbol "q0") "aes,,,")
	(hashq-set! ANS::NoteTable (string->symbol "r0") "a,,,")
	(hashq-set! ANS::NoteTable (string->symbol "s0") "ais,,,")
	(hashq-set! ANS::NoteTable (string->symbol "t0") "aisis,,,")
(hashq-set! ANS::NoteTable (string->symbol "u0") "beses,,,")
	(hashq-set! ANS::NoteTable (string->symbol "v0") "bes,,,")
	(hashq-set! ANS::NoteTable (string->symbol "w0") "b,,,")
	(hashq-set! ANS::NoteTable (string->symbol "x0") "bis,,,")
	(hashq-set! ANS::NoteTable (string->symbol "y0") "bisis,,,")


(hashq-set! ANS::NoteTable (string->symbol "100") "ceses,,")
	(hashq-set! ANS::NoteTable (string->symbol "110") "ces,,")
	(hashq-set! ANS::NoteTable (string->symbol "120") "c,,")
	(hashq-set! ANS::NoteTable (string->symbol "130") "cis,,")
	(hashq-set! ANS::NoteTable (string->symbol "140") "cisis,,")
(hashq-set! ANS::NoteTable (string->symbol "150") "deses,,")
	(hashq-set! ANS::NoteTable (string->symbol "160") "des,,")
	(hashq-set! ANS::NoteTable (string->symbol "170") "d,,")
	(hashq-set! ANS::NoteTable (string->symbol "180") "dis,,")
	(hashq-set! ANS::NoteTable (string->symbol "190") "disis,,")
(hashq-set! ANS::NoteTable (string->symbol "1a0") "eeses,,")
	(hashq-set! ANS::NoteTable (string->symbol "1b0") "ees,,")
	(hashq-set! ANS::NoteTable (string->symbol "1c0") "e,,")
	(hashq-set! ANS::NoteTable (string->symbol "1d0") "eis,,")
	(hashq-set! ANS::NoteTable (string->symbol "1e0") "eisis,,")
(hashq-set! ANS::NoteTable (string->symbol "1f0") "feses,,")
	(hashq-set! ANS::NoteTable (string->symbol "1g0") "fes,,")
	(hashq-set! ANS::NoteTable (string->symbol "1h0") "f,,")
	(hashq-set! ANS::NoteTable (string->symbol "1i0") "fis,,")
	(hashq-set! ANS::NoteTable (string->symbol "1j0") "fisis,,")
(hashq-set! ANS::NoteTable (string->symbol "1k0") "geses,,")
	(hashq-set! ANS::NoteTable (string->symbol "1l0") "ges,,")
	(hashq-set! ANS::NoteTable (string->symbol "1m0") "g,,")
	(hashq-set! ANS::NoteTable (string->symbol "1n0") "gis,,")
	(hashq-set! ANS::NoteTable (string->symbol "1o0") "gisis,,")
(hashq-set! ANS::NoteTable (string->symbol "1p0") "aeses,,")
	(hashq-set! ANS::NoteTable (string->symbol "1q0") "aes,,")
	(hashq-set! ANS::NoteTable (string->symbol "1r0") "a,,")
	(hashq-set! ANS::NoteTable (string->symbol "1s0") "ais,,")
	(hashq-set! ANS::NoteTable (string->symbol "1t0") "aisis,,")
(hashq-set! ANS::NoteTable (string->symbol "1u0") "beses,,")
	(hashq-set! ANS::NoteTable (string->symbol "1v0") "bes,,")
	(hashq-set! ANS::NoteTable (string->symbol "1w0") "b,,")
	(hashq-set! ANS::NoteTable (string->symbol "1x0") "bis,,")
	(hashq-set! ANS::NoteTable (string->symbol "1y0") "bisis,,")
	
(hashq-set! ANS::NoteTable (string->symbol "200") "ceses,")
	(hashq-set! ANS::NoteTable (string->symbol "210") "ces,")
	(hashq-set! ANS::NoteTable (string->symbol "220") "c,")
	(hashq-set! ANS::NoteTable (string->symbol "230") "cis,")
	(hashq-set! ANS::NoteTable (string->symbol "240") "cisis,")
(hashq-set! ANS::NoteTable (string->symbol "250") "deses,")
	(hashq-set! ANS::NoteTable (string->symbol "260") "des,")
	(hashq-set! ANS::NoteTable (string->symbol "270") "d,")
	(hashq-set! ANS::NoteTable (string->symbol "280") "dis,")
	(hashq-set! ANS::NoteTable (string->symbol "290") "disis,")
(hashq-set! ANS::NoteTable (string->symbol "2a0") "eeses,")
	(hashq-set! ANS::NoteTable (string->symbol "2b0") "ees,")
	(hashq-set! ANS::NoteTable (string->symbol "2c0") "e,")
	(hashq-set! ANS::NoteTable (string->symbol "2d0") "eis,")
	(hashq-set! ANS::NoteTable (string->symbol "2e0") "eisis,")
(hashq-set! ANS::NoteTable (string->symbol "2f0") "feses,")
	(hashq-set! ANS::NoteTable (string->symbol "2g0") "fes,")
	(hashq-set! ANS::NoteTable (string->symbol "2h0") "f,")
	(hashq-set! ANS::NoteTable (string->symbol "2i0") "fis,")
	(hashq-set! ANS::NoteTable (string->symbol "2j0") "fisis,")
(hashq-set! ANS::NoteTable (string->symbol "2k0") "geses,")
	(hashq-set! ANS::NoteTable (string->symbol "2l0") "ges,")
	(hashq-set! ANS::NoteTable (string->symbol "2m0") "g,")
	(hashq-set! ANS::NoteTable (string->symbol "2n0") "gis,")
	(hashq-set! ANS::NoteTable (string->symbol "2o0") "gisis,")
(hashq-set! ANS::NoteTable (string->symbol "2p0") "aeses,")
	(hashq-set! ANS::NoteTable (string->symbol "2q0") "aes,")
	(hashq-set! ANS::NoteTable (string->symbol "2r0") "a,")
	(hashq-set! ANS::NoteTable (string->symbol "2s0") "ais,")
	(hashq-set! ANS::NoteTable (string->symbol "2t0") "aisis,")
(hashq-set! ANS::NoteTable (string->symbol "2u0") "beses,")
	(hashq-set! ANS::NoteTable (string->symbol "2v0") "bes,")
	(hashq-set! ANS::NoteTable (string->symbol "2w0") "b,")
	(hashq-set! ANS::NoteTable (string->symbol "2x0") "bis,")
	(hashq-set! ANS::NoteTable (string->symbol "2y0") "bisis,")

(hashq-set! ANS::NoteTable (string->symbol "300") "ceses")
	(hashq-set! ANS::NoteTable (string->symbol "310") "ces")
	(hashq-set! ANS::NoteTable (string->symbol "320") "c")
	(hashq-set! ANS::NoteTable (string->symbol "330") "cis")
	(hashq-set! ANS::NoteTable (string->symbol "340") "cisis")
(hashq-set! ANS::NoteTable (string->symbol "350") "deses")
	(hashq-set! ANS::NoteTable (string->symbol "360") "des")
	(hashq-set! ANS::NoteTable (string->symbol "370") "d")
	(hashq-set! ANS::NoteTable (string->symbol "380") "dis")
	(hashq-set! ANS::NoteTable (string->symbol "390") "disis")
(hashq-set! ANS::NoteTable (string->symbol "3a0") "eeses")
	(hashq-set! ANS::NoteTable (string->symbol "3b0") "ees")
	(hashq-set! ANS::NoteTable (string->symbol "3c0") "e")
	(hashq-set! ANS::NoteTable (string->symbol "3d0") "eis")
	(hashq-set! ANS::NoteTable (string->symbol "3e0") "eisis")
(hashq-set! ANS::NoteTable (string->symbol "3f0") "feses")
	(hashq-set! ANS::NoteTable (string->symbol "3g0") "fes")
	(hashq-set! ANS::NoteTable (string->symbol "3h0") "f")
	(hashq-set! ANS::NoteTable (string->symbol "3i0") "fis")
	(hashq-set! ANS::NoteTable (string->symbol "3j0") "fisis")
(hashq-set! ANS::NoteTable (string->symbol "3k0") "geses")
	(hashq-set! ANS::NoteTable (string->symbol "3l0") "ges")
	(hashq-set! ANS::NoteTable (string->symbol "3m0") "g")
	(hashq-set! ANS::NoteTable (string->symbol "3n0") "gis")
	(hashq-set! ANS::NoteTable (string->symbol "3o0") "gisis")
(hashq-set! ANS::NoteTable (string->symbol "3p0") "aeses")
	(hashq-set! ANS::NoteTable (string->symbol "3q0") "aes")
	(hashq-set! ANS::NoteTable (string->symbol "3r0") "a")
	(hashq-set! ANS::NoteTable (string->symbol "3s0") "ais")
	(hashq-set! ANS::NoteTable (string->symbol "3t0") "aisis")
(hashq-set! ANS::NoteTable (string->symbol "3u0") "beses")
	(hashq-set! ANS::NoteTable (string->symbol "3v0") "bes")
	(hashq-set! ANS::NoteTable (string->symbol "3w0") "b")
	(hashq-set! ANS::NoteTable (string->symbol "3x0") "bis")
	(hashq-set! ANS::NoteTable (string->symbol "3y0") "bisis")
	
(hashq-set! ANS::NoteTable (string->symbol "400") "ceses'")
	(hashq-set! ANS::NoteTable (string->symbol "410") "ces'")
	(hashq-set! ANS::NoteTable (string->symbol "420") "c'")
	(hashq-set! ANS::NoteTable (string->symbol "430") "cis'")
	(hashq-set! ANS::NoteTable (string->symbol "440") "cisis'")
(hashq-set! ANS::NoteTable (string->symbol "450") "deses'")
	(hashq-set! ANS::NoteTable (string->symbol "460") "des'")
	(hashq-set! ANS::NoteTable (string->symbol "470") "d'")
	(hashq-set! ANS::NoteTable (string->symbol "480") "dis'")
	(hashq-set! ANS::NoteTable (string->symbol "490") "disis'")
(hashq-set! ANS::NoteTable (string->symbol "4a0") "eeses'")
	(hashq-set! ANS::NoteTable (string->symbol "4b0") "ees'")
	(hashq-set! ANS::NoteTable (string->symbol "4c0") "e'")
	(hashq-set! ANS::NoteTable (string->symbol "4d0") "eis'")
	(hashq-set! ANS::NoteTable (string->symbol "4e0") "eisis'")
(hashq-set! ANS::NoteTable (string->symbol "4f0") "feses'")
	(hashq-set! ANS::NoteTable (string->symbol "4g0") "fes'")
	(hashq-set! ANS::NoteTable (string->symbol "4h0") "f'")
	(hashq-set! ANS::NoteTable (string->symbol "4i0") "fis'")
	(hashq-set! ANS::NoteTable (string->symbol "4j0") "fisis'")
(hashq-set! ANS::NoteTable (string->symbol "4k0") "geses'")
	(hashq-set! ANS::NoteTable (string->symbol "4l0") "ges'")
	(hashq-set! ANS::NoteTable (string->symbol "4m0") "g'")
	(hashq-set! ANS::NoteTable (string->symbol "4n0") "gis'")
	(hashq-set! ANS::NoteTable (string->symbol "4o0") "gisis'")
(hashq-set! ANS::NoteTable (string->symbol "4p0") "aeses'")
	(hashq-set! ANS::NoteTable (string->symbol "4q0") "aes'")
	(hashq-set! ANS::NoteTable (string->symbol "4r0") "a'")
	(hashq-set! ANS::NoteTable (string->symbol "4s0") "ais'")
	(hashq-set! ANS::NoteTable (string->symbol "4t0") "aisis'")
(hashq-set! ANS::NoteTable (string->symbol "4u0") "beses'")
	(hashq-set! ANS::NoteTable (string->symbol "4v0") "bes'")
	(hashq-set! ANS::NoteTable (string->symbol "4w0") "b'")
	(hashq-set! ANS::NoteTable (string->symbol "4x0") "bis'")
	(hashq-set! ANS::NoteTable (string->symbol "4y0") "bisis'")
	
(hashq-set! ANS::NoteTable (string->symbol "500") "ceses''")
	(hashq-set! ANS::NoteTable (string->symbol "510") "ces''")
	(hashq-set! ANS::NoteTable (string->symbol "520") "c''")
	(hashq-set! ANS::NoteTable (string->symbol "530") "cis''")
	(hashq-set! ANS::NoteTable (string->symbol "540") "cisis''")
(hashq-set! ANS::NoteTable (string->symbol "550") "deses''")
	(hashq-set! ANS::NoteTable (string->symbol "560") "des''")
	(hashq-set! ANS::NoteTable (string->symbol "570") "d''")
	(hashq-set! ANS::NoteTable (string->symbol "580") "dis''")
	(hashq-set! ANS::NoteTable (string->symbol "590") "disis''")
(hashq-set! ANS::NoteTable (string->symbol "5a0") "eeses''")
	(hashq-set! ANS::NoteTable (string->symbol "5b0") "ees''")
	(hashq-set! ANS::NoteTable (string->symbol "5c0") "e''")
	(hashq-set! ANS::NoteTable (string->symbol "5d0") "eis''")
	(hashq-set! ANS::NoteTable (string->symbol "5e0") "eisis''")
(hashq-set! ANS::NoteTable (string->symbol "5f0") "feses''")
	(hashq-set! ANS::NoteTable (string->symbol "5g0") "fes''")
	(hashq-set! ANS::NoteTable (string->symbol "5h0") "f''")
	(hashq-set! ANS::NoteTable (string->symbol "5i0") "fis''")
	(hashq-set! ANS::NoteTable (string->symbol "5j0") "fisis''")
(hashq-set! ANS::NoteTable (string->symbol "5k0") "geses''")
	(hashq-set! ANS::NoteTable (string->symbol "5l0") "ges''")
	(hashq-set! ANS::NoteTable (string->symbol "5m0") "g''")
	(hashq-set! ANS::NoteTable (string->symbol "5n0") "gis''")
	(hashq-set! ANS::NoteTable (string->symbol "5o0") "gisis''")
(hashq-set! ANS::NoteTable (string->symbol "5p0") "aeses''")
	(hashq-set! ANS::NoteTable (string->symbol "5q0") "aes''")
	(hashq-set! ANS::NoteTable (string->symbol "5r0") "a''")
	(hashq-set! ANS::NoteTable (string->symbol "5s0") "ais''")
	(hashq-set! ANS::NoteTable (string->symbol "5t0") "aisis''")
(hashq-set! ANS::NoteTable (string->symbol "5u0") "beses''")
	(hashq-set! ANS::NoteTable (string->symbol "5v0") "bes''")
	(hashq-set! ANS::NoteTable (string->symbol "5w0") "b''")
	(hashq-set! ANS::NoteTable (string->symbol "5x0") "bis''")
	(hashq-set! ANS::NoteTable (string->symbol "5y0") "bisis''")	
	
(hashq-set! ANS::NoteTable (string->symbol "600") "ceses'''")
	(hashq-set! ANS::NoteTable (string->symbol "610") "ces'''")
	(hashq-set! ANS::NoteTable (string->symbol "620") "c'''")
	(hashq-set! ANS::NoteTable (string->symbol "630") "cis'''")
	(hashq-set! ANS::NoteTable (string->symbol "640") "cisis'''")
(hashq-set! ANS::NoteTable (string->symbol "650") "deses'''")
	(hashq-set! ANS::NoteTable (string->symbol "660") "des'''")
	(hashq-set! ANS::NoteTable (string->symbol "670") "d'''")
	(hashq-set! ANS::NoteTable (string->symbol "680") "dis'''")
	(hashq-set! ANS::NoteTable (string->symbol "690") "disis'''")
(hashq-set! ANS::NoteTable (string->symbol "6a0") "eeses'''")
	(hashq-set! ANS::NoteTable (string->symbol "6b0") "ees'''")
	(hashq-set! ANS::NoteTable (string->symbol "6c0") "e'''")
	(hashq-set! ANS::NoteTable (string->symbol "6d0") "eis'''")
	(hashq-set! ANS::NoteTable (string->symbol "6e0") "eisis'''")
(hashq-set! ANS::NoteTable (string->symbol "6f0") "feses'''")
	(hashq-set! ANS::NoteTable (string->symbol "6g0") "fes'''")
	(hashq-set! ANS::NoteTable (string->symbol "6h0") "f'''")
	(hashq-set! ANS::NoteTable (string->symbol "6i0") "fis'''")
	(hashq-set! ANS::NoteTable (string->symbol "6j0") "fisis'''")
(hashq-set! ANS::NoteTable (string->symbol "6k0") "geses'''")
	(hashq-set! ANS::NoteTable (string->symbol "6l0") "ges'''")
	(hashq-set! ANS::NoteTable (string->symbol "6m0") "g'''")
	(hashq-set! ANS::NoteTable (string->symbol "6n0") "gis'''")
	(hashq-set! ANS::NoteTable (string->symbol "6o0") "gisis'''")
(hashq-set! ANS::NoteTable (string->symbol "6p0") "aeses'''")
	(hashq-set! ANS::NoteTable (string->symbol "6q0") "aes'''")
	(hashq-set! ANS::NoteTable (string->symbol "6r0") "a'''")
	(hashq-set! ANS::NoteTable (string->symbol "6s0") "ais'''")
	(hashq-set! ANS::NoteTable (string->symbol "6t0") "aisis'''")
(hashq-set! ANS::NoteTable (string->symbol "6u0") "beses'''")
	(hashq-set! ANS::NoteTable (string->symbol "6v0") "bes'''")
	(hashq-set! ANS::NoteTable (string->symbol "6w0") "b'''")
	(hashq-set! ANS::NoteTable (string->symbol "6x0") "bis'''")
	(hashq-set! ANS::NoteTable (string->symbol "6y0") "bisis'''")	

(hashq-set! ANS::NoteTable (string->symbol "700") "ceses''''")
	(hashq-set! ANS::NoteTable (string->symbol "710") "ces''''")
	(hashq-set! ANS::NoteTable (string->symbol "720") "c''''")
	(hashq-set! ANS::NoteTable (string->symbol "730") "cis''''")
	(hashq-set! ANS::NoteTable (string->symbol "740") "cisis''''")
(hashq-set! ANS::NoteTable (string->symbol "750") "deses''''")
	(hashq-set! ANS::NoteTable (string->symbol "760") "des''''")
	(hashq-set! ANS::NoteTable (string->symbol "770") "d''''")
	(hashq-set! ANS::NoteTable (string->symbol "780") "dis''''")
	(hashq-set! ANS::NoteTable (string->symbol "790") "disis''''")
(hashq-set! ANS::NoteTable (string->symbol "7a0") "eeses''''")
	(hashq-set! ANS::NoteTable (string->symbol "7b0") "ees''''")
	(hashq-set! ANS::NoteTable (string->symbol "7c0") "e''''")
	(hashq-set! ANS::NoteTable (string->symbol "7d0") "eis''''")
	(hashq-set! ANS::NoteTable (string->symbol "7e0") "eisis''''")
(hashq-set! ANS::NoteTable (string->symbol "7f0") "feses''''")
	(hashq-set! ANS::NoteTable (string->symbol "7g0") "fes''''")
	(hashq-set! ANS::NoteTable (string->symbol "7h0") "f''''")
	(hashq-set! ANS::NoteTable (string->symbol "7i0") "fis''''")
	(hashq-set! ANS::NoteTable (string->symbol "7j0") "fisis''''")
(hashq-set! ANS::NoteTable (string->symbol "7k0") "geses''''")
	(hashq-set! ANS::NoteTable (string->symbol "7l0") "ges''''")
	(hashq-set! ANS::NoteTable (string->symbol "7m0") "g''''")
	(hashq-set! ANS::NoteTable (string->symbol "7n0") "gis''''")
	(hashq-set! ANS::NoteTable (string->symbol "7o0") "gisis''''")
(hashq-set! ANS::NoteTable (string->symbol "7p0") "aeses''''")
	(hashq-set! ANS::NoteTable (string->symbol "7q0") "aes''''")
	(hashq-set! ANS::NoteTable (string->symbol "7r0") "a''''")
	(hashq-set! ANS::NoteTable (string->symbol "7s0") "ais''''")
	(hashq-set! ANS::NoteTable (string->symbol "7t0") "aisis''''")
(hashq-set! ANS::NoteTable (string->symbol "7u0") "beses''''")
	(hashq-set! ANS::NoteTable (string->symbol "7v0") "bes''''")
	(hashq-set! ANS::NoteTable (string->symbol "7w0") "b''''")
	(hashq-set! ANS::NoteTable (string->symbol "7x0") "bis''''")
	(hashq-set! ANS::NoteTable (string->symbol "7y0") "bisis''''")	
	
(hashq-set! ANS::NoteTable (string->symbol "800") "ceses'''''")
	(hashq-set! ANS::NoteTable (string->symbol "810") "ces'''''")
	(hashq-set! ANS::NoteTable (string->symbol "820") "c'''''")
	(hashq-set! ANS::NoteTable (string->symbol "830") "cis'''''")
	(hashq-set! ANS::NoteTable (string->symbol "840") "cisis'''''")
(hashq-set! ANS::NoteTable (string->symbol "850") "deses'''''")
	(hashq-set! ANS::NoteTable (string->symbol "860") "des'''''")
	(hashq-set! ANS::NoteTable (string->symbol "870") "d'''''")
	(hashq-set! ANS::NoteTable (string->symbol "880") "dis'''''")
	(hashq-set! ANS::NoteTable (string->symbol "890") "disis'''''")
(hashq-set! ANS::NoteTable (string->symbol "8a0") "eeses'''''")
	(hashq-set! ANS::NoteTable (string->symbol "8b0") "ees'''''")
	(hashq-set! ANS::NoteTable (string->symbol "8c0") "e'''''")
	(hashq-set! ANS::NoteTable (string->symbol "8d0") "eis'''''")
	(hashq-set! ANS::NoteTable (string->symbol "8e0") "eisis'''''")
(hashq-set! ANS::NoteTable (string->symbol "8f0") "feses'''''")
	(hashq-set! ANS::NoteTable (string->symbol "8g0") "fes'''''")
	(hashq-set! ANS::NoteTable (string->symbol "8h0") "f'''''")
	(hashq-set! ANS::NoteTable (string->symbol "8i0") "fis'''''")
	(hashq-set! ANS::NoteTable (string->symbol "8j0") "fisis'''''")
(hashq-set! ANS::NoteTable (string->symbol "8k0") "geses'''''")
	(hashq-set! ANS::NoteTable (string->symbol "8l0") "ges'''''")
	(hashq-set! ANS::NoteTable (string->symbol "8m0") "g'''''")
	(hashq-set! ANS::NoteTable (string->symbol "8n0") "gis'''''")
	(hashq-set! ANS::NoteTable (string->symbol "8o0") "gisis'''''")
(hashq-set! ANS::NoteTable (string->symbol "8p0") "aeses'''''")
	(hashq-set! ANS::NoteTable (string->symbol "8q0") "aes'''''")
	(hashq-set! ANS::NoteTable (string->symbol "8r0") "a'''''")
	(hashq-set! ANS::NoteTable (string->symbol "8s0") "ais'''''")
	(hashq-set! ANS::NoteTable (string->symbol "8t0") "aisis'''''")
(hashq-set! ANS::NoteTable (string->symbol "8u0") "beses'''''")
	(hashq-set! ANS::NoteTable (string->symbol "8v0") "bes'''''")
	(hashq-set! ANS::NoteTable (string->symbol "8w0") "b'''''")
	(hashq-set! ANS::NoteTable (string->symbol "8x0") "bis'''''")
	(hashq-set! ANS::NoteTable (string->symbol "8y0") "bisis'''''")


;;;; Reverse Assignments
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define ANS::NoteTableR (make-hash-table 315))
(hashq-set! ANS::NoteTableR 'ceses,,, "00")
	(hashq-set! ANS::NoteTableR 'ces,,, "10")
	(hashq-set! ANS::NoteTableR 'c,,, "20")
	(hashq-set! ANS::NoteTableR 'cis,,, "30")
	(hashq-set! ANS::NoteTableR 'cisis,,, "40")
(hashq-set! ANS::NoteTableR 'deses,,, "50")
	(hashq-set! ANS::NoteTableR 'des,,, "60")
	(hashq-set! ANS::NoteTableR 'd,,, "70")
	(hashq-set! ANS::NoteTableR 'dis,,, "80")
	(hashq-set! ANS::NoteTableR 'disis,,, "90")
(hashq-set! ANS::NoteTableR 'eeses,,, "a0")
	(hashq-set! ANS::NoteTableR 'ees,,, "b0")
	(hashq-set! ANS::NoteTableR 'e,,, "c0")
	(hashq-set! ANS::NoteTableR 'eis,,, "d0")
	(hashq-set! ANS::NoteTableR 'eisis,,, "e0")
(hashq-set! ANS::NoteTableR 'feses,,, "f0")
	(hashq-set! ANS::NoteTableR 'fes,,, "g0")
	(hashq-set! ANS::NoteTableR 'f,,, "h0")
	(hashq-set! ANS::NoteTableR 'fis,,, "i0")
	(hashq-set! ANS::NoteTableR 'fisis,,, "j0")
(hashq-set! ANS::NoteTableR 'geses,,, "k0")
	(hashq-set! ANS::NoteTableR 'ges,,, "l0")
	(hashq-set! ANS::NoteTableR 'g,,, "m0")
	(hashq-set! ANS::NoteTableR 'gis,,, "n0")
	(hashq-set! ANS::NoteTableR 'gisis,,, "o0")
(hashq-set! ANS::NoteTableR 'aeses,,, "p0")
	(hashq-set! ANS::NoteTableR 'aes,,, "q0")
	(hashq-set! ANS::NoteTableR 'a,,, "r0")
	(hashq-set! ANS::NoteTableR 'ais,,, "s0")
	(hashq-set! ANS::NoteTableR 'aisis,,, "t0")
(hashq-set! ANS::NoteTableR 'beses,,, "u0")
	(hashq-set! ANS::NoteTableR 'bes,,, "v0")
	(hashq-set! ANS::NoteTableR 'b,,, "w0")
	(hashq-set! ANS::NoteTableR 'bis,,, "x0")
	(hashq-set! ANS::NoteTableR 'bisis,,, "y0")


(hashq-set! ANS::NoteTableR 'ceses,, "100")
	(hashq-set! ANS::NoteTableR 'ces,, "110")
	(hashq-set! ANS::NoteTableR 'c,, "120")
	(hashq-set! ANS::NoteTableR 'cis,, "130")
	(hashq-set! ANS::NoteTableR 'cisis,, "140")
(hashq-set! ANS::NoteTableR 'deses,, "150")
	(hashq-set! ANS::NoteTableR 'des,, "160")
	(hashq-set! ANS::NoteTableR 'd,, "170")
	(hashq-set! ANS::NoteTableR 'dis,, "180")
	(hashq-set! ANS::NoteTableR 'disis,, "190")
(hashq-set! ANS::NoteTableR 'eeses,, "1a0")
	(hashq-set! ANS::NoteTableR 'ees,, "1b0")
	(hashq-set! ANS::NoteTableR 'e,, "1c0")
	(hashq-set! ANS::NoteTableR 'eis,, "1d0")
	(hashq-set! ANS::NoteTableR 'eisis,, "1e0")
(hashq-set! ANS::NoteTableR 'feses,, "1f0")
	(hashq-set! ANS::NoteTableR 'fes,, "1g0")
	(hashq-set! ANS::NoteTableR 'f,, "1h0")
	(hashq-set! ANS::NoteTableR 'fis,, "1i0")
	(hashq-set! ANS::NoteTableR 'fisis,, "1j0")
(hashq-set! ANS::NoteTableR 'geses,, "1k0")
	(hashq-set! ANS::NoteTableR 'ges,, "1l0")
	(hashq-set! ANS::NoteTableR 'g,, "1m0")
	(hashq-set! ANS::NoteTableR 'gis,, "1n0")
	(hashq-set! ANS::NoteTableR 'gisis,, "1o0")
(hashq-set! ANS::NoteTableR 'aeses,, "1p0")
	(hashq-set! ANS::NoteTableR 'aes,, "1q0")
	(hashq-set! ANS::NoteTableR 'a,, "1r0")
	(hashq-set! ANS::NoteTableR 'ais,, "1s0")
	(hashq-set! ANS::NoteTableR 'aisis,, "1t0")
(hashq-set! ANS::NoteTableR 'beses,, "1u0")
	(hashq-set! ANS::NoteTableR 'bes,, "1v0")
	(hashq-set! ANS::NoteTableR 'b,, "1w0")
	(hashq-set! ANS::NoteTableR 'bis,, "1x0")
	(hashq-set! ANS::NoteTableR 'bisis,, "1y0")
	
(hashq-set! ANS::NoteTableR 'ceses, "200")
	(hashq-set! ANS::NoteTableR 'ces, "210")
	(hashq-set! ANS::NoteTableR 'c, "220")
	(hashq-set! ANS::NoteTableR 'cis, "230")
	(hashq-set! ANS::NoteTableR 'cisis, "240")
(hashq-set! ANS::NoteTableR 'deses, "250")
	(hashq-set! ANS::NoteTableR 'des, "260")
	(hashq-set! ANS::NoteTableR 'd, "270")
	(hashq-set! ANS::NoteTableR 'dis, "280")
	(hashq-set! ANS::NoteTableR 'disis, "290")
(hashq-set! ANS::NoteTableR 'eeses, "2a0")
	(hashq-set! ANS::NoteTableR 'ees, "2b0")
	(hashq-set! ANS::NoteTableR 'e, "2c0")
	(hashq-set! ANS::NoteTableR 'eis, "2d0")
	(hashq-set! ANS::NoteTableR 'eisis, "2e0")
(hashq-set! ANS::NoteTableR 'feses, "2f0")
	(hashq-set! ANS::NoteTableR 'fes, "2g0")
	(hashq-set! ANS::NoteTableR 'f, "2h0")
	(hashq-set! ANS::NoteTableR 'fis, "2i0")
	(hashq-set! ANS::NoteTableR 'fisis, "2j0")
(hashq-set! ANS::NoteTableR 'geses, "2k0")
	(hashq-set! ANS::NoteTableR 'ges, "2l0")
	(hashq-set! ANS::NoteTableR 'g, "2m0")
	(hashq-set! ANS::NoteTableR 'gis, "2n0")
	(hashq-set! ANS::NoteTableR 'gisis, "2o0")
(hashq-set! ANS::NoteTableR 'aeses, "2p0")
	(hashq-set! ANS::NoteTableR 'aes, "2q0")
	(hashq-set! ANS::NoteTableR 'a, "2r0")
	(hashq-set! ANS::NoteTableR 'ais, "2s0")
	(hashq-set! ANS::NoteTableR 'aisis, "2t0")
(hashq-set! ANS::NoteTableR 'beses, "2u0")
	(hashq-set! ANS::NoteTableR 'bes, "2v0")
	(hashq-set! ANS::NoteTableR 'b, "2w0")
	(hashq-set! ANS::NoteTableR 'bis, "2x0")
	(hashq-set! ANS::NoteTableR 'bisis, "2y0")

(hashq-set! ANS::NoteTableR 'ceses "300")
	(hashq-set! ANS::NoteTableR 'ces "310")
	(hashq-set! ANS::NoteTableR 'c "320")
	(hashq-set! ANS::NoteTableR 'cis "330")
	(hashq-set! ANS::NoteTableR 'cisis "340")
(hashq-set! ANS::NoteTableR 'deses "350")
	(hashq-set! ANS::NoteTableR 'des "360")
	(hashq-set! ANS::NoteTableR 'd "370")
	(hashq-set! ANS::NoteTableR 'dis "380")
	(hashq-set! ANS::NoteTableR 'disis "390")
(hashq-set! ANS::NoteTableR 'eeses "3a0")
	(hashq-set! ANS::NoteTableR 'ees "3b0")
	(hashq-set! ANS::NoteTableR 'e "3c0")
	(hashq-set! ANS::NoteTableR 'eis "3d0")
	(hashq-set! ANS::NoteTableR 'eisis "3e0")
(hashq-set! ANS::NoteTableR 'feses "3f0")
	(hashq-set! ANS::NoteTableR 'fes "3g0")
	(hashq-set! ANS::NoteTableR 'f "3h0")
	(hashq-set! ANS::NoteTableR 'fis "3i0")
	(hashq-set! ANS::NoteTableR 'fisis "3j0")
(hashq-set! ANS::NoteTableR 'geses "3k0")
	(hashq-set! ANS::NoteTableR 'ges "3l0")
	(hashq-set! ANS::NoteTableR 'g "3m0")
	(hashq-set! ANS::NoteTableR 'gis "3n0")
	(hashq-set! ANS::NoteTableR 'gisis "3o0")
(hashq-set! ANS::NoteTableR 'aeses "3p0")
	(hashq-set! ANS::NoteTableR 'aes "3q0")
	(hashq-set! ANS::NoteTableR 'a "3r0")
	(hashq-set! ANS::NoteTableR 'ais "3s0")
	(hashq-set! ANS::NoteTableR 'aisis "3t0")
(hashq-set! ANS::NoteTableR 'beses "3u0")
	(hashq-set! ANS::NoteTableR 'bes "3v0")
	(hashq-set! ANS::NoteTableR 'b "3w0")
	(hashq-set! ANS::NoteTableR 'bis "3x0")
	(hashq-set! ANS::NoteTableR 'bisis "3y0")
	
(hashq-set! ANS::NoteTableR 'ceses' "400")
	(hashq-set! ANS::NoteTableR 'ces' "410")
	(hashq-set! ANS::NoteTableR 'c' "420")
	(hashq-set! ANS::NoteTableR 'cis' "430")
	(hashq-set! ANS::NoteTableR 'cisis' "440")
(hashq-set! ANS::NoteTableR 'deses' "450")
	(hashq-set! ANS::NoteTableR 'des' "460")
	(hashq-set! ANS::NoteTableR 'd' "470")
	(hashq-set! ANS::NoteTableR 'dis' "480")
	(hashq-set! ANS::NoteTableR 'disis' "490")
(hashq-set! ANS::NoteTableR 'eeses' "4a0")
	(hashq-set! ANS::NoteTableR 'ees' "4b0")
	(hashq-set! ANS::NoteTableR 'e' "4c0")
	(hashq-set! ANS::NoteTableR 'eis' "4d0")
	(hashq-set! ANS::NoteTableR 'eisis' "4e0")
(hashq-set! ANS::NoteTableR 'feses' "4f0")
	(hashq-set! ANS::NoteTableR 'fes' "4g0")
	(hashq-set! ANS::NoteTableR 'f' "4h0")
	(hashq-set! ANS::NoteTableR 'fis' "4i0")
	(hashq-set! ANS::NoteTableR 'fisis' "4j0")
(hashq-set! ANS::NoteTableR 'geses' "4k0")
	(hashq-set! ANS::NoteTableR 'ges' "4l0")
	(hashq-set! ANS::NoteTableR 'g' "4m0")
	(hashq-set! ANS::NoteTableR 'gis' "4n0")
	(hashq-set! ANS::NoteTableR 'gisis' "4o0")
(hashq-set! ANS::NoteTableR 'aeses' "4p0")
	(hashq-set! ANS::NoteTableR 'aes' "4q0")
	(hashq-set! ANS::NoteTableR 'a' "4r0")
	(hashq-set! ANS::NoteTableR 'ais' "4s0")
	(hashq-set! ANS::NoteTableR 'aisis' "4t0")
(hashq-set! ANS::NoteTableR 'beses' "4u0")
	(hashq-set! ANS::NoteTableR 'bes' "4v0")
	(hashq-set! ANS::NoteTableR 'b' "4w0")
	(hashq-set! ANS::NoteTableR 'bis' "4x0")
	(hashq-set! ANS::NoteTableR 'bisis' "4y0")
	
(hashq-set! ANS::NoteTableR 'ceses'' "500")
	(hashq-set! ANS::NoteTableR 'ces'' "510")
	(hashq-set! ANS::NoteTableR 'c'' "520")
	(hashq-set! ANS::NoteTableR 'cis'' "530")
	(hashq-set! ANS::NoteTableR 'cisis'' "540")
(hashq-set! ANS::NoteTableR 'deses'' "550")
	(hashq-set! ANS::NoteTableR 'des'' "560")
	(hashq-set! ANS::NoteTableR 'd'' "570")
	(hashq-set! ANS::NoteTableR 'dis'' "580")
	(hashq-set! ANS::NoteTableR 'disis'' "590")
(hashq-set! ANS::NoteTableR 'eeses'' "5a0")
	(hashq-set! ANS::NoteTableR 'ees'' "5b0")
	(hashq-set! ANS::NoteTableR 'e'' "5c0")
	(hashq-set! ANS::NoteTableR 'eis'' "5d0")
	(hashq-set! ANS::NoteTableR 'eisis'' "5e0")
(hashq-set! ANS::NoteTableR 'feses'' "5f0")
	(hashq-set! ANS::NoteTableR 'fes'' "5g0")
	(hashq-set! ANS::NoteTableR 'f'' "5h0")
	(hashq-set! ANS::NoteTableR 'fis'' "5i0")
	(hashq-set! ANS::NoteTableR 'fisis'' "5j0")
(hashq-set! ANS::NoteTableR 'geses'' "5k0")
	(hashq-set! ANS::NoteTableR 'ges'' "5l0")
	(hashq-set! ANS::NoteTableR 'g'' "5m0")
	(hashq-set! ANS::NoteTableR 'gis'' "5n0")
	(hashq-set! ANS::NoteTableR 'gisis'' "5o0")
(hashq-set! ANS::NoteTableR 'aeses'' "5p0")
	(hashq-set! ANS::NoteTableR 'aes'' "5q0")
	(hashq-set! ANS::NoteTableR 'a'' "5r0")
	(hashq-set! ANS::NoteTableR 'ais'' "5s0")
	(hashq-set! ANS::NoteTableR 'aisis'' "5t0")
(hashq-set! ANS::NoteTableR 'beses'' "5u0")
	(hashq-set! ANS::NoteTableR 'bes'' "5v0")
	(hashq-set! ANS::NoteTableR 'b'' "5w0")
	(hashq-set! ANS::NoteTableR 'bis'' "5x0")
	(hashq-set! ANS::NoteTableR 'bisis'' "5y0")	
	
(hashq-set! ANS::NoteTableR 'ceses''' "600")
	(hashq-set! ANS::NoteTableR 'ces''' "610")
	(hashq-set! ANS::NoteTableR 'c''' "620")
	(hashq-set! ANS::NoteTableR 'cis''' "630")
	(hashq-set! ANS::NoteTableR 'cisis''' "640")
(hashq-set! ANS::NoteTableR 'deses''' "650")
	(hashq-set! ANS::NoteTableR 'des''' "660")
	(hashq-set! ANS::NoteTableR 'd''' "670")
	(hashq-set! ANS::NoteTableR 'dis''' "680")
	(hashq-set! ANS::NoteTableR 'disis''' "690")
(hashq-set! ANS::NoteTableR 'eeses''' "6a0")
	(hashq-set! ANS::NoteTableR 'ees''' "6b0")
	(hashq-set! ANS::NoteTableR 'e''' "6c0")
	(hashq-set! ANS::NoteTableR 'eis''' "6d0")
	(hashq-set! ANS::NoteTableR 'eisis''' "6e0")
(hashq-set! ANS::NoteTableR 'feses''' "6f0")
	(hashq-set! ANS::NoteTableR 'fes''' "6g0")
	(hashq-set! ANS::NoteTableR 'f''' "6h0")
	(hashq-set! ANS::NoteTableR 'fis''' "6i0")
	(hashq-set! ANS::NoteTableR 'fisis''' "6j0")
(hashq-set! ANS::NoteTableR 'geses''' "6k0")
	(hashq-set! ANS::NoteTableR 'ges''' "6l0")
	(hashq-set! ANS::NoteTableR 'g''' "6m0")
	(hashq-set! ANS::NoteTableR 'gis''' "6n0")
	(hashq-set! ANS::NoteTableR 'gisis''' "6o0")
(hashq-set! ANS::NoteTableR 'aeses''' "6p0")
	(hashq-set! ANS::NoteTableR 'aes''' "6q0")
	(hashq-set! ANS::NoteTableR 'a''' "6r0")
	(hashq-set! ANS::NoteTableR 'ais''' "6s0")
	(hashq-set! ANS::NoteTableR 'aisis''' "6t0")
(hashq-set! ANS::NoteTableR 'beses''' "6u0")
	(hashq-set! ANS::NoteTableR 'bes''' "6v0")
	(hashq-set! ANS::NoteTableR 'b''' "6w0")
	(hashq-set! ANS::NoteTableR 'bis''' "6x0")
	(hashq-set! ANS::NoteTableR 'bisis''' "6y0")	

(hashq-set! ANS::NoteTableR 'ceses'''' "700")
	(hashq-set! ANS::NoteTableR 'ces'''' "710")
	(hashq-set! ANS::NoteTableR 'c'''' "720")
	(hashq-set! ANS::NoteTableR 'cis'''' "730")
	(hashq-set! ANS::NoteTableR 'cisis'''' "740")
(hashq-set! ANS::NoteTableR 'deses'''' "750")
	(hashq-set! ANS::NoteTableR 'des'''' "760")
	(hashq-set! ANS::NoteTableR 'd'''' "770")
	(hashq-set! ANS::NoteTableR 'dis'''' "780")
	(hashq-set! ANS::NoteTableR 'disis'''' "790")
(hashq-set! ANS::NoteTableR 'eeses'''' "7a0")
	(hashq-set! ANS::NoteTableR 'ees'''' "7b0")
	(hashq-set! ANS::NoteTableR 'e'''' "7c0")
	(hashq-set! ANS::NoteTableR 'eis'''' "7d0")
	(hashq-set! ANS::NoteTableR 'eisis'''' "7e0")
(hashq-set! ANS::NoteTableR 'feses'''' "7f0")
	(hashq-set! ANS::NoteTableR 'fes'''' "7g0")
	(hashq-set! ANS::NoteTableR 'f'''' "7h0")
	(hashq-set! ANS::NoteTableR 'fis'''' "7i0")
	(hashq-set! ANS::NoteTableR 'fisis'''' "7j0")
(hashq-set! ANS::NoteTableR 'geses'''' "7k0")
	(hashq-set! ANS::NoteTableR 'ges'''' "7l0")
	(hashq-set! ANS::NoteTableR 'g'''' "7m0")
	(hashq-set! ANS::NoteTableR 'gis'''' "7n0")
	(hashq-set! ANS::NoteTableR 'gisis'''' "7o0")
(hashq-set! ANS::NoteTableR 'aeses'''' "7p0")
	(hashq-set! ANS::NoteTableR 'aes'''' "7q0")
	(hashq-set! ANS::NoteTableR 'a'''' "7r0")
	(hashq-set! ANS::NoteTableR 'ais'''' "7s0")
	(hashq-set! ANS::NoteTableR 'aisis'''' "7t0")
(hashq-set! ANS::NoteTableR 'beses'''' "7u0")
	(hashq-set! ANS::NoteTableR 'bes'''' "7v0")
	(hashq-set! ANS::NoteTableR 'b'''' "7w0")
	(hashq-set! ANS::NoteTableR 'bis'''' "7x0")
	(hashq-set! ANS::NoteTableR 'bisis'''' "7y0")	
	
(hashq-set! ANS::NoteTableR 'ceses''''' "800")
	(hashq-set! ANS::NoteTableR 'ces''''' "810")
	(hashq-set! ANS::NoteTableR 'c''''' "820")
	(hashq-set! ANS::NoteTableR 'cis''''' "830")
	(hashq-set! ANS::NoteTableR 'cisis''''' "840")
(hashq-set! ANS::NoteTableR 'deses''''' "850")
	(hashq-set! ANS::NoteTableR 'des''''' "860")
	(hashq-set! ANS::NoteTableR 'd''''' "870")
	(hashq-set! ANS::NoteTableR 'dis''''' "880")
	(hashq-set! ANS::NoteTableR 'disis''''' "890")
(hashq-set! ANS::NoteTableR 'eeses''''' "8a0")
	(hashq-set! ANS::NoteTableR 'ees''''' "8b0")
	(hashq-set! ANS::NoteTableR 'e''''' "8c0")
	(hashq-set! ANS::NoteTableR 'eis''''' "8d0")
	(hashq-set! ANS::NoteTableR 'eisis''''' "8e0")
(hashq-set! ANS::NoteTableR 'feses''''' "8f0")
	(hashq-set! ANS::NoteTableR 'fes''''' "8g0")
	(hashq-set! ANS::NoteTableR 'f''''' "8h0")
	(hashq-set! ANS::NoteTableR 'fis''''' "8i0")
	(hashq-set! ANS::NoteTableR 'fisis''''' "8j0")
(hashq-set! ANS::NoteTableR 'geses''''' "8k0")
	(hashq-set! ANS::NoteTableR 'ges''''' "8l0")
	(hashq-set! ANS::NoteTableR 'g''''' "8m0")
	(hashq-set! ANS::NoteTableR 'gis''''' "8n0")
	(hashq-set! ANS::NoteTableR 'gisis''''' "8o0")
(hashq-set! ANS::NoteTableR 'aeses''''' "8p0")
	(hashq-set! ANS::NoteTableR 'aes''''' "8q0")
	(hashq-set! ANS::NoteTableR 'a''''' "8r0")
	(hashq-set! ANS::NoteTableR 'ais''''' "8s0")
	(hashq-set! ANS::NoteTableR 'aisis''''' "8t0")
(hashq-set! ANS::NoteTableR 'beses''''' "8u0")
	(hashq-set! ANS::NoteTableR 'bes''''' "8v0")
	(hashq-set! ANS::NoteTableR 'b''''' "8w0")
	(hashq-set! ANS::NoteTableR 'bis''''' "8x0")
	(hashq-set! ANS::NoteTableR 'bisis''''' "8y0")


;;;; The pillar of filth
;;;; To calculate real and correct intervals you need the pillar of fifth with 35 steps for each realistic notename (and 4 unrealistic ones)
;;;;;;;;;;;;;;;;;;;;;;;;
; First the index where a notename/ANS value can be found in the pillar, which is actually a list.
(define ANS::PillarOfFifthIndex (make-hash-table))
	(hashq-set! ANS::PillarOfFifthIndex (string->symbol "f") 0) ;feses
	(hashq-set! ANS::PillarOfFifthIndex (string->symbol "0") 1) ;ceses
	(hashq-set! ANS::PillarOfFifthIndex (string->symbol "k") 2) ;geses	
	(hashq-set! ANS::PillarOfFifthIndex (string->symbol "5") 3) ;deses
	(hashq-set! ANS::PillarOfFifthIndex (string->symbol "p") 4) ;aeses
	(hashq-set! ANS::PillarOfFifthIndex (string->symbol "a") 5) ;eeses
	(hashq-set! ANS::PillarOfFifthIndex (string->symbol "u") 6) ;beses 
	(hashq-set! ANS::PillarOfFifthIndex (string->symbol "g") 7) ;fes
	(hashq-set! ANS::PillarOfFifthIndex (string->symbol "1") 8) ;ces
	(hashq-set! ANS::PillarOfFifthIndex (string->symbol "l") 9) ;ges
	(hashq-set! ANS::PillarOfFifthIndex (string->symbol "6") 10) ;des
	(hashq-set! ANS::PillarOfFifthIndex (string->symbol "q") 11) ;aes
	(hashq-set! ANS::PillarOfFifthIndex (string->symbol "b") 12) ;ees
	(hashq-set! ANS::PillarOfFifthIndex (string->symbol "v") 13) ;bes
	(hashq-set! ANS::PillarOfFifthIndex (string->symbol "h") 14) ;f
	(hashq-set! ANS::PillarOfFifthIndex (string->symbol "2") 15) ;c
	(hashq-set! ANS::PillarOfFifthIndex (string->symbol "m") 16) ;g
	(hashq-set! ANS::PillarOfFifthIndex (string->symbol "7") 17) ;d
	(hashq-set! ANS::PillarOfFifthIndex (string->symbol "r") 18) ;a
	(hashq-set! ANS::PillarOfFifthIndex (string->symbol "c") 19) ;e
	(hashq-set! ANS::PillarOfFifthIndex (string->symbol "w") 20) ;b
	(hashq-set! ANS::PillarOfFifthIndex (string->symbol "i") 21) ;fis
	(hashq-set! ANS::PillarOfFifthIndex (string->symbol "3") 22) ;cis
	(hashq-set! ANS::PillarOfFifthIndex (string->symbol "n") 23) ;gis
	(hashq-set! ANS::PillarOfFifthIndex (string->symbol "8") 24) ;dis
	(hashq-set! ANS::PillarOfFifthIndex (string->symbol "s") 25) ;ais
	(hashq-set! ANS::PillarOfFifthIndex (string->symbol "d") 26) ;eis
	(hashq-set! ANS::PillarOfFifthIndex (string->symbol "x") 27) ;bis
	(hashq-set! ANS::PillarOfFifthIndex (string->symbol "j") 28) ;fisis
	(hashq-set! ANS::PillarOfFifthIndex (string->symbol "4") 29) ;cisis
	(hashq-set! ANS::PillarOfFifthIndex (string->symbol "o") 30) ;gisis
	(hashq-set! ANS::PillarOfFifthIndex (string->symbol "9") 31) ;disis
	(hashq-set! ANS::PillarOfFifthIndex (string->symbol "t") 32) ;aisis
	(hashq-set! ANS::PillarOfFifthIndex (string->symbol "e") 33) ;eisis	
	(hashq-set! ANS::PillarOfFifthIndex (string->symbol "y") 34) ;bisis


;The list is the actual pillar of fifth. Stepping left and right can be done by calculating a list-ref index value.	
(define ANS::PillarOfFifth 
 (list 
	"f" ;feses
	"0" ;ceses
	"k" ;geses	
	"5" ;deses
	"p" ;aeses
	"a" ;eeses
	"u" ;beses 
	"g" ;fes
	"1" ;ces
	"l" ;ges
	"6" ;des
	"q" ;aes
	"b" ;ees
	"v" ;bes
	"h" ;f
	"2" ;c
	"m" ;g
	"7" ;d
	"r" ;a
	"c" ;e
	"w" ;b
	"i" ;fis
	"3" ;cis
	"n" ;gis
	"8" ;dis
	"s" ;ais
	"d" ;eis
	"x" ;bis
	"j" ;fisis
	"4" ;cisis
	"o" ;gisis
	"9" ;disis
	"t" ;aisis
	"e" ;eisis	
	"y" ;bisis
 )
)

; A table to translate the human readable "m2" for minor second into steps of fifth left or right in the pillar.
; TODO: Augmented and Diminished intervals, octave and >octave. For octaves it will return the same note again, so that must be passt to the calcUp and downFunctions. Maybe don't even do octaves here.
; TODO: this function only should be for < octave. Octave is more easy to do with CalcUp/Down followed by octave up/down
(define (ANS::IntervalGetSteps target)
	(cond
		((or (eq? target 'p1) (eq? target 'P1)) 0)			
		((eq? target 'm2) -5)			
		((eq? target 'M2) 2)			
		((eq? target 'm3) -3)
		((eq? target 'M3) 4)	
		((or (eq? target 'p4) (eq? target 'P4)) -1)	
		((or (eq? target 't) (eq? target 'T)) 6)
		((or (eq? target 'p5) (eq? target 'P5)) 1)
		((eq? target 'm6) -4)			
		((eq? target 'M6) 3)		
		((eq? target 'm7) -2)	
		((eq? target 'M7) 5)		
		(else #f)
	)		
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;Functions;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




;Checks if a string is a lilypond note compatible with (d-ChangeChordNotes)
;(define (ANS::IsLily?)
;	
;)

(define (ANS::Ly2Ans lilynote) ;wants string, returns string
	(hashq-ref ANS::NoteTableR (string->symbol lilynote))
)

(define (ANS::Ans2Ly ansNote) ;wants string, returns strings.
	(hashq-ref ANS::NoteTable (string->symbol ansNote))	
)

; The main function to get notes from Denemo
; Opposite is ANS::ChangeChordNotes
; For singles and chords. Returns a list of ANS string-numbers as chord.
(define (ANS::GetChordNotes)
	(define lilylist (string-tokenize (d-GetNotes)))
	(map ANS::Ly2Ans lilylist)
)

; Extract the note from an ANS-string, without any octave or the tailing zero. Return as string.
(define (ANS::GetNote ansNote) 
		(string-drop-right 
			(string-take-right ansNote 2 )  1  )			
)

;Extract the octave as three digit base35 number/string. 
(define (ANS::GetOctave ansNote) 
	(if (= 2 (string-length ansNote))
		"0" ; its the first octave which has no digit
		(string-append 
			(string-drop-right (string-take-right ansNote 3 )  2  )
		    "00" )	; add two tailing 00 to make it an octave value
	)
)

; Change chord/note to another chord/note
; Opposite is ANS::GetChordNotes
; Wants a string or list of ANS note-strings 
(define (ANS::ChangeChordNotes ansNotes)
	(define newList '())
	(if (list? ansNotes)
		#t
		(set! ansNotes (list ansNotes)))
	(set! newList (map ANS::Ans2Ly ansNotes))
	(d-ChangeChordNotes (string-join newList))
)

;Insert A note/chord on Denemos cursor position 
; wants a string or  a list of ANS note-strings as chord.
; Optional duration and number of dots. returns #t or #f
(define* (ANS::InsertNotes ansNotes #:optional (dots #f) (duration #f) )
	;TODO: Check if these are valid notes.
	(d-InsertA) ; TODO: This is a hack. 
	(d-MoveCursorLeft)
	(ANS::ChangeChordNotes ansNotes)
	(if duration  ;If user gave duration parameter. Does not test if the duration is a valid number
			(eval-string (string-append "(d-Change" (number->string duration) ")"))) ; FIXME eval string should be gone
	(if (and dots (not (= dots 0))) ;If the user gave 0 as durations ignore that as well
			(let loop ((count 0)) 
				(d-AddDot)
				(if (< count (- dots 1))
				(loop (+ 1 count)))))
	(d-MoveCursorRight)
)

; Return the natural, "white key" version of an ansNote.
(define (ANS::GetWhiteKey ansNote) 
	(define decNote (/ (ANS::to10 ansNote)  35))
 
	(ANS::to35 
	(+ (* (quotient decNote 5) 175) 70 )
	)
)

;Alteration adds a sharp, flat or nothing to a ans-note. Returns an ANS
;number-string note. Wants an ans number-string and a procedure that
;will return either 0, 1 or -1. 
(define (ANS::Alteration ansNote caserator)
	(case caserator
	  ((0)		ansNote) ; natural, no change
	  ((1)		(ANS::+ ansNote  "10")) ;sharp
	  ((-1)		(ANS::- ansNote  "10")) ;flat
	  (else   #f ) ; someone might introduce some insane feature in the future where you can add doublecrosses or similar to a keysig. Or maybe there is even a real usage for micotonals like turkish maqam.
	 )
)


;IntervalCalc wants an ANS note as root and an interval like "m2", "M6" or "p5" returns an ANS value as string.
(define (ANS::IntervalCalcPrototype op ansNote interval)
 (let (	(targ (ANS::IntervalGetSteps interval)) 
		(root (hashq-ref ANS::PillarOfFifthIndex (string->symbol (ANS::GetNote ansNote)) ))		
	  )
	(list-ref ANS::PillarOfFifth (+ root (* op targ)))	
  )
)

(define (ANS::IntervalCalcUp ansNote interval)
	(define result	(ANS::IntervalCalcPrototype 1 ansNote interval))
	(define octave (ANS::GetOctave ansNote))	
	(if (ANS::>= result (ANS::GetNote ansNote)) ; test if the calulated interval, just one digit until now, will be in the same octave which means its note-value itself is higher or equal (in case of p1) compared to the root. Or it seems to be lower, in this case we need to add an octave because we really want it higher :)
		 (ANS::+ octave (ANS::* result "10")) ;its still in the same octave, just recalculate
		 (ANS::+ octave "100" (ANS::* result "10")) ;+100 to go one octave up
	)
)

(define (ANS::IntervalCalcDown ansNote interval)
	(define result (ANS::IntervalCalcPrototype -1 ansNote interval))
	(define octave (ANS::GetOctave ansNote))	
	(if (ANS::<= result (ANS::GetNote ansNote)) 
		 (ANS::+ octave (ANS::* result "10")) ;its still in the same octave, just recalculate
		 (ANS::- (ANS::+ octave (ANS::* result "10")) "100") ;-100 to go one octave down		 
	)
)

(define (ANS::CalculateRealOctaveUp ansNote) ; Works with one string
	(ANS::+ ansNote "100")
)

(define (ANS::CalculateRealOctaveDown ansNote) ; Works with one string
	(ANS::- ansNote "100")
)


;Make diatonic. Looks ups the prevailing keysignature and returns the correct diatonic value for a given note.
(define (ANS::GetDiatonic ansNote)
;GetDiatonic. Looks ups the prevailing keysignature and returns the correct diatonic value for a given note.
	(define keysiglist (string-tokenize (d-GetPrevailingKeysig))) ; A list of strings! We need numbers, later
	(define whitekey (ANS::GetWhiteKey ansNote))
	(define getkeysigfor "0")
	
	(set! getkeysigfor  	; the octaveless pitch of the white key triggers sending the connected keysig modicator (0, 1, -1)
	   (cond 
		((string=? "2" (ANS::GetNote whitekey)) (list-ref keysiglist 0 ))
		((string=? "7" (ANS::GetNote whitekey)) (list-ref keysiglist 1))
		((string=? "c" (ANS::GetNote whitekey)) (list-ref keysiglist 2))
		((string=? "h" (ANS::GetNote whitekey)) (list-ref keysiglist 3))
		((string=? "m" (ANS::GetNote whitekey)) (list-ref keysiglist 4))
		((string=? "r" (ANS::GetNote whitekey)) (list-ref keysiglist 5))
		((string=? "w" (ANS::GetNote whitekey)) (list-ref keysiglist 6))
	     )	
	  ) 
       (ANS::Alteration whitekey (string->number getkeysigfor))  ; keysiglist-members are strings so we need to convert first
)

(define (ANS::CalculateDiatonicStepUp ansNote) (ANS::GetDiatonic (ANS::+ ansNote "50")))
(define (ANS::CalculateDiatonicStepDown ansNote ) (ANS::GetDiatonic (ANS::- ansNote "50")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;Random Note Generation;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;TODO: no chords possible yet. It shouldn't matter if you are dealing with chords or single notes.
               
; Generates a random note within a given range. The range includes both values.
(define (ANS::random from to)
	(let (
		  (from (ANS::to10 (ANS::/ from "10"))) ;drop microtones, just use the chromatic/enharmonic material
		  (to (ANS::to10 (ANS::/ to "10")))
		  (rand "0")
		 )
	(set! to (+ 1 (- to from))) ; (- to from ) means only use the relative range, not absolute values . +1 to include the last, given value. 
	(set! rand  (+ from (random to)) ) ; get a random value in the range and then shift the relative range to start from "from"
	(set! rand (ANS::to35 rand)) ; convert back to base35
	(ANS::* rand "10") ; convert to three-digit value again and return	
	)
)


;Random note generator, respects the keysignature. Insert an optional range, default is all 56 diatonic notes.
(define* (ANS::RandomDiatonic #:optional (from "0") (to "8y0"))
	(ANS::GetDiatonic (ANS::random from to))	
)

;Random note generator, one of each possible chromatic notes or optional range. Same probability for natural, flat or sharp.
(define* (ANS::RandomChromatic #:optional (from "0") (to "8y0"))
	(define rand (- (random 3) 1))	; -1, 0 or 1
	(ANS::Alteration (ANS::GetDiatonic from to) rand) 
)  


;Takes a list of notes, shuffles the members and inserts them as new notes.
(define (ANS::InsertListRandomly ansList)
	(define shuffledlist (merge-shuffle-list ansList))
	(for-each ANS::InsertNotes shuffledlist)
)

;Takes a list of notes and randomly pick one to insert. The member
;remains in the list but the function returns a new list without the
;inserted value. 
(define (ANS::InsertMemberRandomly ansList)
	(define rnd (random (length ansList) ))
	(define ANSListcopy (list-copy ansList))
	(ANS::InsertNotes (list (list-ref ansList rnd) ))
	(if (= rnd 0) ;delete1! cannot delete first item
	   (list-tail ANSListcopy 1)
	   (delete1! (list-ref ANSListcopy rnd) ANSListcopy)	
	)
)

;; Example to enter random triads. Uses nearly the complete featureset of ANS as of today. Creating random notes in a range, making it diatonic and then calculate intervals to add up and down and finally placing all as real Denemo notation, as chord at once.
;(define zz (ANS::RandomDiatonic "420" "520" ))     (ANS::InsertNotes  (list zz (ANS::IntervalCalcUp zz 'p5) (ANS::GetDiatonic (ANS::IntervalCalcUp zz 'M3)) ) )
