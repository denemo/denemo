;MetronomeMark
;;Tempo-script by DW tweaked by RTS
(let ( (tag "MetronomeMark")
		(TempoChoice #f) (replace #f) (input "")  (AboveBelow? "^")
		(MidiBPM 60) (ShowBPM? #f)(ValidBPM? #f) (BPMString "") (Go? #t)
		(LilyString #f) (TempoAdjust #f)(DisplayString "") (InQuotes "")(duration "4") (bpm 60)  (OldLily  (not (d-CheckLilyVersion "2.12.0")))
	)

(define (GetBPM BaseBeat )  	;sets MidiBPM, ValidBPM?, BPMString
	(let ((dotted #f)  (MetronomeMarkVisible #f)(defaultBPM "60") (len 1))
		(set! defaultBPM  
			(cond 
				( (equal? TempoChoice "Presto" ) "168" )
				( (equal? TempoChoice "Vivace" ) "140" )
				( (equal? TempoChoice "Allegro" ) "120" )
				( (equal? TempoChoice "Moderato" ) "108" )
				( (equal? TempoChoice "Andante" ) "84" )
				( (equal? TempoChoice "Adagio" ) "72" )
				( (equal? TempoChoice "Largo" ) "60" )
				( (equal? TempoChoice "Lento" ) "40" )
				(else "60" )
			)
		)
		(if (equal? "" BaseBeat)  (set! BaseBeat (d-GetUserInput (_ "Metronome Marking") (_ "Give unit beat duration (e.g., 4. for dotted-quarter) \n or enter n for none:") "4" )))
		
		(if (or (equal? BaseBeat (_ "n") ) (equal? #f BaseBeat))
			(begin
				(set! ValidBPM? #f)	;if user doesn't want to use BPM.
				(if (not BaseBeat) (set! Go? #f))
			)
			(begin 				; if user wants to use bpm...
				(set! len (string-length BaseBeat) ) 
				(set! dotted (equal? "." (substring BaseBeat (- len 1) len ))  ) ;see if a dot at end
				(if dotted
					(set! duration (substring BaseBeat 0 (- len 1)))  ;if there's a dot, cut it off from BaseBeat to get base duration.
					(set! duration BaseBeat)  
				)
				(if replace (set! defaultBPM (d-DirectiveGet-standalone-midibytes tag ))) ;use old BPM if there. Bug: Only good for 4=...   
				
				(set! bpm (d-GetUserInput (_ "Metronome Marking")  (string-append (_ "Give number of these ") BaseBeat (_ " beats per minute:")) defaultBPM ) )
				
				
				(set! ValidBPM?   (not (equal? (and (string->number duration) (string->number bpm) ) #f)))  ;don't go unless both are numbers.
				;don't go unless base duration is valid lilypond: (could go higher if wanted):
				(set! ValidBPM? (and ValidBPM? (or   (equal? duration "1")(equal? duration "2")(equal? duration "4")(equal? duration "8")
					(equal? duration "16")) ) )
				(if (and bpm  (equal? ValidBPM? #t))
					(begin
						(if dotted (set! MidiBPM (number->string (floor (* (/ (string->number bpm) (string->number duration)) 6 ) ) ) ) 
							(set! MidiBPM (number->string (floor (* (/ (string->number bpm) (string->number duration) ) 4))  ) )
						);want * 3/2 for dotted,*4 since midi uses quarters and divide by duration,     
						(if (not ShowBPM? ) (set! ShowBPM?  (d-GetOption (string-append (_ "BPM Printed") stop (_ "BPM Not Printed") stop))))
						
						(if (not ShowBPM?) (set! Go? #f))
						(set! ShowBPM? (equal? ShowBPM? (_ "BPM Printed")))

						(set! BPMString (string-append  BaseBeat "=" bpm ))

					) ;begin
					(d-WarningDialog (_ "Incorrect BPM syntax."))
				)
			)
		)
	);let
);define GetBPM


;procedure begins here.

;see if there's already one there:
(if (d-Directive-standalone?  tag)
	;if we want to REPLACE an existing directive...
	(let ((choice #f))		
		(set! replace 'edit)
		(set! choice (d-GetOption  (string-append "Change"stop"Delete" stop cue-Advanced stop)))
		(cond
			;((boolean? choice)
			;	(d-WarningDialog "Operation cancelled"))
			((equal? choice "Change")
				(set! replace #t ))
			((equal? choice  cue-Advanced)
				(if (not (d-DirectiveTextEdit-standalone tag))
          (d-DirectiveDelete-standalone tag)))
			((equal? choice "Delete")
				(d-DirectiveDelete-standalone tag))
		)
	)
)

(if (boolean? replace)	;as long as we're not just editing an existing directive, we continue from here.
	(begin	
		;get tempo text from user:
		(set! TempoChoice (d-GetOption (string-append "Presto" stop "Vivace" stop "Allegro" stop 
			"Moderato" stop "Andante" stop "Adagio" stop "Largo" stop  "Lento" stop "Tempo Adjust (ritardando, etc.)" stop 
			"Custom tempo (e.g., Allegro assai)" stop "Beat Change (e.g., 4=4.)" stop "No Tempo Text-Metronome Only" stop)))
		(if (not TempoChoice) (set! Go? #f))

		(if (equal? TempoChoice "No Tempo Text-Metronome Only") (set! TempoChoice #f ) )
		(if (equal? TempoChoice "Custom tempo (e.g., Allegro assai)") 	;read input if user wants custom:
			(begin 
				(set! InQuotes "\"")
				(set! TempoChoice (d-GetUserInput "Tempo setting" "Enter tempo text:" "Allegro assai" ) )
				(if (not TempoChoice) (set! Go? #f))
			)
		)
		(if (equal? TempoChoice  "Tempo Adjust (ritardando, etc.)") 	;for custom italic tempo adjustments
			(begin
				(set! TempoAdjust #t)
				(set! InQuotes "\"")
				(set! TempoChoice (d-GetUserInput "Tempo adjust" "Enter text:" "rit." ))
				(if (not TempoChoice) (set! Go? #f))
				(if Go? (set! AboveBelow? (d-GetOption (string-append "Neutral" stop "Above" stop  "Below" stop ))))
				(if (not AboveBelow?) (set! Go? #f))
				(set! AboveBelow? 
					(cond
						((equal? AboveBelow? "Above") "^" )
						((equal? AboveBelow? "Below") "_" )
						(else "-")
					)
				)
			)
		)
		(if (equal? TempoChoice "Beat Change (e.g., 4=4.)" ) 
			(begin
				(set! Go? #f)
				(d-BeatChange)
			)
		)
		(if (equal? TempoChoice "") (set! TempoChoice #f))
		
		;TempoChoice should now have the desired text to be shown, or #f is nothing is to be shown.
		;TempoAdjust = #t if italics should be used.
		
		(if Go? (GetBPM "" ))	;find out what bpm the user wants and whether to print it(but not if user cancelled)
		
		; now set DisplayString-what denemo shows.
		
		(if ShowBPM?  
			(begin   ;if we print the BPM:
				(if TempoChoice 			
					(set! DisplayString (string-append TempoChoice "(" BPMString ")" ))
					(set! DisplayString BPMString)
		
				)
			)
			;if NOT showing BPM...
			(if ValidBPM? 
				(set! DisplayString (string-append (if TempoChoice TempoChoice "") "[" BPMString "]" )) 
				(set! DisplayString TempoChoice)
			)
		)
		
		;now need to set LilyString...
		(if TempoChoice (set! TempoChoice (string-append InQuotes TempoChoice InQuotes) )) ;if more than 1 word, may need quotes.
		
		(if TempoAdjust 
			(begin		;if we want to REPLACE an existing tempo directive...
				(if TempoChoice (set! LilyString (string-append "s8*0" AboveBelow? "\\markup { \\italic "  TempoChoice "} " )))
				(if ValidBPM? (set! LilyString (string-append LilyString "\\tempo " BPMString " ")))		
			) 	;if TempoAdjust=#t.
			(begin		;when TempoAdjust = #f...
				(if ShowBPM?
					(if OldLily
						;if using OldLily syntax....
						(if TempoChoice
							(set! LilyString  
								(string-append  "s8*0" AboveBelow? "\\markup \\bold { " TempoChoice " (\\smaller \\general-align #Y #DOWN"
								" \\note #\"" duration  "\" #.75 = " bpm  ")} \\once \\override Score.MetronomeMark #'transparent = ##t \\tempo "  BPMString " ")
							)
							(set! LilyString  (string-append "\\tempo "  BPMString " "))
						)			
						
						;if we're NOT using OldLily syntax:
						(set! LilyString (string-append "\\tempo " (if TempoChoice (string-append TempoChoice " ") "") BPMString " "))
					)
					;if we're NOT showing BPM...
					(if OldLily
						(set! LilyString (if TempoChoice (string-append " s8*0" AboveBelow? "\\markup \\bold { " TempoChoice " } " )))
						(if TempoChoice (set! LilyString (string-append "\\tempo " TempoChoice " ")))
					)
				)
			)
		)
		
		;now make the directive...
		(if Go?
			(begin
				(if (not replace) (d-DirectivePut-standalone tag ) )
				(if LilyString (d-DirectivePut-standalone-postfix tag LilyString ) )
				(d-DirectivePut-standalone-grob  tag "MetronomeMark")
				(d-DirectivePut-standalone-graphic  tag (string-append "\n" DisplayString "\nDenemo\n12"))
				(d-DirectivePut-standalone-gy tag -25)
				(d-DirectivePut-standalone-minpixels tag 10 )
				(d-DirectivePut-standalone-ty tag 85 ) ;;try -40 instead of 85 for above-the-staff
				(if (equal? ValidBPM? #t)
					(begin 
						(d-DirectivePut-standalone-override tag (logior DENEMO_OVERRIDE_TAGEDIT DENEMO_OVERRIDE_TEMPO DENEMO_OVERRIDE_STEP))
						(d-DirectivePut-standalone-midibytes tag MidiBPM)
					)
				)
				(d-SetSaved #f)
				(d-RefreshDisplay)
				(d-ChooseCondition) ;;; this did not fire using d-PopUpMenu so changed to RadioBoxMenu
				(d-MoveCursorRight))))))