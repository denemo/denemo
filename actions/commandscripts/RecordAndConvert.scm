;;; Warning!!! This file is derived from those in actions/menus/... do not edit here
;;;RecordAndConvert
(define (DenemoPlay)

  (d-Play "
  (let ()
  (define response #f)
  (d-PopPosition)
  
  (set! response (d-GetUserInput\"Recording Finished\" \"Convert to notation?\" \"y\"))
  
  (if (equal? response \"y\")
 	 (d-ConvertMidiForBass))
  
  )
  (d-TogglePlayAlong)
  (define (DenemoPlay) (DefaultDenemoPlay))
  "))
  (d-CreateIntro)
  (d-PushPosition)
  (if (not (d-TogglePlayAlong))
  	 (d-TogglePlayAlong))	
  (d-MidiRecord)
