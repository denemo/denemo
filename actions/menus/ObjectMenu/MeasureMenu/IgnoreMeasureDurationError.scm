;;;IgnoreMeasureDurationError
(if IgnoreMeasureDurationError::params
    (d-InfoDialog (_ "This Directive tells Denemo not to signal an error if there are too many notes or rests after this point (or too few) for a complete measure."))
    (let ((tag "!"))
        (d-PushPosition)
        (while (d-PrevObjectInMeasure))
        (if (d-Directive-standalone? tag)
            (d-DirectiveDelete-standalone tag)
            (begin
            	(d-Directive-standalone tag)
            	(d-DirectivePut-standalone-postfix tag (string-append "%{" (_ "Partial Measure is Acceptable") "%}"))
            	(d-DirectivePut-standalone-display tag (_ "Partial Measure"))
            	(d-DirectivePut-standalone-minpixels tag 30)))
        (d-PopPosition)
        (d-SetSaved #f)
        (d-RefreshDisplay)))
