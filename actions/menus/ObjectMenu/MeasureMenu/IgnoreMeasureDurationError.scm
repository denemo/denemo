;;;IgnoreMeasureDurationError
(if IgnoreMeasureDurationError::params
    (d-InfoDialog (_ "This object tells Denemo not to signal an error when the current measure is incomplete or overfull"))
    (let ((tag "!"))
        (if (d-Directive-standalone? tag)
            (d-DirectiveDelete-standalone tag)
            (d-DirectivePut-standalone-minpixels tag 10))))
