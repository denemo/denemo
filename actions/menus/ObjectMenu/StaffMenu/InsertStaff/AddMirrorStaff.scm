;;AddMirrorStaff
(let ((params AddMirrorStaff::params))
    (if (not params)
        (set! params 'staff))
    (d-AddMirrorVoice params)
    (if (number? params)
        (d-SetCurrentVoiceAsStaff)))
