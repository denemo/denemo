;;AddMirrorVoice
(let ((params AddMirrorVoice::params)(name (d-StaffProperties "query=denemo_name")))
    (d-AddAfter)
    (d-StaffProperties (string-append "denemo_name=" name) )
    (if (not (eq? params 'staff))
        (d-SetCurrentStaffAsVoice))
    (d-SubstituteMusic (if (number? params) params #f))
    (d-RefreshDisplay))

