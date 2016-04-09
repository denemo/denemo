;;AddMirrorVoice
(let ((params AddMirrorVoice::params)(name (d-StaffProperties "query=denemo_name")))
    (d-AddAfter)
    (d-StaffProperties (string-append "denemo_name=" name) )
    (if (not params)
        (d-SetCurrentStaffAsVoice))
    (d-SubstituteMusic)
    (d-RefreshDisplay))
