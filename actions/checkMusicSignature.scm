;;;This script requires a Scheme variable DenemoMusicSignature to be set to a list of semitone steps
;;;It will test the current denemo score to see if it matches at the opening bars
(d-GoToPosition 1 1 1 1) ; this could check for the opening bars of each movement by looping with d-NextMovement from here
(disp "\n 𝅘𝅥   Checking score " (d-GetFilename))
(let ((current (d-GetNoteFromTopAsMidi))
        (test #f)
        (next #f))
    (if (not current)
        (d-NextNote))
    (set! current (d-GetNoteFromTopAsMidi))

    (while (and (d-NextNote) (not (null? DenemoMusicSignature)))
        (set! next (d-GetNoteFromTopAsMidi))
        (set! test (- next current))
        (set! current next)
        (if (= test (car DenemoMusicSignature))
            (begin  
                (set! DenemoMusicSignature (cdr DenemoMusicSignature)))
            (begin
                ;(disp "No match at current" DenemoMusicSignature " test " test "\n\n")
                (d-Quit))))
    (if (null? DenemoMusicSignature)
        (disp  "\n𝅘𝅥   The file " (d-GetFilename) " matches the given Denemo Music Signature\n"))
    (d-Quit))
            
                
        
