;;;This script requires a Scheme variable DenemoMusicSignature to be set to a list of semitone steps
;;;It will test the current denemo score to see if it matches at the opening bars
(d-GoToPosition DenemoSearchMovement 1 1 1) ; this could check for the opening bars of each movement by looping with d-NextMovement from here
(disp "\nChecking score " (d-GetFilename))
(let ((current (d-GetNoteFromTopAsMidi))
        (sig DenemoMusicSignature)
        (test #f)
        (next #f))
    (if (not current)
        (d-NextNote))
    (set! current (d-GetNoteFromTopAsMidi))
(disp "starting with current " current "\n\n\n\n")
(if current
    (begin
        (while (and (d-NextNote) (not (null? sig)))
            (set! next (d-GetNoteFromTopAsMidi))
            (set! test (- next current))
            (set! current next)
            (if (= test (car sig))
                (begin  
                    (set! sig (cdr sig)))
                (begin
                    (disp "No match at current" DenemoMusicSignature " test " test "\n\n")
                    (d-Quit "1"))))
        (if (null? sig)
            (begin
                (disp  "\nThe file " (d-GetFilename) " matches the given Denemo Music Signature\n")
                (d-Quit "0"))))
    (begin
        (disp "Returning status 2 (target score has too few notes)\n")
        (d-Quit "2"))))
(d-Quit "3") ;;just one note in target
            
                
        
