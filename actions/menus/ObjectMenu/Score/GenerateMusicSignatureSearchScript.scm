;;;GenerateMusicSignatureSearchScript
(use-modules (ice-9 ftw))
(d-GoToPosition 1 1 1 1)
(let ((current (d-GetNoteFromTopAsMidi))
        (count (string->number (d-GetUserInput "Create Music Signature" "Give number of notes to match: " "5")))
        (startdir #f)
        (theproc #f)
        (sig "")
        (test #f)
        (found #f)
        (next #f))
        
      (define (theproc filename statinfo flag) ;(disp "searching file " filename "\nwith flag " flag "\n")
        (d-KeepAlive)
	    (if (and (eq? flag 'regular) (or (string-suffix? ".denemo" filename) (string-suffix? ".denemo.gz" filename)));;; also check for .denemo or .denemo.gz in filename
            (let ((cmd (string-append DENEMO_BIN_DIR  "/denemo -n -a \"(define DenemoMusicSignature '(" sig "))\" -i "
		                DENEMO_ACTIONS_DIR 
		                "/checkMusicSignature.scm " 
		                "\"" filename "\""))
                    (status 0))
                    (disp "issuing " cmd "\n\n")
                (set! status  (system* 
                    (string-append DENEMO_BIN_DIR  "/denemo")
                    "-n" 
                    "-a" (string-append "(define DenemoMusicSignature '(" sig "))")
                    "-i" (string-append DENEMO_ACTIONS_DIR "checkMusicSignature.scm") 
                    filename))

                (disp "returned " status "\n")
                (if (zero? status)
                    (begin
                        (d-OpenNewWindow filename)
                        (set! found #t)
                        #f)
                    (begin
                        
                        #t))) ;not a match, continue traversal
            (begin
          
            #t))) ;not a file, continue traversal
;;;;actual procedure        
    (if (not current)
        (d-NextNote))
    (set! current (d-GetNoteFromTopAsMidi))
    (while (and (d-NextNote) (> count 0)) 
        (set! next (d-GetNoteFromTopAsMidi))
        (set! test (- next current))
        (set! sig (string-append sig " " (number->string test)))
        (set! current next)
        (set! count (1- count)))
  (set! startdir (d-ChooseDirectory "Where to search" DENEMO_HOME_DIR '() ))
  (d-InfoDialog "Searching ... the display will be very sluggish!")
  (ftw startdir theproc)
  (if found
    (d-InfoDialog "This score starts with the same intervals")
    (d-InfoDialog (string-append "No score in or below " startdir " starts with the same intervals"))))
