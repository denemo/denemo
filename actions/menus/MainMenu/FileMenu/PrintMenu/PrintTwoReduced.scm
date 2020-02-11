;;;PrintTwoReduced New version                 
(let ((id #f)(tag "Share Staff" )(stems-swapped #f)(upstart 0)(downstart 0)(up 0)(down 0)(o1 1)(o2 1) (m 1) (bar 1)(s1 1)(s2 2)) 
    
    (define (swap-stems)
        (define (apply-directive)
            
            (d-DirectivePut-chord-prefix tag (if stems-swapped "\\voiceOne " "\\voiceTwo "))
            (d-DirectivePut-chord-postfix tag "\\noBeam ")
            (d-DirectivePut-chord-display tag (_ "Share Staff"))
            (d-DirectivePut-chord-allow tag id)
            (d-DirectivePut-chord-override tag (logior DENEMO_ALT_OVERRIDE DENEMO_OVERRIDE_AFFIX)))
        (d-GoToPosition m s1 bar o1)
        (apply-directive)
        (set! stems-swapped (not stems-swapped))
        (d-GoToPosition m s2 bar o2)
        (apply-directive))
        
        
                
     (define (find-upper-note)
            (let loop () (disp "fu: bar:" bar " o1:" o1 " o2:" o2 " upstart:" upstart " downstart:" downstart)
                (if (d-GoToPosition m s1 bar o1)
                    (if (or (not (Note?)) (> downstart (d-GetStartTick)))
                        (begin
                            (set! o1 (1+ o1))
                            (loop))
                        (begin
                            (set! up (d-GetNoteAsMidi))
                            (set! upstart (d-GetStartTick))))
                    (begin
                        (set! upstart #f)
                        #f))))
    (define (find-lower-note)
        (let loop () (disp "fl: bar:" bar " o1:" o1 " o2:" o2  " upstart:" upstart " downstart:" downstart)
            (if (d-GoToPosition m s2 bar o2)
                (if (or (not (Note?)) (> upstart (d-GetStartTick)))
                    (begin
                        (set! o2 (1+ o2))
                        (loop))
                    (begin
                            (set! down (d-GetNoteAsMidi))
                            (set! downstart (d-GetStartTick))))
                    (begin
                        (set! downstart #f)
                        #f))))
    (define (find-notes)
        (if (and upstart downstart)
            (if (> upstart downstart)
                (find-lower-note)
                (find-upper-note))
            #f))
    (define (check-pitches)
        (if (d-GoToPosition m s2 bar o2)
            (begin
                (set! down (d-GetNoteAsMidi))
                (if (d-GoToPosition m s1 bar o1)
                    (begin
                        (set! up (d-GetNoteAsMidi))
                        (if (or (and (> up down) stems-swapped)
                                (and (> down up)(not stems-swapped)))
                            (swap-stems))
                        (disp "\n\nSuccess: bar:" bar " o1:" o1 " o2:" o2 "\n") 
                        (disp "Up pitch:" up " Down pitch:" down)
                        #t)))))
        
    (define (do-bar)
        (disp "upstart:" upstart " downstart:" downstart)
        (while (and (find-notes) upstart downstart (not (= upstart downstart))))
        (disp "find-notes " upstart " vs " downstart)
        (if (and upstart downstart)
                    (begin
                        (check-pitches)
                        (set! o1 (1+ o1))
                        (set! o2 (1+ o2))
                        (do-bar))
                    (let loop () 
                        (set! bar (1+ bar))
                        
                        (if (<= bar (d-GetMeasuresInStaff))
                            (begin
                                (disp "Onward to bar " bar "\n")
                                (set! o1 1)
                                (set! o2 1)
                                (set! upstart 0)
                                (set! downstart 0)
                                (find-upper-note)
                                    (if upstart
                                        (begin
                                            (find-lower-note)
                                            (if downstart
                                                (check-pitches)
                                                (loop)))
                                        (loop)))
                            #f))))
;;;;
(d-CreateLayout tag)  
(set! id (d-GetLayoutId))
(d-DeleteLayout tag)

(while (<= m (d-GetMovementsInScore))
    (set! s1 1)
    (set! s2 2)
    (set! bar 1)
   
    (set! o1 1)
    (set! o2 1)
    
    (d-GoToPosition m s1 bar o1)
    (d-SmallerStaff) ;; FIXME make conditional
    (d-DirectivePut-voice-postfix tag "\\voiceOne") 
    (d-DirectivePut-voice-allow tag id)
    
    (d-DirectivePut-standalone tag)
    (d-DirectivePut-standalone-minpixels tag 5)
    (d-DirectivePut-standalone-postfix tag "\\override Voice.Slur.stencil = ##f                   
                                    \\override Voice.Tie.stencil = ##f ")
    (d-DirectivePut-standalone-allow tag id) (disp "set 1 allow " id " on " tag "\n")
    (d-GoToPosition m s2 bar o2)
    (d-SmallerStaff)
    (d-DirectivePut-voice-postfix tag "\\voiceTwo") 
    (d-DirectivePut-voice-allow tag id)
    
    
    (d-SetCurrentStaffAsVoice)
    (d-DirectivePut-standalone tag)
    (d-DirectivePut-standalone-minpixels tag 5)
    (d-DirectivePut-standalone-postfix tag "\\override Voice.Slur.stencil = ##f                   
                                    \\override Voice.Tie.stencil = ##f ")
    (d-DirectivePut-standalone-allow tag id) (disp "set 2 allow " id " on " tag "\n")
    (while (and (do-bar) (<= bar (d-GetMeasuresInStaff))))
    (set! m (1+ m)))
(d-CreateLayout tag)  
(d-SelectDefaultLayout)
(set! m 1) 
(while (<= m (d-GetMovementsInScore))
    (d-GoToPosition m s1 1 1)
    (d-SmallerStaff)
    (d-GoToPosition m s2 1 1) 
    (disp "Restoring voice as staff in movement " m "\n")
    (d-SetCurrentVoiceAsStaff)
    (d-SmallerStaff)
    (set! m (1+ m)))
(d-GoToPosition 1 1 1 1)
(d-SelectLayoutId id)
(d-SetSaved #f))