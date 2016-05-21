;;;;Tongue
(d-LilyPondDefinition (cons "tongue"
 "
 #(define-music-function (parser location dots) (integer?)
   (let ((script (make-music 'ArticulationEvent
                             'articulation-type \"staccato\")))
     (set! (ly:music-property script 'tweaks)
           (acons 'stencil
                  (lambda (grob)
                    (let ((stil (ly:script-interface::print grob)))
                      (let loop ((count (1- dots)) (new-stil stil))
                        (if (> count 0)
                            (loop (1- count)
                                  (ly:stencil-combine-at-edge new-stil X RIGHT stil 0.2))
                            (ly:stencil-aligned-to new-stil X CENTER)))))
                  (ly:music-property script 'tweaks)))
     script))
"))
(let ((tag "Tongue"))
    (if (d-Directive-chord? tag)
        (begin
            (d-DirectiveDelete-chord tag)
            (d-InfoDialog (_ "Deleted")))
        (let ((repeats (d-GetUserInput (_ "Tonguing Indication") (_ "Give number of dots required: ") "3")))
            (if (and repeats (string->number repeats))
                (let ((number (string->number repeats)))
                    (if (and (positive? number) (< number 10))
                        (let ((text (make-string number #\.)))
                            (d-DirectivePut-chord-prefix tag (string-append "-\\tongue #" repeats " "))
                            (d-DirectivePut-chord-display tag text))
                            (d-InfoDialog (_ "Cancelled"))))
                (d-InfoDialog (_ "Cancelled")))))
    (d-SetSaved #f))
