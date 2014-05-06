;;;;CouleStaff
    (let ((tag "CouleStaff"))
      (if (d-Directive-standalone? tag)
        (d-InfoDialog (_ "This staff creates notes drawn as a diagonal line. Use these to create a coule between two notes another part."))
        (begin
            (d-SetSaved #f)
            (d-AddAfter)
            (d-SetCurrentStaffAsVoice)
            (d-StaffProperties "denemo_name=Coulés")
            (d-DirectivePut-standalone-postfix tag " \\override NoteHead.stencil = #(ly:make-stencil
       (list 'draw-line 0.15 -0.5 -0.4 2 0.4)
       '(-0.1 . 0.1) '(0.1 . 1))
     \\override Stem.stencil = ##f
     \\override Flag.stencil = ##f")
     (d-DirectivePut-standalone-minpixels 50)
            (d-DirectivePut-standalone-display tag "coulé"))))