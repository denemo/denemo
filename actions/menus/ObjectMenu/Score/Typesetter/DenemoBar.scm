;;;DenemoBar
(let ((tag "DenemoBar"))
        (if (d-Directive-score? tag)
                    (begin
                        (d-InfoDialog (_ "Automatic barlines and beaming rules re-instated"))
                        (d-DirectiveDelete-score tag)
                        (d-DirectiveDelete-score "ScoreTiming")) 
                    (begin
                        (d-InfoDialog (_ "Literal Barlines now ON. Execute the command a second time to turn it off. This command is for un-metered music. You will have to manually beam the music. Do not use this for an upbeat (pickup, anacrusis), there is a proper command for that."))
                        (d-DirectivePut-score-prefix tag "\n
increaseBarNumber = \\applyContext
#(lambda (x)
  (let ((measurepos (ly:context-property x 'measurePosition)))
   ; Only increase bar number if not at start of measure.
   ; This way we ensure that you won't increase bar number twice
   ; if two parallel voices call increaseBarNumber simultanously:
   (if (< 0 (ly:moment-main-numerator measurepos)) ; ugh. ignore grace part
    (begin
     (ly:context-set-property!
      (ly:context-property-where-defined x 'internalBarNumber)
      'internalBarNumber
      (1+ (ly:context-property x 'internalBarNumber)))
     (ly:context-set-property!
      (ly:context-property-where-defined x 'currentBarNumber)
      'currentBarNumber
      (1+ (ly:context-property x 'currentBarNumber)))
     ; set main part of measurepos to zero, leave grace part as it is:
     (ly:context-set-property!
      (ly:context-property-where-defined x 'measurePosition)
      'measurePosition
      (ly:make-moment 0 1
       (ly:moment-grace-numerator measurepos)
       (ly:moment-grace-denominator measurepos)))))))

% Named Increasing BAR
nibar = #(define-music-function (parser location x) (string?)
#{
  \\bar $x
  \\increaseBarNumber
#})
                    AutoBarline =   \\nibar \"|\"\n")
                    (d-DirectivePut-score-override tag DENEMO_OVERRIDE_AFFIX)
                    (d-DirectivePut-score-postfix "ScoreTiming" " \\set Score.timing = ##f \n")))
        (d-SetSaved #f))
