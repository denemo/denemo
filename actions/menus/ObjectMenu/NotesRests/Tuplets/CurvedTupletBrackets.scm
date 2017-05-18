;;;;CurvedTupletBrackets
(d-LilyPondDefinition (cons "tupletBracketToSlur"
 "{
  % Use slur-stencil
  \\override TupletBracket.stencil = #ly:slur::print
  %% Use 'thickness from Slur
  \\override TupletBracket.thickness = #1.2
  %% 'control-points need to be set
  \\override TupletBracket.control-points =
    #(lambda (grob)
      (let* ((x-pos (ly:grob-property grob 'X-positions))
             (pos (ly:grob-property grob 'positions))
             (x-ln (interval-length x-pos))
             (dir (ly:grob-property grob 'direction))
             ;; read out the height of the TupletBracket, maybe negative!
             (height (- (cdr pos) (car pos)))
             ;; height-corr is introduced because sometimes the shape of the
             ;; slur needs to be adjusted.
             ;; It is used in the 2nd/3rd control-point.
             ;; The value of 0.3 is found by trial and error
             (height-corr (* 0.3 dir height))
             (edge-height (ly:grob-property grob 'edge-height '(0.7 . 0.7)))
             (pad 1.0))
        (list
          ;; first cp
          (cons
            (+ (car x-pos) 0.5)
            (- (+ (* dir pad) (+ (car pos) (* -1 dir (car edge-height))))
              (if (= dir -1)
                  (if (> height 3)
                    (/ dir 2.0)
                    0.0)
                  (if (< height -3)
                    (/ dir 2.0)
                    0.0))))
          ;; second cp
          (cons
            (+ (car x-pos) (* x-ln 1/4))
            (+ (* dir pad) (+ (car pos) (* dir (+ 0.5 height-corr)))))
          ;; third cp
          (cons
            (+ (car x-pos) (* x-ln 3/4))
            (+ (* dir pad) (+ (cdr pos) (* dir (- 0.5 height-corr)))))
          ;; fourth cp
          (cons
            (- (cdr x-pos) 0.5)
            (+ (* dir pad) (+ (cdr pos) (* -1 dir (cdr edge-height)))))
        )))
  \\override TupletBracket.staff-padding = #'()
  #(define (invert-direction x) (if (eq? UP (ly:tuplet-bracket::calc-direction x)) DOWN UP))
\\override TupletBracket.direction = #invert-direction
}"))
(let ((tag "CurvedTupletBrackets"))
    (if (d-Directive-score? tag)
        (begin
            (d-DirectiveDelete-score tag)
            (d-InfoDialog (_ "Square Tuplet Brackets re-instated")))
        (begin
            (d-DirectivePut-score-prefix tag "\n\\layout { \\tupletBracketToSlur }\n")
            (d-InfoDialog (_ "Tuplet Brackets Curved"))))
    (d-SetSaved #f))
