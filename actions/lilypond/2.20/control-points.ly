\version "2.20.0"

#(define (make-cross-stencil coords)
 (ly:stencil-add
   (make-line-stencil 0.1 (- (car coords) 0.2) (- (cdr coords) 0.2)
  (+ (car coords) 0.2) (+ (cdr coords) 0.2))
   (make-line-stencil 0.1 (- (car coords) 0.2) (+ (cdr coords) 0.2)
  (+ (car coords) 0.2) (- (cdr coords) 0.2))))

#(define (display-control-points line)
 (lambda (grob)
   (let* ((grob-name (lambda (x) (assq-ref (ly:grob-property x 'meta) 'name)))
          (name (grob-name grob))
          (stil (cond ((or (eq? name 'Slur)(eq? name
'PhrasingSlur))(ly:slur::print grob))
                      ((eq? name 'Tie)(ly:tie::print grob))))
          (cps (ly:grob-property grob 'control-points)))

  (ly:stencil-add stil
    (ly:stencil-in-color
      (ly:stencil-add
         (make-cross-stencil (first cps))
         (make-cross-stencil (second cps))
         (make-cross-stencil (third cps))
         (make-cross-stencil (fourth cps))
         )
         1 0 0)

     (if (eq? line #t)
         (begin
          (ly:stencil-add
           (make-line-stencil 0.05 (car (first cps)) (cdr (first cps))
(car (second cps))  (cdr (second cps)))
           (make-line-stencil 0.05 (car (second cps)) (cdr (second
cps)) (car (third cps))  (cdr (third cps)))
           (make-line-stencil 0.05 (car (third cps)) (cdr (third cps))
(car (fourth cps))  (cdr (fourth cps)))
           ))
         empty-stencil)
     )
     )))
