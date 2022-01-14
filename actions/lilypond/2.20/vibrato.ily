%\version "2.22.0"

%% https://raw.githubusercontent.com/mwitmer/LyUtil/master/ly/expressive_markings/vibrato.ly
%% Original author: Mark Witmer
%% Rewritten version by Harm
%% and Valentin

#(define (line-part-min-max x1 x2)
  (list (min x1 x2) (max x1 x2)))

#(define (bezier-part-min-max x1 x2 x3 x4)
  ((lambda (x) (list (reduce min 10000 x) (reduce max -10000 x)))
   (map
    (lambda (x)
      (+ (* x1 (expt (- 1 x) 3))
         (+ (* 3 (* x2 (* (expt (- 1 x) 2) x)))
            (+ (* 3 (* x3 (* (- 1 x) (expt x 2))))
               (* x4 (expt x 3))))))
    (if (< (+ (expt x2 2) (+ (expt x3 2) (* x1 x4)))
           (+ (* x1 x3) (+ (* x2 x4) (* x2 x3))))
        (list 0.0 1.0)
        (filter
         (lambda (x) (and (>= x 0) (<= x 1)))
         (append
          (list 0.0 1.0)
          (map (lambda (op)
                 (if (not (eqv? 0.0
                                (exact->inexact (- (+ x1 (* 3 x3)) (+ x4 (* 3 x2))))))
                     ;; Zeros of the bezier curve
                     (/ (+ (- x1 (* 2 x2))
                           (op x3
                               (sqrt (- (+ (expt x2 2)
                                           (+ (expt x3 2) (* x1 x4)))
                                        (+ (* x1 x3)
                                           (+ (* x2 x4) (* x2 x3)))))))
                        (- (+ x1 (* 3 x3)) (+ x4 (* 3 x2))))
                     ;; Apply L'hopital's rule to get the zeros if 0/0
                     (* (op 0 1)
                        (/ (/ (- x4 x3) 2)
                           (sqrt (- (+ (* x2 x2)
                                       (+ (* x3 x3) (* x1 x4)))
                                    (+ (* x1 x3)
                                       (+ (* x2 x4) (* x2 x3)))))))))
               (list + -))))))))
               
#(define (bezier-min-max x1 y1 x2 y2 x3 y3 x4 y4)
  (map (lambda (x)
         (apply bezier-part-min-max x))
       `((,x1 ,x2 ,x3 ,x4) (,y1 ,y2 ,y3 ,y4))))

#(define (line-min-max x1 y1 x2 y2)
  (map (lambda (x)
         (apply line-part-min-max x))
       `((,x1 ,x2) (,y1 ,y2))))

#(define (path-min-max origin pointlist)

  ((lambda (x)
     (list
      (reduce min +inf.0 (map caar x))
      (reduce max -inf.0 (map cadar x))
      (reduce min +inf.0 (map caadr x))
      (reduce max -inf.0 (map cadadr x))))
   (map (lambda (x)
          (if (= (length x) 8)
              (apply bezier-min-max x)
              (apply line-min-max x)))
        (map (lambda (x y)
               (append (list (cadr (reverse x)) (car (reverse x))) y))
             (append (list origin)
                     (reverse (cdr (reverse pointlist)))) pointlist))))
                     
#(define (make-path-stencil path thickness x-scale y-scale fill)
  "Make a stencil based on the path described by the list @var{path},
with thickness @var{thickness}, and scaled by @var{x-scale} in the X
direction and @var{y-scale} in the Y direction.  @var{fill} is a boolean
argument that specifies if the path should be filled.  Valid path
commands are: moveto rmoveto lineto rlineto curveto rcurveto closepath,
and their standard SVG single letter equivalents: M m L l C c Z z."

  (define (convert-path path origin previous-point)
    "Recursive function to standardize command names and
convert any relative path expressions (in @var{path}) to absolute
values.  Returns a list of lists.  @var{origin} is a pair of x and y
coordinates for the origin point of the path (used for closepath and
reset by moveto commands).  @var{previous-point} is a pair of x and y
coordinates for the previous point in the path."
    (if (pair? path)
        (let*
         ((head-raw (car path))
          (rest (cdr path))
          (head (cond
                 ((memq head-raw '(rmoveto M m)) 'moveto)
                 ((memq head-raw '(rlineto L l)) 'lineto)
                 ((memq head-raw '(rcurveto C c)) 'curveto)
                 ((memq head-raw '(Z z)) 'closepath)
                 (else head-raw)))
          (arity (cond
                  ((memq head '(lineto moveto)) 2)
                  ((eq? head 'curveto) 6)
                  (else 0)))
          (coordinates-raw (take rest arity))
          (is-absolute (if (memq head-raw
                           '(rmoveto m rlineto l rcurveto c)) #f #t))
          (coordinates (if is-absolute
                           coordinates-raw
                           ;; convert relative coordinates to absolute by
                           ;; adding them to previous point values
                           (map (lambda (c n)
                                  (if (even? n)
                                      (+ c (car previous-point))
                                      (+ c (cdr previous-point))))
                             coordinates-raw
                             (iota arity))))
          (new-point (if (eq? head 'closepath)
                         origin
                         (cons
                          (list-ref coordinates (- arity 2))
                          (list-ref coordinates (- arity 1)))))
          (new-origin (if (eq? head 'moveto)
                          new-point
                          origin)))
         (cons (cons head coordinates)
           (convert-path (drop rest arity) new-origin new-point)))
        '()))

  (let* ((path-absolute (convert-path path (cons 0 0) (cons 0 0)))
         ;; scale coordinates
         (path-scaled (if (and (= 1 x-scale) (= 1 y-scale))
                          path-absolute
                          (map (lambda (path-unit)
                                 (map (lambda (c n)
                                        (cond
                                         ((= 0 n) c)
                                         ((odd? n) (* c x-scale))
                                         (else (* c y-scale))))
                                   path-unit
                                   (iota (length path-unit))))
                            path-absolute)))
         ;; a path must begin with a 'moveto'
         (path-final (if (eq? 'moveto (car (car path-scaled)))
                         path-scaled
                         (append (list (list 'moveto 0 0)) path-scaled)))
         ;; remove all commands in order to calculate bounds
         (path-headless (map cdr (delete (list 'closepath) path-final)))
         (bound-list (path-min-max
                      (car path-headless)
                      (cdr path-headless))))
    (ly:make-stencil
     `(path ,thickness
        ,(concatenate path-final)
        round
        round
        ,(if fill #t #f))
     (coord-translate
      ((if (< x-scale 0) reverse-interval identity)
       (cons
        (list-ref bound-list 0)
        (list-ref bound-list 1)))
      `(,(/ thickness -2) . ,(/ thickness 2)))
     (coord-translate
      ((if (< y-scale 0) reverse-interval identity)
       (cons
        (list-ref bound-list 2)
        (list-ref bound-list 3)))
      `(,(/ thickness -2) . ,(/ thickness 2))))))
% Returns the width of a grob
#(define (grob-width grob)
  (let ((x-ext (ly:grob-property grob 'X-extent)))
    (if (interval-sane? x-ext)
        (- (cdr x-ext) (car x-ext))
        0)))
    
#(define (apply-proc-to-leading-two-args proc ls rl)
  (if (null? (cdr ls))
      (reverse rl)
      (apply-proc-to-leading-two-args
        proc
        (cdr ls)
        (cons (proc (car ls) (cadr ls)) rl))))

#(define (make-amplitudes-list amplitudes total-span wavelength)
(format #t "\n\nMakes a list of amplitudes for the vibrato ~A ~A \n\n"  amplitudes (length amplitudes)) 
  (if (= (length amplitudes) 1)
      (set! amplitudes (append amplitudes amplitudes)))
  (let* (
         ;; how many waves for the entire total-span
         (lngth (/ total-span wavelength))
         ;; the total-span is divided into parts:
         (parts (1- (length amplitudes)))
         ;; each part gets that much waves
         (partial-length (/ lngth parts))
         ;; get a list of amplitude-pairs, i.e.:
         ;; '(1 2 3 4) -> '((1 . 2) (2 . 3) (3 . 4))
         (amp-pairs
           (apply-proc-to-leading-two-args
             cons
             amplitudes
             '()))
         ;; calculate the amplitudes
         (amplitudes-list
           (append-map
             (lambda (amp-pair)
               (map
                 (lambda (n)
                   (+ (car amp-pair) 
                       (* (/ n partial-length)
                          (- (cdr amp-pair) (car amp-pair)))))
                 (iota (ceiling partial-length))))
              amp-pairs)))
      ;; don't forget last amplitude
      (append amplitudes-list (list (last amplitudes)))))

#(define (wave-line-stencil left-bound x-span thick amplitude-list wave-length) 

  (if (zero? x-span)
      empty-stencil
      (let* (;; get the amount of waves which will be needed
             (waves-amount (length amplitude-list))
             ;; the added waves would result in a line with length
             (raw-line-length (* waves-amount wave-length))
             ;; get the factor to scale the provided wave-length to ensure
             ;; matching lengths
             (corr (/ raw-line-length x-span))
             ;; calculate the scaled wave-length
             (scaled-wave-length (/ wave-length corr)))
         (make-path-stencil
           (append
             `(moveto ,left-bound 0.0)
             (append-map
               (lambda (amp)
                 `(rcurveto
                   ,(/ scaled-wave-length 3.0) ,amp 
                   ,(* 2 (/ scaled-wave-length 3.0)) ,(- amp)
                   ,scaled-wave-length 0.0))
               amplitude-list))
            thick
            1
            1
            #f))))

#(define (make-wavy-vibrato-stencil grob amplitudes wave-length thickness)
"Creates a stencil that draws a wavy line for vibrato based on @var{amplitudes},
a list of vertival lengths, and @var{wave-length} for the horizontal extent.
"
  (let* ((orig (ly:grob-original grob))
         (siblings (if (ly:grob? orig) (ly:spanner-broken-into orig) '()))
         (thick (ly:grob-property grob 'thickness thickness ;;0.2
         ))
         ;; length of the actual grob
         (xspan (grob-width grob))
         ;; add the length of all siblings
         (total-span
           (if (null? siblings)
               (grob-width grob)
               (reduce + 0 (map (lambda (g) (grob-width g)) siblings))))
         ;; get the x-position for the start
         (left-bound 
           (if (or (null? siblings) (eq? (car siblings) grob)) 
               ;; compensate thickness of the line
               (* thick -2)
               ;; start a little left
               (1- (assoc-get 'X (ly:grob-property grob 'left-bound-info)))))
         ;; get the length of the already done parts of the wavy line
         (span-so-far 
           (if (null? siblings) 
               0
               (-
                 (reduce + 0 
                   (map 
                     (lambda (g) (grob-width g)) 
                     (member grob (reverse siblings))))
                 xspan)))
         ;; get the entire list of amplitudes
         (amplitude-list 
           (make-amplitudes-list amplitudes total-span wave-length))
         ;;;; limit the amplitude-list to the needed values
         ;;;; there may be rounding issues
         ;; delete already done amplitudes from 'amplitude-list'-head
         (amplitude-list-tail
            (drop
              amplitude-list
              (inexact->exact (floor (/ span-so-far wave-length)))))
         ;; limit 'amplitude-list-tail' to the actual needed values    
         (amplitude-todo
           (if (zero? (inexact->exact (floor (/ xspan wave-length))))
               amplitude-list-tail
               (take
                 amplitude-list-tail
                 (inexact->exact (ceiling (/ xspan wave-length))))))
         ;; process the final stencil
         (final-stencil
           (wave-line-stencil
             left-bound
             xspan 
             thick
             (if (null? siblings)
                 amplitude-list
                 amplitude-todo)
             wave-length))
         (bound-details
           (ly:grob-property grob 'bound-details))
         ;; bound-details.left.padding affects both broken and unbroken spanner
         ;; whereas bound-details.left-broken.padding only affects the broken
         ;; spanner part (same for right and right-broken)
         ;; We need to move the stencil along x-axis if padding is inserted to
         ;; the left
         (x-offset
           (cond
             ((or (null? siblings) (equal? grob (car siblings)))
               (assoc-get 'padding (assoc-get 'left bound-details '()) 0))
             ((member grob siblings)
               ;; get at least one working value for the offset
               (or (assoc-get 
                     'padding 
                     (assoc-get 'left-broken bound-details '()) 
                     #f)
                   (assoc-get 'padding (assoc-get 'left bound-details '()) 0)))
             (else 0))))

      (ly:stencil-translate-axis
        final-stencil
        ;; TODO
        ;; there's a little inconsistency here, with the need to add some
        ;; correction, i.e. (* thick 2)
        (if (zero? x-offset)
            0
            (- x-offset (* thick 2)))
        X)))

vibrato = 
#(define-music-function (amplitudes wave-length thickness) (list? number? number?) 
"Overrides @code{TrillSpanner.after-line-breaking}, setting a new stencil,
drawning a wavy line looking at @var{amplitudes} and @var{wave-length} with thickness @var{thickness}.
Limitations:
  - @var{wave-length} is a constant, it can't be changed dynamically while
    processing one vibrato.
  - Each part of the vibrato (growing or shrinking) is of equal length.
    Would be nice to have something like:
      go from amplitude 1 to 4 while the underlying music lasts a quarter
"
#{  
  \once \override TrillSpanner.after-line-breaking = 
    #(lambda (grob) 
       (ly:grob-set-property! grob 'stencil 
         (make-wavy-vibrato-stencil grob amplitudes wave-length thickness)))
#})


