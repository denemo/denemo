#(define (string-upper-case str) str)
#(define-markup-command (piece-title layout props title) (markup?)
  (interpret-markup layout props
   (markup #:column (#:vspace 1
                     #:fill-line (#:fontsize 4 title)
                     #:vspace 1))))
                     
\paper {
  bookTitleMarkup = \markup \when-property #'header:title {
     { \postscript #"
                    gsave
                    initmatrix
                    0.35 setlinewidth 40 20 moveto 520 0 rlineto 0 800 rlineto -520 0 rlineto 0 -800 rlineto  stroke
                    0.15 setlinewidth 45 25 moveto 510 0 rlineto 0 790 rlineto -510 0 rlineto 0 -790 rlineto  stroke
                    grestore" }
    \column {
      \vspace #6
      \fill-line { \fontsize #8 \italic \fromproperty #'header:composer }
      \vspace #1
      \fill-line { \fontsize #8 \italic \fromproperty #'header:poet }
      \vspace #6
      \fill-line { \fontsize #10 \fromproperty #'header:title }
      \vspace #6
      \fill-line { \postscript #"-20 0 moveto 40 0 rlineto stroke" }
      \vspace #6
      \fill-line { \fontsize #5 \fromproperty #'header:date }
      \vspace #1 
      \fill-line {
        \when-property #'header:arrangement \column {
          \vspace #5
          \fill-line { \fontsize #3 \fromproperty #'header:arrangement }
        }
      }
    }
  }
  scoreTitleMarkup = \markup \null


oddFooterMarkup = \markup {
  \column {
    \fill-line {
      %% Copyright header field only on last page.
      \on-the-fly #last-page { \fromproperty #'header:copyright
      \fromproperty #'header:tagline }
    }
  }
}
evenFooterMarkup = \oddFooterMarkup
}
