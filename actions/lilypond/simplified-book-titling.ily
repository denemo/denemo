#(define (string-upper-case str) str)
#(define-markup-command (piece-title layout props title) (markup?)
  (interpret-markup layout props
   (markup #:column (#:vspace 1
                     #:fill-line (#:fontsize 4 title)
                     #:vspace 1))))
                     
\paper {
  bookTitleMarkup = \markup \when-property #'header:title {
     { \postscript #"0.35 setlinewidth 0 20 moveto 108 0 rlineto 0 -160 rlineto -108 0 rlineto 0 160 rlineto  stroke
	                   0.15 setlinewidth 5 15 moveto 98 0 rlineto 0 -150 rlineto -98 0 rlineto 0 150 rlineto  stroke" }
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
