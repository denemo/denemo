#(define (string-upper-case str) str)
#(define-markup-command (piece-title layout props title) (markup?)
  (interpret-markup layout props
   (markup #:column (#:vspace 1
                     #:fill-line (#:fontsize 4 title)
                     #:vspace 1))))
#(define-markup-command (when-notproperty layout props symbol markp) (symbol? markup?)
  (if (chain-assoc-get symbol props)
      (ly:make-stencil '()  '(1 . -1) '(1 . -1))
      (interpret-markup layout props markp)))

                     
\paper {
  bookTitleMarkup = \markup \when-property #'header:title {
     { \postscript #"
                    gsave
                    initmatrix
                    1 setlinewidth 40 40 moveto 517 0 rlineto 0 760 rlineto -517 0 rlineto 0 -760 rlineto  stroke
                    0.5 setlinewidth 45 45 moveto 507 0 rlineto 0 750 rlineto -507 0 rlineto 0 -750 rlineto  stroke
                    grestore" }
    \column {
      \when-property #'header:poet \vspace #6
      \when-notproperty #'header:poet  \vspace #16
      \fill-line { \fontsize #8 \italic \fromproperty #'header:composer }
      \vspace #1
      \when-property #'header:poet  
          \fill-line { \fontsize #8 \italic \fromproperty #'header:poet }
      \when-property #'header:poet     
          \vspace #6
      \when-notproperty #'header:poet  \vspace #2
      \fill-line { \fontsize #10 \fromproperty #'header:title }
      \vspace #1
      \fill-line { \postscript #"-20 0 moveto 40 0 rlineto stroke" }
      \vspace #6
      \fill-line { \fontsize #5 \fromproperty #'header:date }
      \when-property #'header:date \vspace #6
      \when-property #'header:instrumentation 
          \fill-line { \fontsize #5 \italic \fromproperty #'header:instrumentation }
			\when-property #'header:instrumentation \vspace #4
      \when-property #'header:incipit 
          \fill-line { \fontsize #5 \italic \fromproperty #'header:incipit }
      
      \vspace #1 
      \fill-line {
        \when-property #'header:arranger \column {
          \vspace #5
          \fill-line { \fontsize #3 \fromproperty #'header:arranger }
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
\pageBreak
