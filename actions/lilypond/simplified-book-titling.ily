#(define (string-upper-case str) str)
\paper {
  tocTitleMarkup = \markup \column {
    \vspace #2
    \fontsize #6 \fill-line { \paper-prop #'tocTitle "TABLE OF CONTENTS" }
    \vspace #2
  }
  tocPieceMarkup = \markup \fill-line {
    \line-width-ratio #0.7 \fill-line {
       \fontsize #6 \line { \fromproperty #'toc:text }
      \fromproperty #'toc:page
    }
  }
  tocSectionMarkup = \markup \italic \column {
    \fill-line { \fromproperty #'toc:text }
  }
  tocChapterMarkup = \markup \large \italic \column {
    \vspace #1
    \fontsize #2 \fill-line { \fromproperty #'toc:text }
    \vspace #1
  }
}
