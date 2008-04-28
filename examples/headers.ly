\header {
  dedication = "dedication"
  title = "Title"
  subtitle = "Subtitle"
  subsubtitle = "Subsubtitle"
  composer = "Composer (xxxx-yyyy)"
  opus = "Opus 0"
  piece = "Piece I"
  instrument = "Instrument"
  arranger = "Arranger"
  poet = "Poet"
  texttranslator = "Translator"
  copyright = "public domain"
  source =  "urtext "
  enteredby = "your name here"
  maintainerEmail = "your email here"
  texidoc = "The standard header that ought to be above a file."

}
Key =  \notes \key c \major
TimeSig = \notes \time 4/4
MyViolin = \context Voice = MyVln \notes  { \Key \TimeSig c''4 }
MyViolinStaff = \context Staff = MyStaff <
\MyViolin
>
\score {
    \MyViolinStaff
    \paper {}
}
