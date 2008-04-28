
Clef = \clef treble
Key =  \notes \key c \major
TimeSig = \notes \time 4/4
MyViolin = \context Voice = MyVln \notes  { \Clef \Key \TimeSig c''4 \repeat volta 2 { d < e g c > f}|}
MyViolinStaff = \context Staff = MyStaff <
\MyViolin
>
\score {
    \MyViolinStaff
    \paper {}
}
