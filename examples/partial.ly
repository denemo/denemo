Clef = \clef treble
Key =  \notes \key c \major
TimeSig = \notes \time 4/4
MyViolin = \context Voice = MyVln \notes  { \Clef \Key \TimeSig \partial 8 c''8 d'' e'' f''2 g''4 c4*1/4 c4*1/4 c4*1/4 c4*1/4 c4*1/4 c4*1/4 c4*1/4 c4*1/4 e''4 f''4 d''2} 
MyViolinStaff = \context Staff = MyStaff <
\MyViolin
> 
\score {
    \MyViolinStaff
    \paper {}
}
