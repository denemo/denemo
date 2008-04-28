


Key =  \notes \key c \minor
TimeSig = \notes \time 3/4

ViolinI = \context Voice = VlnI \notes  { \Key \TimeSig c''4 d'' e'' |
 ees''4  f''  g''  }
ViolinII = \context Voice = VlnII \notes  { \Key  \TimeSig a''4 bes'' c'' c''4  bes'  aes'  |
 }

Violini = \context Staff = Vlns <
\ViolinI
\ViolinII
>
tempStaff = \context Staff = temp < \ViolinII>
\score {
<
    \Violini
    \tempStaff
>
    \paper {}
}
