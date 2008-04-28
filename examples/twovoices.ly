
Key =  \notes \key c \minor
TimeSig = \notes \time 3/4

ViolinI = \context Voice = VlnI \notes  { \Key \TimeSig c''4 d'' e'' }
ViolinII = \context Voice = VlnII \notes  { \Key  \TimeSig a''4 b'' c'' }

Violini = \context Staff = Vlns <
\ViolinI
\ViolinII
>

\score {
    \Violini
    \paper {}
}
