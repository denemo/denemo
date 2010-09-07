
%mine = {a b c}

%\score { a << b g>> d }


mine = \new Staff  { ais' << c'' bis ees >> g g g}
\score {
 << 
\mine
\mine
 >> 
 }

\score {
<<
 \new Staff  {\clef bass \time 3/16  \key d \major {a b << e g b >> c c c} }
 \new Staff  {d e f << c e g >> a a a}
\mine
>>
}

\layout{
        }
\header{
        }



