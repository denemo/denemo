
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
 \new Staff  << {a b << e g b >> c c c} >>
 \new Staff  {d e f << c e g >> a a a}
\mine
>>
}