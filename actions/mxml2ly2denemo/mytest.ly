
%mine = {a b c}


mine = \new Staff  { ais' << c'' bis ees >> cis,}
%\score {
% << 
%\mine
%\mine
% >> 
% }

\score {
<<
 \new Staff  {a b << e g b >> c}
 \new Staff  {d e f << c e g >>}
\mine
>>
}