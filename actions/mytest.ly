
\version "2.12.3"
% automatically converted from Hasse Flute Sonate I Adagio.xml

PartPTwoVoiceOne =  \relative a' {
    \clef "treble" \key d \major \numericTimeSignature\time 4/4 <a fis
        d>8 <fis d b>16. g32 <a fis>8 <fis d>8 <e a cis,>8 e16. fis32 <g e>8
    <cis, g' e>8 | % 2
    <fis d>8 <g cis,>8 <a d,>8 <b e,>8 cis,8 cis16. b32 a8 d16 fis16 | 
    }



% The score definition
\score {
    <<
        \new Staff  = "PartPTwoVoiceTwo" {  \PartPTwoVoiceOne }
        
        >>
   
    }

