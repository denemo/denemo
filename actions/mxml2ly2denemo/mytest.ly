
\version "2.10.33"
% converted from mytest.xml
%\include "mytest-defs.ly" 

NilsAssign = { b b b b }

PartPOneVoiceOne =  {
    |  % 1
    % \clef "G" \key f \major \time 3/4 | % 1
    r4 c'4 r4 | % 2
    c''2 a'4 | % 3
    g'2 f'4 | % 4
    d'4 f'4 d'4 | % 5
    e'2 c'4 | % 6
    d'4 e'4 f'4 | % 7
    a'4 c''4 e''4 | % 8
    d''2. | % 9
    c''2 c''4 | % \barNumberCheck #10
    d''2 a'4 | % 11
    c''2 b'4 | % 12
    c''2 g'4 | % 13
    bes'2 a'4 | % 14
    a'4 g'4 f'4 | % 15
    d'4 e'4 f'4 | % 16
    fis'2 g'4 | % 17
    c''2 c'4 | % 18
    c''2 a'4 | % 19
    g'2 f'4 | % \barNumberCheck #20
    d'4 f'4 d'4 | % 21
    e'2 c'4 | % 22
    d'4 e'4 f'4 | % 23
    a'4 c''4 e''4 | % 24
    d''2. | % 25
    c''2 c''4 | % 26
    f''2 cis''4 | % 27
    e''2 d''4 | % 28
    d''2 a'4 | % 29
    c''2 bes'4 | %\barNumberCheck #30
    d'4 e'4 f'4 | % 31
    a'2 a'4 | % 32
    f'2. ~ | % 33
    f'2 r4 R1*3/4 }

		<<
        \new Staff <<
                \context Voice = "PartPOneVoiceOne"  \PartPOneVoiceOne
                >>
                
        \new Staff <<
                \context Voice = "NilsAssign"  \NilsAssign
                >>
        >>

