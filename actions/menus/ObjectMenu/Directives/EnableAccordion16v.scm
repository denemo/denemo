;;;EnableAccordion16v

(d-LilyPondDefinition (cons
"Diskant" "\\markup {
  \\musicglyph #\"accordion.discant\"
}"))
(d-LilyPondDefinition (cons
"punkt" "\\markup {
  \\musicglyph #\"accordion.dot\"
}"))
(d-LilyPondDefinition (cons
"accBasson" "^\\markup {
  \\combine
  \\Diskant
  \\raise #0.5 \\punkt
}"))
(d-LilyPondDefinition (cons
"accPiccolo" "^\\markup {
    \\combine
       \\Diskant
       \\raise #2.5 \\punkt
}"))
(d-LilyPondDefinition (cons
"accBandon" "^\\markup {
  \\combine
    \\Diskant
    \\combine
      \\raise #0.5 \\punkt
      \\raise #1.5 \\punkt
}"))
(d-LilyPondDefinition (cons
"accVCello" "^\\markup {
  \\combine
    \\Diskant
    \\combine
      \\raise #0.5 \\punkt
      \\combine
        \\translate #'(-0.6 . 0) \\raise #1.5 \\punkt
        \\translate #'(0.6 . 0) \\raise #1.5 \\punkt
}"))
(d-LilyPondDefinition (cons
"accHarmon" "^\\markup {
  \\combine
    \\Diskant
    \\combine
      \\raise #0.5 \\punkt
      \\combine
        \\raise #1.5 \\punkt
        \\raise #2.5 \\punkt
}"))
(d-LilyPondDefinition (cons
"accOrgel" "^\\markup {
  \\combine
   \\Diskant
    \\combine
      \\raise #0.5 \\punkt
      \\raise #2.5 \\punkt
}"))
(d-LilyPondDefinition (cons
"accHaupt" "^\\markup {
  \\combine
    \\Diskant
    \\combine
      \\raise #0.5 \\punkt
      \\combine
          \\translate #'(0.6 . 0) \\raise #1.5 \\punkt
          \\combine
            \\translate #'(-0.6 . 0) \\raise #1.5 \\punkt
            \\raise #2.5 \\punkt
}"))
(d-LilyPondDefinition (cons
"accCelesta" "^\\markup {
  \\combine
    \\Diskant
    \\combine
       \\translate #'(0.6 . 0)  \\raise #1.5 \\punkt
      \\translate #'(-0.6 . 0) \\raise #1.5 \\punkt
}"))
(d-LilyPondDefinition (cons
"accOboe" "^\\markup {
  \\combine
    \\Diskant
    \\combine
      \\raise #1.5 \\punkt
      \\raise #2.5 \\punkt
}"))
(d-LilyPondDefinition (cons
"accClarin" "^\\markup {
  \\combine
    \\Diskant
    \\raise #1.5 \\punkt
}"))

(d-LilyPondDefinition (cons
"accGeige" "^\\markup {
  \\combine
    \\Diskant
    \\combine
     \\translate #'(-0.6 . 0)  \\raise #1.5 \\punkt
      \\combine
        \\translate #'(0.6 . 0) \\raise #1.5 \\punkt
        \\raise #2.5 \\punkt
}"))
