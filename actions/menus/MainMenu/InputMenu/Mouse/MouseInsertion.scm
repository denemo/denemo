;;MouseInsertion
(d-AddKeybinding "InsertOneNote" "PrsL-Shift")
(d-AddKeybinding "AddNoteToChord" "PrsL-Shift-Alt")
(d-AddKeybinding "StagedDelete" "PrsL-Control")

(d-CreatePaletteButton  "Select Duration" "𝅝" "Selects 𝅝 as the duration for inserting notes or rests"  "(d-Set0)")

(d-CreatePaletteButton  "Select Duration" "𝅗𝅥" "Selects 𝅗𝅥 as the duration for inserting notes or rests"  "(d-Set1)")

(d-CreatePaletteButton  "Select Duration" "𝅘𝅥" "Selects 𝅘𝅥 as the duration for inserting notes or rests"  "(d-Set2)")

(d-CreatePaletteButton  "Select Duration" "𝅘𝅥𝅮" "Selects 𝅘𝅥𝅮 as the duration for inserting notes or rests"  "(d-Set3)")

(d-CreatePaletteButton  "Select Duration" "𝅘𝅥𝅯" "Selects 𝅘𝅥𝅯 as the duration for inserting notes or rests"  "(d-Set4)")

(d-CreatePaletteButton  "Select Duration" "𝅘𝅥𝅱" "Selects 𝅘𝅥𝅱 as the duration for inserting notes or rests"  "(d-Set5)")
(d-CreatePaletteButton  "General"  "♯" "Sharpen the note at the cursor or next entered note if the cursor is in the appending position"  "(d-SharpenNote)")
(d-CreatePaletteButton  "General"  "♭" "Flatten the note at the cursor or next entered note if the cursor is in the appending position"  "(d-FlattenNote)")
(d-CreatePaletteButton  "General"  "•" "Dot, double dot or undot the note at the cursor"   "(d-DotDoubleDotNoDot)")
(d-CreatePaletteButton  "General"  "(" "Begin Slur from note/chord at cursor, or remove a Begin Slur" "(d-ToggleBeginSlur)")
(d-CreatePaletteButton  "General"  ")" "End Slur at note/chord at cursor, or remove an End Slur" "(d-ToggleEndSlur)")

(d-CreatePaletteButton  "General"  "☊" "Tie from the note/chord at the cursor - will be ignored if next note is not at the same pitch" "(d-ToggleTie)")
(d-CreatePaletteButton  "General"  "3" "Alternates inserting start triplet and end triplet markers" "(d-ToggleTripleting)")
(d-CreatePaletteButton  "General" "<span font=\"times\" size=\"large\" font_style=\"italic\" font_weight=\"bold\">Allegro</span>" "Insert a tempo change at cursor" "(d-MetronomeMark)")

(d-CreatePaletteButton   "Dynamics" "<span font=\"times\" size=\"large\" font_style=\"italic\" font_weight=\"bold\">f p</span>" "Insert a Dynamic Marking at the cursor" "(d-DynamicText)")
(d-CreatePaletteButton   "Dynamics"  "<span font=\"times\" size=\"large\" font_weight=\"bold\">&lt;</span>" "Start Crescrendo from note/chord at cursor" "(d-ToggleStartCrescendo)")
(d-CreatePaletteButton   "Dynamics" "<span font=\"times\" size=\"large\" font_weight=\"bold\">&lt;|</span>" "End Crescendo on note/chord at cursor" "(d-ToggleEndCrescendo)")
(d-CreatePaletteButton   "Dynamics" "<span font=\"times\" size=\"large\" font_weight=\"bold\">&gt;</span>" "Start Diminuendo from note/chord at cursor " "(d-ToggleStartDiminuendo)")
(d-CreatePaletteButton   "Dynamics"  "<span font=\"times\" size=\"large\" font_weight=\"bold\">&gt;|</span>" "End Diminuendo from note/chord at cursor" "(d-ToggleEndDiminuendo)")


(if DenemoPref_newbie
    (d-InfoDialog (_ "To insert notes hold down Shift key while left-clicking the mouse. To add to a chord hold down Shift and Alt, to delete hold down Control.
    Or use keys a,b,c,d,e,f,g, 0,1,2,3,4 (with Shift, Control or Alt).
    Or use palettes. To hide palettes right-click on a button and choose Edit Palette (saves space).
    To cut out excessive messages like this use Help menu ->Turn Excessive Tooltips Off/On.")))
