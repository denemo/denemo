(define (DrumGM2Custom)

;This is not very usefull currently.

;GM Midi Drumtable
(define gmAcousticBasedrum "b,,") ;35
(define gmBassDrum "c,") ;36 low c c, 
(define gmSideStick "cis,")	;37
(define gmAcousticSnare "d,") ;38
(define gmHandClap "dis,") ;39
(define gmElectricSnare "e,");40
(define gmLowFloorTom "f,") ;41
(define gmClosedHighHat "fis,") ;42
(define gmHighFloorTom "g,") ;43
(define gmPedalHighHat "gis,") ;44
(define gmLowTom "a,") ;45
(define gmOpenHighHat "ais,") ;46
(define gmLowMidTom "b,") ;47
(define gmHighMidTom "c") ;48  "small c" c
(define gmCrashCymbal1 "cis") ;49
(define gmHighTom "d") ;50
(define gmRideCymbal1 "dis") ;51
(define gmChineseCymbal1 "e") ;52
(define gmRideBell "f") ;53
(define gmTambourine "fis") ;54
(define gmSplashCymbal "g") ;55
(define gmCowbell "gis") ;56
(define gmCrashCymbal2 "a") ;57
(define gmVibraslap "ais") ;58
(define gmRideCymbal2 "b") ;59
(define gmHighBongo "c'") ;60  Middle C c'
(define gmLowBongo "cis'") ;61
(define gmMuteHighConga "d'") ;62
(define gmOpenHighConga "dis'") ;63
(define gmLowConga "e'") ;64
(define gmHighTimbale "f'") ;65
(define gmLowTimbale "fis'") ;66
(define gmHighAgogo "g'") ;67
(define gmLowAgogo "gis'") ;68
(define gmCabasa "a'") ;69
(define gmMaracas "ais'") ;70
(define gmShortWhistle "b'") ;71
(define gmLongWhistle "c") ;72 "high c" c
(define gmShortGuiro "cis''") ;73
(define gmLongGuiro "d''") ;74
(define gmClaves "dis''") ;75
(define gmHighWoodBlock "e''") ;76
(define gmLowWoodBlock "f''") ;77
(define gmMuteCuica "g''") ;78
(define gmOpenCuica "gis''") ;79
(define gmMuteTriangle "a''") ;80
(define gmOpenTriangle "ais''") ;81 a'
 
;Custom DrumTable
(define AcousticBasedrum "b,,") ;35
(define BassDrum "c,") ;36 low c c, 
(define SideStick "cis,")	;37
(define AcousticSnare "d,") ;38
(define HandClap "dis,") ;39
(define ElectricSnare "e,");40
(define LowFloorTom "f,") ;41
(define ClosedHighHat "fis,") ;42
(define HighFloorTom "g,") ;43
(define PedalHighHat "gis,") ;44
(define LowTom "a,") ;45
(define OpenHighHat "ais,") ;46
(define LowMidTom "b,") ;47
(define HighMidTom "c") ;48  "small c" c
(define CrashCymbal1 "cis") ;49
(define HighTom "d") ;50
(define RideCymbal1 "dis") ;51
(define ChineseCymbal1 "e") ;52
(define RideBell "f") ;53
(define Tambourine "fis") ;54
(define SplashCymbal "g") ;55
(define Cowbell "gis") ;56
(define CrashCymbal2 "a") ;57
(define Vibraslap "ais") ;58
(define RideCymbal2 "b") ;59
(define HighBongo "c'") ;60  Middle C c'
(define LowBongo "cis'") ;61
(define MuteHighConga "d'") ;62
(define OpenHighConga "dis'") ;63
(define LowConga "e'") ;64
(define HighTimbale "f'") ;65
(define LowTimbale "fis'") ;66
(define HighAgogo "g'") ;67
(define LowAgogo "gis'") ;68
(define Cabasa "a'") ;69
(define Maracas "ais'") ;70
(define ShortWhistle "b'") ;71
(define LongWhistle "c") ;72 "high c" c
(define ShortGuiro "cis''") ;73
(define LongGuiro "d''") ;74
(define Claves "dis''") ;75
(define HighWoodBlock "e''") ;76
(define LowWoodBlock "f''") ;77
(define MuteCuica "g''") ;78
(define OpenCuica "gis''") ;79
(define MuteTriangle "a''") ;80
(define OpenTriangle "ais''") ;81 a'
 
(cond 
      ((string-ci=? (d-GetNotes) gmAcousticBasedrum) (d-PutNoteName AcousticBasedrum))
      ((string-ci=? (d-GetNotes) gmBassDrum) (d-PutNoteName BassDrum))
      ((string-ci=? (d-GetNotes) gmSideStick) (d-PutNoteName SideStick))
      ((string-ci=? (d-GetNotes) gmAcousticSnare) (d-PutNoteName AcousticSnare))
      ((string-ci=? (d-GetNotes) gmHandClap) (d-PutNoteName HandClap))
      ((string-ci=? (d-GetNotes) gmElectricSnare) (d-PutNoteName ElectricSnare))
      ((string-ci=? (d-GetNotes) gmLowFloorTom) (d-PutNoteName LowFloorTom))
      ((string-ci=? (d-GetNotes) gmClosedHighHat) (d-PutNoteName ClosedHighHat))
      ((string-ci=? (d-GetNotes) gmHighFloorTom) (d-PutNoteName HighFloorTom))
      ((string-ci=? (d-GetNotes) gmPedalHighHat) (d-PutNoteName PedalHighHat))
      ((string-ci=? (d-GetNotes) gmLowTom) (d-PutNoteName LowTom))
      ((string-ci=? (d-GetNotes) gmOpenHighHat) (d-PutNoteName OpenHighHat))
      ((string-ci=? (d-GetNotes) gmLowMidTom) (d-PutNoteName LowMidTom))
 
      ((string-ci=? (d-GetNotes) gmHighMidTom) (d-PutNoteName HighMidTom)) 
      ((string-ci=? (d-GetNotes) gmCrashCymbal1) (d-PutNoteName CrashCymbal1))
      ((string-ci=? (d-GetNotes) gmHighTom) (d-PutNoteName HighTom))
      ((string-ci=? (d-GetNotes) gmRideCymbal1) (d-PutNoteName RideCymbal1))
      ((string-ci=? (d-GetNotes) gmChineseCymbal1) (d-PutNoteName ChineseCymbal1))
      ((string-ci=? (d-GetNotes) gmRideBell) (d-PutNote NameRideBell))
      ((string-ci=? (d-GetNotes) gmTambourine) (d-PutNoteName Tambourine))
      ((string-ci=? (d-GetNotes) gmSplashCymbal) (d-PutNoteName SplashCymbal))
      ((string-ci=? (d-GetNotes) gmCowbell) (d-PutNoteName Cowbell))
      ((string-ci=? (d-GetNotes) gmCrashCymbal2) (d-PutNoteName CrashCymbal2))
      ((string-ci=? (d-GetNotes) gmVibraslap) (d-PutNoteName Vibraslap))
      ((string-ci=? (d-GetNotes) gmRideCymbal2) (d-PutNoteName RideCymbal2))
      ((string-ci=? (d-GetNotes) gmHighBongo) (d-PutNoteName HighBongo))
 
      ((string-ci=? (d-GetNotes) gmLowBongo) (d-PutNoteName LowBongo)) 
      ((string-ci=? (d-GetNotes) gmMuteHighConga) (d-PutNoteName MuteHighConga))       
      ((string-ci=? (d-GetNotes) gmOpenHighConga) (d-PutNoteName OpenHighConga)) 
      ((string-ci=? (d-GetNotes) gmLowConga) (d-PutNoteName LowConga)) 
      ((string-ci=? (d-GetNotes) gmHighTimbale) (d-PutNoteName HighTimbale))       
      ((string-ci=? (d-GetNotes) gmLowTimbale) (d-PutNoteName LowTimbale)) 
      ((string-ci=? (d-GetNotes) gmHighAgogo) (d-PutNoteName HighAgogo)) 
      ((string-ci=? (d-GetNotes) gmLowAgogo) (d-PutNoteName LowAgogo))       
      ((string-ci=? (d-GetNotes) gmCabasa) (d-PutNoteName Cabasa)) 
      ((string-ci=? (d-GetNotes) gmMaracas) (d-PutNoteName Maracas)) 
      ((string-ci=? (d-GetNotes) gmShortWhistle) (d-PutNoteName ShortWhistle))       
      ((string-ci=? (d-GetNotes) gmLongWhistle) (d-PutNoteName LongWhistle)) 
 
      ((string-ci=? (d-GetNotes) gmShortGuiro) (d-PutNoteName ShortGuiro)) 
      ((string-ci=? (d-GetNotes) gmLongGuiro) (d-PutNoteName LongGuiro)) 
      ((string-ci=? (d-GetNotes) gmClaves) (d-PutNoteName Claves))
      ((string-ci=? (d-GetNotes) gmHighWoodBlock) (d-PutNoteName HighWoodBlock))
      ((string-ci=? (d-GetNotes) gmLowWoodBlock) (d-PutNoteName LowWoodBlock))
      ((string-ci=? (d-GetNotes) gmMuteCuica) (d-PutNoteName MuteCuica))
      ((string-ci=? (d-GetNotes) gmOpenCuica) (d-PutNoteName OpenCuica ))
      ((string-ci=? (d-GetNotes) gmMuteTriangle) (d-PutNoteName MuteTriangle))
      ((string-ci=? (d-GetNotes) gmOpenTriangle) (d-PutNoteName OpenTriangle))
 
      (else (newline))
)
 
 
); End of DrumGM2Custom
 
(DrumGM2Custom)
(d-NextNote)
(d-RefreshDisplay)