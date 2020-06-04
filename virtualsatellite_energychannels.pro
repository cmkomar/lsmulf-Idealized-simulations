
; RBSP
; MagEIS electron detector
; (keV from MagEIS instrument paper Table 1 - note, these differ from
; actual energies with peak response, which are spacecraft dependent
; and probably change with time - see updates on ECT site)

MagEIS_en = $
   [ 19.5, 32.7, 50.0, 71.8, 98.1, 0129, 0166, 0207, 0146, $
     0232, 0334, 0450, 0580, 0722, 0877, 1040, 1200, 1740, $
     2520, 3770 ]

; REPT electron detector
; (keV from Table 7 in Baker et al instrument paper)

REPT_en = $
   [ 2300, 2850, 3600, 4500, 5600, 7150, 8800, $
     11650, 22550, 59450 ]


; LANL-GEO
; SOPA electron detector
; (keV, from LANL-GEO data files)

SOPAe_en = $
   [ 61.2373, 88.7412, 125.499, 183.712, 266.224, $
     396.863, 612.372, 908.295, 1284.52, 1936.49 ]

; ESP electron detector
; (keV, from LANL-GEO data files)

ESPe_en = $
   [ 01122.5, 1989.98, 2437.21, 03074.09, 3968.63, $
     5196.15, 6841.05, 9178.24, 16692.5 ]


; THEMIS
; SST electron energy channels
; (keV, from Drew Turner 2011 IGPP talk)
SST_en = $
   [ 031., 041., 052., 065., 091., 0137., 0200., 0288., $
     401., 413., 545., 556., 715., 1000. ]







end
