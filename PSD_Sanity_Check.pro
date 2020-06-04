
; Required files:
;
; @idlrc
; .compile PSD_Master_Plot_Lstar reversect Analyze_Hartinger-10

; Load the data for the priority wave and the null hypothesis
; simulations initialized with Lstar
IF $
   ( do_load_check_lstar EQ !NULL ) $
THEN BEGIN

   PRINT, 'LOADING run_1701: '
   PSD_Master_plot, $
      'run1701', $
      psd_1701_LSTAR, time_1701_LSTAR, $
      rr_1701_LSTAR, xmm_1701_LSTAR, $
      AVERAGE = 55, AE8MIN = 0

   psd0_1701_LSTAR = psd_1701_LSTAR * 0.
   
   PRINT, 'LOADING run1709: '
   PSD_Master_plot, $
      'run1709', $
      psd_1709_LSTAR, time_1709_LSTAR, $
      rr_1709_LSTAR, xmm_1709_LSTAR, $
      AVERAGE = 55, AE8MIN = 0

   psd0_1709_LSTAR = psd_1709_LSTAR * 0.

   FOR $
      iTime = 0, 2160 $
   DO BEGIN

      FOR $
         iMu = 0, 59 $
      DO BEGIN

         psd0_1701_LSTAR( iTime, *, iMu ) = $
            psd_1701_LSTAR( 535, *, iMu ) 
         psd0_1709_LSTAR( iTime, *, iMu ) = $
            psd_1709_LSTAR( 535, *, iMu ) 
         
      ENDFOR

   ENDFOR
   
   ratio_LSTAR = psd_1701_LSTAR / psd_1709_LSTAR

   do_load_check_lstar = !false

ENDIF   

iR = 40
print, 'L-shell: ', rr_1701_LSTAR( iR )

W, 1, 5
WINDOW, 0
LOADCT, 3, /SILENT
IMAGE_CONT, psd_1709_LSTAR( *, *, 37 ), time_1709_LSTAR, rr_1709_LSTAR, $
            TITLE = 'w/o Waves'
MAKELINEY, time_1709_LSTAR( 1260 - 55 ), LINESTYLE = 1
MAKELINEY, time_1709_LSTAR( 1260 + 55 ), LINESTYLE = 1
MAKELINEX, rr_1709_LSTAR( iR ), LINESTYLE = 1

LOADCT, 0, /SILENT
PLOT, time_1709_LSTAR( 1260 - 55 : 1260 + 55 ), $
      psd_1709_LSTAR( 1260 - 55 : 1260 + 55, iR, 37 )
MAKELINEX, MEAN( psd_1709_LSTAR( 1260 - 55 : 1260 + 55, iR, 37 ) ), $
           LINESTYLE = 2

LOADCT, 3, /SILENT
IMAGE_CONT, psd_1701_LSTAR( *, *, 37 ), time_1701_LSTAR, rr_1701_LSTAR, $
            TITLE = 'w/  Waves'
MAKELINEY, time_1701_LSTAR( 1260 - 55 ), LINESTYLE = 1
MAKELINEY, time_1701_LSTAR( 1260 + 55 ), LINESTYLE = 1
MAKELINEX, rr_1701_LSTAR( iR ), LINESTYLE = 1

LOADCT, 0, /SILENT
PLOT, time_1701_LSTAR( 1260 - 55 : 1260 + 55 ), $
      psd_1701_LSTAR( 1260 - 55 : 1260 + 55, iR, 37 )
MAKELINEX, MEAN( psd_1701_LSTAR( 1260 - 55 : 1260 + 55, iR, 37 ) ), $
           LINESTYLE = 2

LOADCT, 70, /SILENT
REVERSECT
IMAGE_CONT, ( ratio_LSTAR( *, *, 37 ) - 1 ) * 100., $
            time_1701_LSTAR, rr_1701_LSTAR, $
            TITLE = '% Diff.', $
            MIN = -100., MAX = 100.

SET_PLOT, 'PS'
DEVICE, FILENAME = 'Images/PSD_Sanity_Check_Lstar_AE8MAX.eps', $
        XSIZE = 10., YSIZE = 10., /INCHES, $
        /PORTRAIT, /COLOR, /ENCAPSULATED, BITS_PER_PIXEL = 8

numx_subplots = 1
x_start = 0.100
x_end   = 0.875
x_space = 0.001
x_subplot_size = $
   ( x_end - x_start - $
     ( numx_subplots - 1 ) * x_space ) / $
   numx_subplots

numy_subplots = 5
y_start = 0.020
y_end   = 0.970
y_space = 0.035
y_subplot_size = $
   ( y_end - y_start - $
     ( numy_subplots - 1 ) * y_space ) / $
   numy_subplots

W, numx_subplots, numy_subplots + 1

fmin = -11.0
fmax = -05.0

ny = numy_subplots
LOADCT, 3, /SILENT
IMAGE_CONT, $
   alog10(psd_1709_LSTAR( *, *, 37 )), $
   time_1709_LSTAR, rr_1709_LSTAR, $
   TITLE = 'w/o Waves', $
   XTITLE = 'Simulation Time [ hours ]', $
   YTITLE = TEXTOIDL( 'L [ R_E ]' ), $
   MIN = fmin, MAX = fmax, $
   POSITION = $
   [ x_start, $
     y_start + ( ny - 1 ) * y_subplot_size + ( ny - 0 ) * y_space, $
     x_start + x_subplot_size, $
     y_start + ( ny - 0 ) * y_subplot_size + ( ny - 1 ) * y_space ], $
   /NORM, CHARTHICK = 2
TVLCT, 255, 255, 255, 255
MAKELINEY, $
   time_1709_LSTAR( 1260 - 55 ), $
   LINESTYLE = 1, COLOR = 255, THICK = 3
MAKELINEY, $
   time_1709_LSTAR( 1260 + 55 ), $
   LINESTYLE = 1, COLOR = 255, THICK = 3
MAKELINEX, $
   rr_1709_LSTAR( iR ), $
   LINESTYLE = 1, COLOR = 255, THICK = 3
LOADCT, 3, /SILENT
COLORBAR, $
   RANGE = [ fmin, fmax ], $
   DIVISIONS = 5, $
   /VERTICAL, /RIGHT, $
   TITLE = TEXTOIDL( 'log_{10}PSD [ c / MeV cm ]^3' ), $
   POSITION = $
   [ x_start + x_subplot_size + .025, $
     y_start + ( ny - 1 ) * y_subplot_size + ( ny - 0 ) * y_space, $
     x_start + x_subplot_size + .050, $
     y_start + ( ny - 0 ) * y_subplot_size + ( ny - 1 ) * y_space ], $
   /NORM, $
   ;; TICKNAMES = [ TEXTOIDL( '10^{-10}' ), TEXTOIDL( '10^{-9}' ), $
   ;;               TEXTOIDL( '10^{-8}' ), TEXTOIDL( '10^{-7}' ), $
   ;;               TEXTOIDL( '10^{-6}' ), TEXTOIDL( '10^{-5}' ) ], $
   TICKLEN = -0.25, $
   CHARSIZE = 2, CHARTHICK = 3
ny = ny - 1

LOADCT, 0, /SILENT
PLOT, $
   time_1709_LSTAR( 1260 - 55 : 1260 + 55 ), $
   psd_1709_LSTAR( 1260 - 55 : 1260 + 55, iR, 37 ), $
   XTITLE = 'Simulation Time [ hours ]', $
   YTITLE = TEXTOIDL( 'PSD [ c / MeV cm ]^3' ), $
   POSITION = $
   [ x_start, $
     y_start + ( ny - 1 ) * y_subplot_size + ( ny - 0 ) * y_space, $
     x_start + x_subplot_size, $
     y_start + ( ny - 0 ) * y_subplot_size + ( ny - 1 ) * y_space ], $
   /NORM, CHARTHICK = 2, THICK = 3
MAKELINEX, $
   MEAN( psd_1709_LSTAR( 1260 - 55 : 1260 + 55, iR, 37 ) ), $
   LINESTYLE = 2, THICK = 3
ny = ny - 1

LOADCT, 3, /SILENT
IMAGE_CONT, $
   alog10(psd_1701_LSTAR( *, *, 37 )), $
   time_1701_LSTAR, rr_1701_LSTAR, $
   TITLE = 'w/  Waves', $
   XTITLE = 'Simulation Time [ hours ]', $
   YTITLE = TEXTOIDL( 'L [ R_E ]' ), $
   min = fmin,max = fmax, $
   POSITION = $
   [ x_start, $
     y_start + ( ny - 1 ) * y_subplot_size + ( ny - 0 ) * y_space, $
     x_start + x_subplot_size, $
     y_start + ( ny - 0 ) * y_subplot_size + ( ny - 1 ) * y_space ], $
   /NORM, CHARTHICK = 2
;; POSITION = $
;; [ 0.075, 0.70, 0.85, 0.90 ], /NORM
TVLCT, 255, 255, 255, 255
MAKELINEY, $
   time_1701_LSTAR( 1260 - 55 ), $
   LINESTYLE = 1, COLOR = 255, THICK = 3
MAKELINEY, $
   time_1701_LSTAR( 1260 + 55 ), $
   LINESTYLE = 1, COLOR = 255, THICK = 3
MAKELINEX, $
   rr_1701_LSTAR( iR ), $
   LINESTYLE = 1, COLOR = 255, THICK = 3
LOADCT, 3, /SILENT
COLORBAR, $
   RANGE = [ fmin, fmax ], $
   DIVISIONS = 5, $
   /VERTICAL, /RIGHT, $
   TITLE = TEXTOIDL( 'log_{10}PSD [ c / MeV cm ]^3' ), $
   POSITION = $
   [ x_start + x_subplot_size + .025, $
     y_start + ( ny - 1 ) * y_subplot_size + ( ny - 0 ) * y_space, $
     x_start + x_subplot_size + .050, $
     y_start + ( ny - 0 ) * y_subplot_size + ( ny - 1 ) * y_space ], $
   /NORM, $
   ;; TICKNAMES = [ TEXTOIDL( '10^{-10}' ), TEXTOIDL( '10^{-9}' ), $
   ;;               TEXTOIDL( '10^{-8}' ), TEXTOIDL( '10^{-7}' ), $
   ;;               TEXTOIDL( '10^{-6}' ), TEXTOIDL( '10^{-5}' ) ], $
   TICKLEN = -0.25, $
   CHARSIZE = 2, CHARTHICK = 3
ny = ny - 1

LOADCT, 0, /SILENT
PLOT, $
   time_1701_LSTAR( 1260 - 55 : 1260 + 55 ), $
   psd_1701_LSTAR( 1260 - 55 : 1260 + 55, iR, 37 ), $
   XTITLE = 'Simulation Time [ hours ]', $
   YTITLE = TEXTOIDL( 'PSD [ c / MeV cm ]^3' ), $
   POSITION = $
   [ x_start, $
     y_start + ( ny - 1 ) * y_subplot_size + ( ny - 0 ) * y_space, $
     x_start + x_subplot_size, $
     y_start + ( ny - 0 ) * y_subplot_size + ( ny - 1 ) * y_space ], $
   /NORM, CHARTHICK = 2, THICK = 3
MAKELINEX, $
   MEAN( psd_1701_LSTAR( 1260 - 55 : 1260 + 55, iR, 37 ) ), $
   LINESTYLE = 2, THICK = 3
ny = ny - 1

LOADCT, 70, /SILENT
REVERSECT
IMAGE_CONT, $
   ( ratio_LSTAR( *, *, 37 ) - 1 ) * 100., $
   time_1701_LSTAR, rr_1701_LSTAR, $
   TITLE = '% Diff.', $
   XTITLE = 'Simulation Time [ hours ]', $
   YTITLE = TEXTOIDL( 'L [ R_E ]' ), $
   MIN = -100., MAX = 100., $
   POSITION = $
   [ x_start, $
     y_start + ( ny - 1 ) * y_subplot_size + ( ny - 0 ) * y_space, $
     x_start + x_subplot_size, $
     y_start + ( ny - 0 ) * y_subplot_size + ( ny - 1 ) * y_space ], $
   /NORM, CHARTHICK = 2
TVLCT, 000, 000, 000, 000
MAKELINEX, $
   rr_1701_LSTAR( iR ), $
   LINESTYLE = 1, COLOR = 000, THICK = 3
PLOT, $
   time_1701_LSTAR, rr_1701_LSTAR, /XS, /YS, $
   TITLE = '% Diff.', $
   XTITLE = 'Simulation Time [ hours ]', $
   YTITLE = TEXTOIDL( 'L [ R_E ]' ), $
   POSITION = $
   [ x_start, $
     y_start + ( ny - 1 ) * y_subplot_size + ( ny - 0 ) * y_space, $
     x_start + x_subplot_size, $
     y_start + ( ny - 0 ) * y_subplot_size + ( ny - 1 ) * y_space ], $
   /NORM, CHARTHICK = 2, /NODATA
COLORBAR, $
   RANGE = [ -100, 100 ], $
   DIVISIONS = 4, $
   /VERTICAL, /RIGHT, $
   TITLE = TEXTOIDL( '% Difference' ), $
   POSITION = $
   [ x_start + x_subplot_size + .025, $
     y_start + ( ny - 1 ) * y_subplot_size + ( ny - 0 ) * y_space, $
     x_start + x_subplot_size + .050, $
     y_start + ( ny - 0 ) * y_subplot_size + ( ny - 1 ) * y_space ], $
   /NORM, $
   CHARSIZE = 2, CHARTHICK = 3
ny = ny - 1

DEVICE, /CLOSE

FOR $
   iMu = 17, 42 $
DO BEGIN
   
   DEVICE, FILENAME = 'Images/PSD_February_Plot_'+ $
           	STRTRIM( STRING( iMu ), 2 ) + '_Lstar_AE8MAX.eps', $
           XSIZE = 10., YSIZE = 10., /INCHES, $
           /PORTRAIT, /COLOR, /ENCAPSULATED, BITS_PER_PIXEL = 8
   
   numx_subplots = 1
   x_start = 0.100
   x_end   = 0.875
   x_space = 0.001
   x_subplot_size = $
      ( x_end - x_start - $
        ( numx_subplots - 1 ) * x_space ) / $
      numx_subplots
   
   numy_subplots = 2
   y_start = 0.020
   y_end   = 0.970
   y_space = 0.035
   y_subplot_size = $
      ( y_end - y_start - $
        ( numy_subplots - 1 ) * y_space ) / $
      numy_subplots
   
   W, numx_subplots, numy_subplots + 1
   
   fmin = -100.0
   fmax =  100.0
   
   ny = numy_subplots
   LOADCT, 70, /SILENT
   REVERSECT
   IMAGE_CONT, $
      ( psd_1709_LSTAR( 536 : *, *, iMu ) / $
        psd0_1709_LSTAR( 536 : *, *, iMu ) - 1 ) * 100., $
      time_1709_LSTAR( 536 : * ), rr_1709_LSTAR, $
      TITLE = 'AE8MAX w/o Waves, mu = ' + $
      	STRING( xmm_1709_LSTAR( iMu ) * 100., FORMAT='(I4)') + $
      	' MeV / G', $
      XTITLE = 'Simulation Time [ hours ]', $
      YTITLE = TEXTOIDL( 'L [ R_E ]' ), $
      MIN = fmin, MAX = fmax, $
      POSITION = $
      [ x_start, $
        y_start + ( ny - 1 ) * y_subplot_size + ( ny - 0 ) * y_space, $
        x_start + x_subplot_size, $
        y_start + ( ny - 0 ) * y_subplot_size + ( ny - 1 ) * y_space ], $
      /NORM, CHARTHICK = 2
   COLORBAR, $
      RANGE = [ fmin, fmax ], $
      DIVISIONS = 4, $
      /VERTICAL, /RIGHT, $
      TITLE = TEXTOIDL( 'PSD [ c / MeV cm ]^3' ), $
      POSITION = $
      [ x_start + x_subplot_size + .025, $
        y_start + ( ny - 1 ) * y_subplot_size + ( ny - 0 ) * y_space, $
        x_start + x_subplot_size + .050, $
        y_start + ( ny - 0 ) * y_subplot_size + ( ny - 1 ) * y_space ], $
      /NORM, $
      TICKLEN = -0.25, $
      CHARSIZE = 2, CHARTHICK = 3
   ny = ny - 1

   LOADCT, 70, /SILENT
   REVERSECT
   IMAGE_CONT, $
      ( psd_1701_LSTAR( 536 : *, *, iMu ) / $
        psd0_1701_LSTAR( 536 : *, *, iMu ) - 1 ) * 100., $
      time_1701_LSTAR( 536 : * ), rr_1701_LSTAR, $
      TITLE = 'AE8MAX w/  Waves, mu = ' + $
      	STRING( xmm_1701_LSTAR( iMu ) * 100., FORMAT='(I4)') + $
      	' MeV / G', $
      XTITLE = 'Simulation Time [ hours ]', $
      YTITLE = TEXTOIDL( 'L [ R_E ]' ), $
      MIN = fmin, MAX = fmax, $
      POSITION = $
      [ x_start, $
        y_start + ( ny - 1 ) * y_subplot_size + ( ny - 0 ) * y_space, $
        x_start + x_subplot_size, $
        y_start + ( ny - 0 ) * y_subplot_size + ( ny - 1 ) * y_space ], $
      /NORM, CHARTHICK = 2
   COLORBAR, $
      RANGE = [ fmin, fmax ], $
      DIVISIONS = 4, $
      /VERTICAL, /RIGHT, $
      TITLE = TEXTOIDL( 'PSD [ c / MeV cm ]^3' ), $
      POSITION = $
      [ x_start + x_subplot_size + .025, $
        y_start + ( ny - 1 ) * y_subplot_size + ( ny - 0 ) * y_space, $
        x_start + x_subplot_size + .050, $
        y_start + ( ny - 0 ) * y_subplot_size + ( ny - 1 ) * y_space ], $
      /NORM, $
      TICKLEN = -0.25, $
      CHARSIZE = 2, CHARTHICK = 3
   ny = ny - 1

   ;; LOADCT, 70, /SILENT
   ;; REVERSECT
   ;; IMAGE_CONT, $
   ;;    ( psd_1701_AE8MIN_lstar( 536 : *, *, iMu ) / $
   ;;      psd0_1701_AE8MIN_lstar( 536 : *, *, iMu ) - 1 ) * 100., $
   ;;    time_1701_AE8MIN_lstar( 536 : * ), rr_1701_AE8MIN_lstar, $
   ;;    TITLE = 'AE8MIN w/  Waves, mu = ' + $
   ;;    	STRING( xmm_1701_AE8MIN_lstar( iMu ) * 100., FORMAT='(I4)') + $
   ;;    	' MeV / G', $
   ;;    XTITLE = 'Simulation Time [ hours ]', $
   ;;    YTITLE = TEXTOIDL( 'L [ R_E ]' ), $
   ;;    MIN = fmin, MAX = fmax, $
   ;;    POSITION = $
   ;;    [ x_start, $
   ;;      y_start + ( ny - 1 ) * y_subplot_size + ( ny - 0 ) * y_space, $
   ;;      x_start + x_subplot_size, $
   ;;      y_start + ( ny - 0 ) * y_subplot_size + ( ny - 1 ) * y_space ], $
   ;;    /NORM, CHARTHICK = 2
;; TVLCT, 255, 255, 255, 255
;; MAKELINEY, $
;;    time_1709_LSTAR( 1260 - 55 ), $
;;    LINESTYLE = 1, COLOR = 255, THICK = 3
;; MAKELINEY, $
;;    time_1709_LSTAR( 1260 + 55 ), $
;;    LINESTYLE = 1, COLOR = 255, THICK = 3
;; MAKELINEX, $
;;    rr_1709_LSTAR( iR ), $
;;    LINESTYLE = 1, COLOR = 255, THICK = 3
;; LOADCT, 3, /SILENT
   ;; COLORBAR, $
   ;;    RANGE = [ fmin, fmax ], $
   ;;    DIVISIONS = 4, $
   ;;    /VERTICAL, /RIGHT, $
   ;;    TITLE = TEXTOIDL( 'PSD [ c / MeV cm ]^3' ), $
   ;;    POSITION = $
   ;;    [ x_start + x_subplot_size + .025, $
   ;;      y_start + ( ny - 1 ) * y_subplot_size + ( ny - 0 ) * y_space, $
   ;;      x_start + x_subplot_size + .050, $
   ;;      y_start + ( ny - 0 ) * y_subplot_size + ( ny - 1 ) * y_space ], $
   ;;    /NORM, $
   ;;    ;; TICKNAMES = [ TEXTOIDL( '10^{-10}' ), TEXTOIDL( '10^{-9}' ), $
   ;;    ;;               TEXTOIDL( '10^{-8}' ), TEXTOIDL( '10^{-7}' ), $
   ;;    ;;               TEXTOIDL( '10^{-6}' ), TEXTOIDL( '10^{-5}' ) ], $
   ;;    TICKLEN = -0.25, $
   ;;    CHARSIZE = 2, CHARTHICK = 3
   ;; ny = ny - 1
   
   DEVICE, /CLOSE

ENDFOR
   
SET_PLOT, 'X'

dim = SIZE( psd_1701_LSTAR, /DIMENSIONS )
nTime = dim( 0 )
nR = dim( 1 )
nMu = dim( 2 )

psd_Time_average_1701_LSTAR = FLTARR( nR, nMu )
;psd_Time_average_1701_AE8MIN_lstar = FLTARR( nR, nMu )
psd_Time_average_1709_LSTAR = FLTARR( nR, nMu )


FOR $
   iR = 0, nR - 1 $
DO BEGIN

   FOR $
      iMu = 0, nMu - 1 $
   DO BEGIN

      psd_Time_Average_1701_LSTAR( iR, iMu ) = $
         MEAN( PSD_1701_LSTAR( 1980 : 2160, iR, iMu ) )
      ;; psd_Time_Average_1701_AE8MIN_lstar( iR, iMu ) = $
      ;;    MEAN( PSD_1701_AE8MIN_lstar( 1980 : 2160, iR, iMu ) )
      psd_Time_Average_1709_LSTAR( iR, iMu ) = $
         MEAN( PSD_1709_LSTAR( 1980 : 2160, iR, iMu ) )
      
   ENDFOR

ENDFOR


test_string = ''
FOR $
   iMu = 26, 42 $
DO BEGIN

   PRINT, 'iMu = ', imu

   SET_PLOT, 'X'
   ;; W, 2, 3
   W, 1, 1
   LOADCT, 0, /SILENT
   yr_max = $
      MAX( CEIL( ALOG10( psd_time_average_1709_LSTAR( 16 : *, iMu ) ) * 1. ) )
   yr_min = yr_max - 3.


    PLOT, rr_1709_LSTAR, psd_time_average_1709_LSTAR( *, iMu ), $
          ;; PLOT, rr_1709_LSTAR, psd_1709_LSTAR( 0, *, iMu ), $
          TITLE = 'AE8MAX w/o  Waves, mu = ' + $
          	STRING( xmm_1709_Lstar( iMu ) * 100., FORMAT='(I4)') + $
          	' MeV / G', $
          XTITLE = TEXTOIDL( 'L [ R_E ]' ), XMINOR = 8, $
          YTITLE = TEXTOIDL( 'PSD [ c / MeV cm ]^3' ), /YLOG, $
          YRANGE = 10^[ yr_min, yr_max  ], $
          ;; YRANGE = [ 1d-10, 1d0 ], $
          ;; YTITLE = TEXTOIDL( 'PSD [ c / MeV cm ]^3' ), $
          ;; YRANGE = [ 0., MAX( psd_1709_LSTAR( 0, *, iMu ) ) ] * 1.2, $
          PSYM = -7, CHARSIZE = 2, CHARTHICK = 2
   OPLOT, rr_1709_LSTAR, psd_time_average_1701_LSTAR( *, iMu ), $
          PSYM =  6
   ;; OPLOT, rr_1709_LSTAR, psd_time_average_1701_AE8MIN_lstar( *, iMu ), $
   ;;        PSYM =  4
   ;; OPLOT, rr_1709_LSTAR, psd_1709_AE8MAZ_lstar( 0, *, iMu ), $
          ;; TITLE = 'AE8MAX w/o  Waves, mu = ' + $
          ;; 	STRING( xmm_1701_AE8MIN_lstar( iMu ) * 100., FORMAT='(I4)') + $
          ;; 	' MeV / G', $
          ;; XTITLE = TEXTOIDL( 'L [ R_E ]' ), XMINOR = 8, $
          ;; YTITLE = TEXTOIDL( 'PSD [ c / MeV cm ]^3' ), /YLOG, $
          ;; ;; YRANGE = [ 1d-10, 1d0 ], $
          ;; ;; YTITLE = TEXTOIDL( 'PSD [ c / MeV cm ]^3' ), $
          ;; YRANGE = [ 0., MAX( psd_1709_LSTAR( 0, *, iMu ) ) ] * 1.2, $
          ;; PSYM = -7, CHARSIZE = 2, CHARTHICK = 2
          ;; PSYM = 4
   ;; OPLOT, rr_1709_LSTAR, psd_time_average_1709_LSTAR( *, iMu ), $
   ;;        PSYM =  6

   ;; OPLOT, rr_1701_LSTAR, psd_1701_LSTAR( 0, *, iMu ), $
          ;; TITLE = 'AE8MAX w/  Waves, mu = ' + $
          ;; 	STRING( xmm_1701_AE8MIN_lstar( iMu ) * 100., FORMAT='(I4)') + $
          ;; 	' MeV / G', $
          ;; XTITLE = TEXTOIDL( 'L [ R_E ]' ), XMINOR = 8, $
          ;; ;; YTITLE = TEXTOIDL( 'PSD [ c / MeV cm ]^3' ), /YLOG, $
          ;; ;; YRANGE = [ 1d-10, 1d0 ], $
          ;; YTITLE = TEXTOIDL( 'PSD [ c / MeV cm ]^3' ), $
          ;; YRANGE = [ 0, MAX( psd_1709_LSTAR( 0, *, iMu ) ) ] * 1.2, $
          ;; PSYM = -7, CHARSIZE = 2, CHARTHICK = 2
          ;; PSYM = 6
   ;; OPLOT, rr_1701_LSTAR, psd_time_average_1701_LSTAR( *, iMu ), $
   ;;        PSYM =  6
   ;;  PLOT, rr_1701_LSTAR, psd_1701_LSTAR( 0, *, iMu ), $
   ;;        TITLE = 'AE8MAX w/  Waves, mu = ' + $
   ;;        	STRING( xmm_1701_AE8MIN_lstar( iMu ) * 100., FORMAT='(I4)') + $
   ;;        	' MeV / G', $
   ;;        XTITLE = TEXTOIDL( 'L [ R_E ]' ), XMINOR = 8, $
   ;;        YTITLE = TEXTOIDL( 'PSD [ c / MeV cm ]^3' ), /YLOG, $
   ;;        ;; YRANGE = [ 1d-10, 1d0 ], $
   ;;        ;; YTITLE = TEXTOIDL( 'PSD [ c / MeV cm ]^3' ), $
   ;;        YRANGE = [ 0, MAX( psd_1709_LSTAR( 0, *, iMu ) ) ] * 1.2, $
   ;;        PSYM = -7, CHARSIZE = 2, CHARTHICK = 2
   ;; OPLOT, rr_1701_LSTAR, psd_time_average_1701_LSTAR( *, iMu ), $
   ;;        PSYM =  6

   ;;  PLOT, rr_1701_AE8MIN_lstar, psd_1701_AE8MIN_lstar( 0, *, iMu ), $
   ;;        TITLE = 'AE8MIN w/  Waves, mu = ' + $
   ;;        	STRING( xmm_1701_AE8MIN_lstar( iMu ) * 100., FORMAT='(I4)') + $
   ;;        	' MeV / G', $
   ;;        XTITLE = TEXTOIDL( 'L [ R_E ]' ), XMINOR = 8, $
   ;;        ;; YTITLE = TEXTOIDL( 'PSD [ c / MeV cm ]^3' ), /YLOG, $
   ;;        ;; YRANGE = [ 1d-10, 1d0 ], $
   ;;        YTITLE = TEXTOIDL( 'PSD [ c / MeV cm ]^3' ), $
   ;;        YRANGE = [ 0, MAX( psd_1709_LSTAR( 0, *, iMu ) ) ] * 1.2, $
   ;;        PSYM = -7, CHARSIZE = 2, CHARTHICK = 2
   ;; OPLOT, rr_1701_AE8MIN_lstar, psd_time_average_1701_AE8MIN_lstar( *, iMu ), $
   ;;        PSYM =  6
   ;;  PLOT, rr_1701_AE8MIN_lstar, psd_1701_AE8MIN_lstar( 0, *, iMu ), $
   ;;        TITLE = 'AE8MIN w/  Waves, mu = ' + $
   ;;        	STRING( xmm_1701_AE8MIN_lstar( iMu ) * 100., FORMAT='(I4)') + $
   ;;        	' MeV / G', $
   ;;        XTITLE = TEXTOIDL( 'L [ R_E ]' ), XMINOR = 8, $
   ;;        YTITLE = TEXTOIDL( 'PSD [ c / MeV cm ]^3' ), /YLOG, $
   ;;        ;; YRANGE = [ 1d-10, 1d0 ], $
   ;;        ;; YTITLE = TEXTOIDL( 'PSD [ c / MeV cm ]^3' ), $
   ;;        YRANGE = [ 0, MAX( psd_1709_LSTAR( 0, *, iMu ) ) ] * 1.2, $
   ;;        PSYM = -7, CHARSIZE = 2, CHARTHICK = 2
   ;; OPLOT, rr_1701_AE8MIN_lstar, psd_time_average_1701_AE8MIN_lstar( *, iMu ), $
   ;;        PSYM =  6

   SET_PLOT, 'PS'
   DEVICE, $
      FILENAME = 'Images/PSD_Line_Plot_mu' + $
      		STRTRIM( STRING( iMu, FORMAT = '(I02)' ), 2 ) + $
      '_Lstar_AE8MAX.eps', $
      XSIZE = 10., YSIZE = 10., /INCHES, $
      /ENCAPSULATED, /PORTRAIT, /COLOR, BITS_PER_PIXEL = 8
   
   ;; LOADCT, 0, /SILENT
   ;; W, 2, 3
   ;;  PLOT, rr_1709_LSTAR, psd_1709_LSTAR( 0, *, iMu ), $
   ;;        TITLE = 'AE8MAX w/o  Waves, mu = ' + $
   ;;        	STRING( xmm_1701_AE8MIN_lstar( iMu ) * 100., FORMAT='(I4)') + $
   ;;        	' MeV / G', $
   ;;        XTITLE = TEXTOIDL( 'L [ R_E ]' ), XMINOR = 8, $
   ;;        ;; YTITLE = TEXTOIDL( 'PSD [ c / MeV cm ]^3' ), /YLOG, $
   ;;        ;; YRANGE = [ 1d-10, 1d0 ], $
   ;;        YTITLE = TEXTOIDL( 'PSD [ c / MeV cm ]^3' ), $
   ;;        YRANGE = [ 0., MAX( psd_1709_LSTAR( 0, *, iMu ) ) ] * 1.2, $
   ;;        PSYM = -7, CHARSIZE = 2, CHARTHICK = 2
   ;; OPLOT, rr_1709_LSTAR, psd_time_average_1709_LSTAR( *, iMu ), $
   ;;        PSYM =  6
   ;;  PLOT, rr_1709_LSTAR, psd_1709_LSTAR( 0, *, iMu ), $
   ;;        TITLE = 'AE8MAX w/o  Waves, mu = ' + $
   ;;        	STRING( xmm_1701_AE8MIN_lstar( iMu ) * 100., FORMAT='(I4)') + $
   ;;        	' MeV / G', $
   ;;        XTITLE = TEXTOIDL( 'L [ R_E ]' ), XMINOR = 8, $
   ;;        YTITLE = TEXTOIDL( 'PSD [ c / MeV cm ]^3' ), /YLOG, $
   ;;        ;; YRANGE = [ 1d-10, 1d0 ], $
   ;;        ;; YTITLE = TEXTOIDL( 'PSD [ c / MeV cm ]^3' ), $
   ;;        YRANGE = [ 0., MAX( psd_1709_LSTAR( 0, *, iMu ) ) ] * 1.2, $
   ;;        PSYM = -7, CHARSIZE = 2, CHARTHICK = 2
   ;; OPLOT, rr_1709_LSTAR, psd_time_average_1709_LSTAR( *, iMu ), $
   ;;        PSYM =  6

   ;;  PLOT, rr_1701_LSTAR, psd_1701_LSTAR( 0, *, iMu ), $
   ;;        TITLE = 'AE8MAX w/  Waves, mu = ' + $
   ;;        	STRING( xmm_1701_AE8MIN_lstar( iMu ) * 100., FORMAT='(I4)') + $
   ;;        	' MeV / G', $
   ;;        XTITLE = TEXTOIDL( 'L [ R_E ]' ), XMINOR = 8, $
   ;;        ;; YTITLE = TEXTOIDL( 'PSD [ c / MeV cm ]^3' ), /YLOG, $
   ;;        ;; YRANGE = [ 1d-10, 1d0 ], $
   ;;        YTITLE = TEXTOIDL( 'PSD [ c / MeV cm ]^3' ), $
   ;;        YRANGE = [ 0, MAX( psd_1709_LSTAR( 0, *, iMu ) ) ] * 1.2, $
   ;;        PSYM = -7, CHARSIZE = 2, CHARTHICK = 2
   ;; OPLOT, rr_1701_LSTAR, psd_time_average_1701_LSTAR( *, iMu ), $
   ;;        PSYM =  6
   ;;  PLOT, rr_1701_LSTAR, psd_1701_LSTAR( 0, *, iMu ), $
   ;;        TITLE = 'AE8MAX w/  Waves, mu = ' + $
   ;;        	STRING( xmm_1701_AE8MIN_lstar( iMu ) * 100., FORMAT='(I4)') + $
   ;;        	' MeV / G', $
   ;;        XTITLE = TEXTOIDL( 'L [ R_E ]' ), XMINOR = 8, $
   ;;        YTITLE = TEXTOIDL( 'PSD [ c / MeV cm ]^3' ), /YLOG, $
   ;;        ;; YRANGE = [ 1d-10, 1d0 ], $
   ;;        ;; YTITLE = TEXTOIDL( 'PSD [ c / MeV cm ]^3' ), $
   ;;        YRANGE = [ 0, MAX( psd_1709_LSTAR( 0, *, iMu ) ) ] * 1.2, $
   ;;        PSYM = -7, CHARSIZE = 2, CHARTHICK = 2
   ;; OPLOT, rr_1701_LSTAR, psd_time_average_1701_LSTAR( *, iMu ), $
   ;;        PSYM =  6

   ;;  PLOT, rr_1701_AE8MIN_lstar, psd_1701_AE8MIN_lstar( 0, *, iMu ), $
   ;;        TITLE = 'AE8MIN w/  Waves, mu = ' + $
   ;;        	STRING( xmm_1701_AE8MIN_lstar( iMu ) * 100., FORMAT='(I4)') + $
   ;;        	' MeV / G', $
   ;;        XTITLE = TEXTOIDL( 'L [ R_E ]' ), XMINOR = 8, $
   ;;        ;; YTITLE = TEXTOIDL( 'PSD [ c / MeV cm ]^3' ), /YLOG, $
   ;;        ;; YRANGE = [ 1d-10, 1d0 ], $
   ;;        YTITLE = TEXTOIDL( 'PSD [ c / MeV cm ]^3' ), $
   ;;        YRANGE = [ 0, MAX( psd_1709_LSTAR( 0, *, iMu ) ) ] * 1.2, $
   ;;        PSYM = -7, CHARSIZE = 2, CHARTHICK = 2
   ;; OPLOT, rr_1701_AE8MIN_lstar, psd_time_average_1701_AE8MIN_lstar( *, iMu ), $
   ;;        PSYM =  6
   ;;  PLOT, rr_1701_AE8MIN_lstar, psd_1701_AE8MIN_lstar( 0, *, iMu ), $
   ;;        TITLE = 'AE8MIN w/  Waves, mu = ' + $
   ;;        	STRING( xmm_1701_AE8MIN_lstar( iMu ) * 100., FORMAT='(I4)') + $
   ;;        	' MeV / G', $
   ;;        XTITLE = TEXTOIDL( 'L [ R_E ]' ), XMINOR = 8, $
   ;;        YTITLE = TEXTOIDL( 'PSD [ c / MeV cm ]^3' ), /YLOG, $
   ;;        ;; YRANGE = [ 1d-10, 1d0 ], $
   ;;        ;; YTITLE = TEXTOIDL( 'PSD [ c / MeV cm ]^3' ), $
   ;;        YRANGE = [ 0, MAX( psd_1709_LSTAR( 0, *, iMu ) ) ] * 1.2, $
   ;;        PSYM = -7, CHARSIZE = 2, CHARTHICK = 2
   ;; OPLOT, rr_1701_AE8MIN_lstar, psd_time_average_1701_AE8MIN_lstar( *, iMu ), $
   ;;        PSYM =  6

   W, 1, 1
   LOADCT, 0, /SILENT
   TVLCT, 255, 000, 000, 101
   TVLCT, 000, 000, 255, 102
   ;;  PLOT, rr_1709_LSTAR, psd_time_average_1709_LSTAR( *, iMu ), $
   ;;        TITLE = 'AE8MAX w/o  Waves, mu = ' + $
   ;;        	STRING( xmm_1701_AE8MIN_lstar( iMu ) * 100., FORMAT='(I4)') + $
   ;;        	' MeV / G', $
   ;;        XTITLE = TEXTOIDL( 'L [ R_E ]' ), XMINOR = 8, $
   ;;        YTITLE = TEXTOIDL( 'PSD [ c / MeV cm ]^3' ), $
   ;;        YRANGE = 10^[ yr_min, yr_max  ], $
   ;;        PSYM = -7, CHARSIZE = 2, CHARTHICK = 3, THICK = 2, $
   ;;        XTHICK  = 3, YTHICK = 3
   ;; OPLOT, rr_1709_LSTAR, psd_time_average_1701_LSTAR( *, iMu ), $
   ;;        PSYM =  -6, COLOR = 101, THICK = 2
   ;; OPLOT, rr_1709_LSTAR, psd_time_average_1701_AE8MIN_lstar( *, iMu ), $
   ;;        PSYM =  -4, COLOR = 102, THICK = 2
   ;; LEGEND, $
   ;;    [ 'No Waves', 'AE8MAX', 'AE8MIN' ], $
   ;;    PSYM = -[ 7, 6, 4 ], LINESTYLE = [ 0, 0, 0 ], $
   ;;    COLOR = [ 000, 101, 102 ], $
   ;;    BOX = 1, /TOP, /LEFT, NUMBER = 1.5, $
   ;;    CHARTHICK = 3, THICK = 3
   
    PLOT, rr_1709_LSTAR, psd_time_average_1709_LSTAR( *, iMu ), $
          TITLE = 'AE8MAX w/o  Waves, mu = ' + $
          	STRING( xmm_1701_Lstar( iMu ) * 100., FORMAT='(I4)') + $
          	' MeV / G', $
          XTITLE = TEXTOIDL( 'L [ R_E ]' ), XMINOR = 8, $
          YTITLE = TEXTOIDL( 'PSD [ c / MeV cm ]^3' ), $
          YRANGE = 10^[ yr_min, yr_max  ], /YLOG, $
          PSYM = -7, CHARSIZE = 2, CHARTHICK = 3, THICK = 2, $
          XTHICK  = 3, YTHICK = 3
   OPLOT, rr_1709_LSTAR, psd_time_average_1701_LSTAR( *, iMu ), $
          PSYM =  -6, COLOR = 101, THICK = 2
   LEGEND, $
      [ 'No Waves', 'AE8MAX' ], $
      PSYM = -[ 7, 6 ], LINESTYLE = [ 0, 0 ], $
      COLOR = [ 000, 101 ], $
      BOX = 1, /BOTTOM, /RIGHT, NUMBER = 1.5, $
      CHARTHICK = 3, THICK = 3
      

   DEVICE, /CLOSE
   ;; READ, test_string

   SET_PLOT,'X'
   
ENDFOR

END
