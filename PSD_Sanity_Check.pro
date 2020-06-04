
IF $
   ( do_load_check_0 EQ !NULL ) $
THEN BEGIN

   PRINT, 'LOADING run_1701_AE8MAX: '
   PSD_Master_plot, $
      'run1701', $
      psd_1701_AE8MAX_0, time_1701_AE8MAX_0, $
      rr_1701_AE8MAX_0, xmm_1701_AE8MAX_0, $
      AVERAGE = 55, AE8MIN = 0

   psd0_1701_AE8MAX_0 = psd_1701_AE8MAX_0 * 0.
   
   PRINT, 'LOADING run_1701_AE8MIN: '
   PSD_Master_plot, $
      'run1701', $
      psd_1701_AE8MIN_0, time_1701_AE8MIN_0, $
      rr_1701_AE8MIN_0, xmm_1701_AE8MIN_0, $
      AVERAGE = 55, AE8MIN = 1

   psd0_1701_AE8MIN_0 = psd_1701_AE8MIN_0 * 0.
   
   PRINT, 'LOADING run1709: '
   PSD_Master_plot, $
      'run1709', $
      psd_1709_AE8MAX_0, time_1709_AE8MAX_0, $
      rr_1709_AE8MAX_0, xmm_1709_AE8MAX_0, $
      AVERAGE = 55, AE8MIN = 0

   psd0_1709_AE8MAX_0 = psd_1709_AE8MAX_0 * 0.

   FOR $
      iTime = 0, 2160 $
   DO BEGIN

      FOR $
         iMu = 0, 59 $
      DO BEGIN

         psd0_1701_AE8MAX_0( iTime, *, iMu ) = $
            psd_1701_AE8MAX_0( 535, *, iMu ) 
         psd0_1701_AE8MIN_0( iTime, *, iMu ) = $
            psd_1701_AE8MIN_0( 535, *, iMu ) 
         psd0_1709_AE8MAX_0( iTime, *, iMu ) = $
            psd_1709_AE8MAX_0( 535, *, iMu ) 
         
      ENDFOR

   ENDFOR
   
   ratio_AE8MAX_0 = psd_1701_AE8MAX_0 / psd_1709_AE8MAX_0

   do_load_check_0 = !false

ENDIF   

iR = 40
print, 'L-shell: ', rr_1701_AE8MAX_0( iR )

W, 1, 5
WINDOW, 0
LOADCT, 3, /SILENT
IMAGE_CONT, psd_1709_AE8MAX_0( *, *, 37 ), time_1709_AE8MAX_0, rr_1709_AE8MAX_0, $
            TITLE = 'w/o Waves'
MAKELINEY, time_1709_AE8MAX_0( 1260 - 55 ), LINESTYLE = 1
MAKELINEY, time_1709_AE8MAX_0( 1260 + 55 ), LINESTYLE = 1
MAKELINEX, rr_1709_AE8MAX_0( iR ), LINESTYLE = 1

LOADCT, 0, /SILENT
PLOT, time_1709_AE8MAX_0( 1260 - 55 : 1260 + 55 ), $
      psd_1709_AE8MAX_0( 1260 - 55 : 1260 + 55, iR, 37 )
MAKELINEX, MEAN( psd_1709_AE8MAX_0( 1260 - 55 : 1260 + 55, iR, 37 ) ), $
           LINESTYLE = 2

LOADCT, 3, /SILENT
IMAGE_CONT, psd_1701_AE8MAX_0( *, *, 37 ), time_1701_AE8MAX_0, rr_1701_AE8MAX_0, $
            TITLE = 'w/  Waves'
MAKELINEY, time_1701_AE8MAX_0( 1260 - 55 ), LINESTYLE = 1
MAKELINEY, time_1701_AE8MAX_0( 1260 + 55 ), LINESTYLE = 1
MAKELINEX, rr_1701_AE8MAX_0( iR ), LINESTYLE = 1

LOADCT, 0, /SILENT
PLOT, time_1701_AE8MAX_0( 1260 - 55 : 1260 + 55 ), $
      psd_1701_AE8MAX_0( 1260 - 55 : 1260 + 55, iR, 37 )
MAKELINEX, MEAN( psd_1701_AE8MAX_0( 1260 - 55 : 1260 + 55, iR, 37 ) ), $
           LINESTYLE = 2

LOADCT, 70, /SILENT
REVERSECT
IMAGE_CONT, ( ratio_AE8MAX_0( *, *, 37 ) - 1 ) * 100., $
            time_1701_AE8MAX_0, rr_1701_AE8MAX_0, $
            TITLE = '% Diff.', $
            MIN = -100., MAX = 100.

SET_PLOT, 'PS'
DEVICE, FILENAME = 'Images/PSD_Sanity_Check.eps', $
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

fmin = 1e-8
fmax = 1e-5

ny = numy_subplots
LOADCT, 33, /SILENT
IMAGE_CONT, $
   alog10(psd_1709_AE8MAX_0( *, *, 37 )), $
   time_1709_AE8MAX_0, rr_1709_AE8MAX_0, $
   TITLE = 'w/o Waves', $
   XTITLE = 'Simulation Time [ hours ]', $
   YTITLE = TEXTOIDL( 'L [ R_E ]' ), $
   MIN = alog10(fmin), MAX = alog10(fmax), $
   POSITION = $
   [ x_start, $
     y_start + ( ny - 1 ) * y_subplot_size + ( ny - 0 ) * y_space, $
     x_start + x_subplot_size, $
     y_start + ( ny - 0 ) * y_subplot_size + ( ny - 1 ) * y_space ], $
   /NORM, CHARTHICK = 2
TVLCT, 255, 255, 255, 255
MAKELINEY, $
   time_1709_AE8MAX_0( 1260 - 55 ), $
   LINESTYLE = 1, COLOR = 255, THICK = 3
MAKELINEY, $
   time_1709_AE8MAX_0( 1260 + 55 ), $
   LINESTYLE = 1, COLOR = 255, THICK = 3
MAKELINEX, $
   rr_1709_AE8MAX_0( iR ), $
   LINESTYLE = 1, COLOR = 255, THICK = 3
LOADCT, 33, /SILENT
COLORBAR, $
   RANGE = [ fmin, fmax ], $
   DIVISIONS = 3, $
   /VERTICAL, /RIGHT, $
   TITLE = TEXTOIDL( 'PSD [ c / MeV cm ]^3' ), $
   POSITION = $
   [ x_start + x_subplot_size + .025, $
     y_start + ( ny - 1 ) * y_subplot_size + ( ny - 0 ) * y_space, $
     x_start + x_subplot_size + .050, $
     y_start + ( ny - 0 ) * y_subplot_size + ( ny - 1 ) * y_space ], $
   /NORM, $
   TICKNAMES = [ TEXTOIDL( '10^{-8}' ), TEXTOIDL( '10^{-7}' ), $
                 TEXTOIDL( '10^{-6}' ), TEXTOIDL( '10^{-5}' ) ], $
   TICKLEN = -0.25, $
   CHARSIZE = 2, CHARTHICK = 3
ny = ny - 1

LOADCT, 0, /SILENT
PLOT, $
   time_1709_AE8MAX_0( 1260 - 55 : 1260 + 55 ), $
   psd_1709_AE8MAX_0( 1260 - 55 : 1260 + 55, iR, 37 ), $
   XTITLE = 'Simulation Time [ hours ]', $
   YTITLE = TEXTOIDL( 'PSD [ c / MeV cm ]^3' ), $
   POSITION = $
   [ x_start, $
     y_start + ( ny - 1 ) * y_subplot_size + ( ny - 0 ) * y_space, $
     x_start + x_subplot_size, $
     y_start + ( ny - 0 ) * y_subplot_size + ( ny - 1 ) * y_space ], $
   /NORM, CHARTHICK = 2, THICK = 3
MAKELINEX, $
   MEAN( psd_1709_AE8MAX_0( 1260 - 55 : 1260 + 55, iR, 37 ) ), $
   LINESTYLE = 2, THICK = 3
ny = ny - 1

LOADCT, 33, /SILENT
IMAGE_CONT, $
   alog10(psd_1701_AE8MAX_0( *, *, 37 )), $
   time_1701_AE8MAX_0, rr_1701_AE8MAX_0, $
   TITLE = 'w/  Waves', $
   XTITLE = 'Simulation Time [ hours ]', $
   YTITLE = TEXTOIDL( 'L [ R_E ]' ), $
   min = alog10(fmin),max = alog10(fmax), $
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
   time_1701_AE8MAX_0( 1260 - 55 ), $
   LINESTYLE = 1, COLOR = 255, THICK = 3
MAKELINEY, $
   time_1701_AE8MAX_0( 1260 + 55 ), $
   LINESTYLE = 1, COLOR = 255, THICK = 3
MAKELINEX, $
   rr_1701_AE8MAX_0( iR ), $
   LINESTYLE = 1, COLOR = 255, THICK = 3
LOADCT, 33, /SILENT
COLORBAR, $
   RANGE = [ fmin, fmax ], $
   DIVISIONS = 3, $
   /VERTICAL, /RIGHT, $
   TITLE = TEXTOIDL( 'PSD [ c / MeV cm ]^3' ), $
   POSITION = $
   [ x_start + x_subplot_size + .025, $
     y_start + ( ny - 1 ) * y_subplot_size + ( ny - 0 ) * y_space, $
     x_start + x_subplot_size + .050, $
     y_start + ( ny - 0 ) * y_subplot_size + ( ny - 1 ) * y_space ], $
   /NORM, $
   TICKNAMES = [ TEXTOIDL( '10^{-8}' ), TEXTOIDL( '10^{-7}' ), $
                 TEXTOIDL( '10^{-6}' ), TEXTOIDL( '10^{-5}' ) ], $
   TICKLEN = -0.25, $
   CHARSIZE = 2, CHARTHICK = 3
ny = ny - 1

LOADCT, 0, /SILENT
PLOT, $
   time_1701_AE8MAX_0( 1260 - 55 : 1260 + 55 ), $
   psd_1701_AE8MAX_0( 1260 - 55 : 1260 + 55, iR, 37 ), $
   XTITLE = 'Simulation Time [ hours ]', $
   YTITLE = TEXTOIDL( 'PSD [ c / MeV cm ]^3' ), $
   POSITION = $
   [ x_start, $
     y_start + ( ny - 1 ) * y_subplot_size + ( ny - 0 ) * y_space, $
     x_start + x_subplot_size, $
     y_start + ( ny - 0 ) * y_subplot_size + ( ny - 1 ) * y_space ], $
   /NORM, CHARTHICK = 2, THICK = 3
MAKELINEX, $
   MEAN( psd_1701_AE8MAX_0( 1260 - 55 : 1260 + 55, iR, 37 ) ), $
   LINESTYLE = 2, THICK = 3
ny = ny - 1

LOADCT, 70, /SILENT
REVERSECT
IMAGE_CONT, $
   ( ratio_AE8MAX_0( *, *, 37 ) - 1 ) * 100., $
   time_1701_AE8MAX_0, rr_1701_AE8MAX_0, $
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
   rr_1701_AE8MAX_0( iR ), $
   LINESTYLE = 1, COLOR = 000, THICK = 3
PLOT, $
   time_1701_AE8MAX_0, rr_1701_AE8MAX_0, /XS, /YS, $
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
           	STRTRIM( STRING( iMu ), 2 ) + '.eps', $
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
   
   numy_subplots = 3
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
      ( psd_1709_AE8MAX_0( 536 : *, *, iMu ) / $
        psd0_1709_AE8MAX_0( 536 : *, *, iMu ) - 1 ) * 100., $
      time_1709_AE8MAX_0( 536 : * ), rr_1709_AE8MAX_0, $
      TITLE = 'AE8MAX w/o Waves, mu = ' + $
      	STRING( xmm_1709_AE8MAX_0( iMu ) * 100., FORMAT='(I4)') + $
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
      ( psd_1701_AE8MAX_0( 536 : *, *, iMu ) / $
        psd0_1701_AE8MAX_0( 536 : *, *, iMu ) - 1 ) * 100., $
      time_1701_AE8MAX_0( 536 : * ), rr_1701_AE8MAX_0, $
      TITLE = 'AE8MAX w/  Waves, mu = ' + $
      	STRING( xmm_1701_AE8MAX_0( iMu ) * 100., FORMAT='(I4)') + $
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
      ( psd_1701_AE8MIN_0( 536 : *, *, iMu ) / $
        psd0_1701_AE8MIN_0( 536 : *, *, iMu ) - 1 ) * 100., $
      time_1701_AE8MIN_0( 536 : * ), rr_1701_AE8MIN_0, $
      TITLE = 'AE8MIN w/  Waves, mu = ' + $
      	STRING( xmm_1701_AE8MIN_0( iMu ) * 100., FORMAT='(I4)') + $
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
;; TVLCT, 255, 255, 255, 255
;; MAKELINEY, $
;;    time_1709_AE8MAX_0( 1260 - 55 ), $
;;    LINESTYLE = 1, COLOR = 255, THICK = 3
;; MAKELINEY, $
;;    time_1709_AE8MAX_0( 1260 + 55 ), $
;;    LINESTYLE = 1, COLOR = 255, THICK = 3
;; MAKELINEX, $
;;    rr_1709_AE8MAX_0( iR ), $
;;    LINESTYLE = 1, COLOR = 255, THICK = 3
;; LOADCT, 3, /SILENT
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
      ;; TICKNAMES = [ TEXTOIDL( '10^{-10}' ), TEXTOIDL( '10^{-9}' ), $
      ;;               TEXTOIDL( '10^{-8}' ), TEXTOIDL( '10^{-7}' ), $
      ;;               TEXTOIDL( '10^{-6}' ), TEXTOIDL( '10^{-5}' ) ], $
      TICKLEN = -0.25, $
      CHARSIZE = 2, CHARTHICK = 3
   ny = ny - 1
   
   DEVICE, /CLOSE

ENDFOR
   
SET_PLOT, 'X'

dim = SIZE( psd_1701_AE8MAX_0, /DIMENSIONS )
nTime = dim( 0 )
nR = dim( 1 )
nMu = dim( 2 )

psd_Time_average_1701_AE8MAX_0 = FLTARR( nR, nMu )
psd_Time_average_1701_AE8MIN_0 = FLTARR( nR, nMu )
psd_Time_average_1709_AE8MAX_0 = FLTARR( nR, nMu )


FOR $
   iR = 0, nR - 1 $
DO BEGIN

   FOR $
      iMu = 0, nMu - 1 $
   DO BEGIN

      psd_Time_Average_1701_AE8MAX_0( iR, iMu ) = $
         MEAN( PSD_1701_AE8MAX_0( 1980 : 2160, iR, iMu ) )
      psd_Time_Average_1701_AE8MIN_0( iR, iMu ) = $
         MEAN( PSD_1701_AE8MIN_0( 1980 : 2160, iR, iMu ) )
      psd_Time_Average_1709_AE8MAX_0( iR, iMu ) = $
         MEAN( PSD_1709_AE8MAX_0( 1980 : 2160, iR, iMu ) )
      
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
      MAX( CEIL( ALOG10( psd_time_average_1709_AE8MAX_0( 16 : *, iMu ) ) * 1. ) )
   yr_min = yr_max - 3.


    PLOT, rr_1709_AE8MAX_0, psd_time_average_1709_AE8MAX_0( *, iMu ), $
          ;; PLOT, rr_1709_AE8MAX_0, psd_1709_AE8MAX_0( 0, *, iMu ), $
          TITLE = 'AE8MAX w/o  Waves, mu = ' + $
          	STRING( xmm_1701_AE8MIN_0( iMu ) * 100., FORMAT='(I4)') + $
          	' MeV / G', $
          XTITLE = TEXTOIDL( 'L [ R_E ]' ), XMINOR = 8, $
          YTITLE = TEXTOIDL( 'PSD [ c / MeV cm ]^3' ), /YLOG, $
          YRANGE = 10^[ yr_min, yr_max  ], $
          ;; YRANGE = [ 1d-10, 1d0 ], $
          ;; YTITLE = TEXTOIDL( 'PSD [ c / MeV cm ]^3' ), $
          ;; YRANGE = [ 0., MAX( psd_1709_AE8MAX_0( 0, *, iMu ) ) ] * 1.2, $
          PSYM = -7, CHARSIZE = 2, CHARTHICK = 2
   OPLOT, rr_1709_AE8MAX_0, psd_time_average_1701_AE8MAX_0( *, iMu ), $
          PSYM =  6
   OPLOT, rr_1709_AE8MAX_0, psd_time_average_1701_AE8MIN_0( *, iMu ), $
          PSYM =  4
   ;; OPLOT, rr_1709_AE8MAX_0, psd_1709_AE8MAZ_0( 0, *, iMu ), $
          ;; TITLE = 'AE8MAX w/o  Waves, mu = ' + $
          ;; 	STRING( xmm_1701_AE8MIN_0( iMu ) * 100., FORMAT='(I4)') + $
          ;; 	' MeV / G', $
          ;; XTITLE = TEXTOIDL( 'L [ R_E ]' ), XMINOR = 8, $
          ;; YTITLE = TEXTOIDL( 'PSD [ c / MeV cm ]^3' ), /YLOG, $
          ;; ;; YRANGE = [ 1d-10, 1d0 ], $
          ;; ;; YTITLE = TEXTOIDL( 'PSD [ c / MeV cm ]^3' ), $
          ;; YRANGE = [ 0., MAX( psd_1709_AE8MAX_0( 0, *, iMu ) ) ] * 1.2, $
          ;; PSYM = -7, CHARSIZE = 2, CHARTHICK = 2
          ;; PSYM = 4
   ;; OPLOT, rr_1709_AE8MAX_0, psd_time_average_1709_AE8MAX_0( *, iMu ), $
   ;;        PSYM =  6

   ;; OPLOT, rr_1701_AE8MAX_0, psd_1701_AE8MAX_0( 0, *, iMu ), $
          ;; TITLE = 'AE8MAX w/  Waves, mu = ' + $
          ;; 	STRING( xmm_1701_AE8MIN_0( iMu ) * 100., FORMAT='(I4)') + $
          ;; 	' MeV / G', $
          ;; XTITLE = TEXTOIDL( 'L [ R_E ]' ), XMINOR = 8, $
          ;; ;; YTITLE = TEXTOIDL( 'PSD [ c / MeV cm ]^3' ), /YLOG, $
          ;; ;; YRANGE = [ 1d-10, 1d0 ], $
          ;; YTITLE = TEXTOIDL( 'PSD [ c / MeV cm ]^3' ), $
          ;; YRANGE = [ 0, MAX( psd_1709_AE8MAX_0( 0, *, iMu ) ) ] * 1.2, $
          ;; PSYM = -7, CHARSIZE = 2, CHARTHICK = 2
          ;; PSYM = 6
   ;; OPLOT, rr_1701_AE8MAX_0, psd_time_average_1701_AE8MAX_0( *, iMu ), $
   ;;        PSYM =  6
   ;;  PLOT, rr_1701_AE8MAX_0, psd_1701_AE8MAX_0( 0, *, iMu ), $
   ;;        TITLE = 'AE8MAX w/  Waves, mu = ' + $
   ;;        	STRING( xmm_1701_AE8MIN_0( iMu ) * 100., FORMAT='(I4)') + $
   ;;        	' MeV / G', $
   ;;        XTITLE = TEXTOIDL( 'L [ R_E ]' ), XMINOR = 8, $
   ;;        YTITLE = TEXTOIDL( 'PSD [ c / MeV cm ]^3' ), /YLOG, $
   ;;        ;; YRANGE = [ 1d-10, 1d0 ], $
   ;;        ;; YTITLE = TEXTOIDL( 'PSD [ c / MeV cm ]^3' ), $
   ;;        YRANGE = [ 0, MAX( psd_1709_AE8MAX_0( 0, *, iMu ) ) ] * 1.2, $
   ;;        PSYM = -7, CHARSIZE = 2, CHARTHICK = 2
   ;; OPLOT, rr_1701_AE8MAX_0, psd_time_average_1701_AE8MAX_0( *, iMu ), $
   ;;        PSYM =  6

   ;;  PLOT, rr_1701_AE8MIN_0, psd_1701_AE8MIN_0( 0, *, iMu ), $
   ;;        TITLE = 'AE8MIN w/  Waves, mu = ' + $
   ;;        	STRING( xmm_1701_AE8MIN_0( iMu ) * 100., FORMAT='(I4)') + $
   ;;        	' MeV / G', $
   ;;        XTITLE = TEXTOIDL( 'L [ R_E ]' ), XMINOR = 8, $
   ;;        ;; YTITLE = TEXTOIDL( 'PSD [ c / MeV cm ]^3' ), /YLOG, $
   ;;        ;; YRANGE = [ 1d-10, 1d0 ], $
   ;;        YTITLE = TEXTOIDL( 'PSD [ c / MeV cm ]^3' ), $
   ;;        YRANGE = [ 0, MAX( psd_1709_AE8MAX_0( 0, *, iMu ) ) ] * 1.2, $
   ;;        PSYM = -7, CHARSIZE = 2, CHARTHICK = 2
   ;; OPLOT, rr_1701_AE8MIN_0, psd_time_average_1701_AE8MIN_0( *, iMu ), $
   ;;        PSYM =  6
   ;;  PLOT, rr_1701_AE8MIN_0, psd_1701_AE8MIN_0( 0, *, iMu ), $
   ;;        TITLE = 'AE8MIN w/  Waves, mu = ' + $
   ;;        	STRING( xmm_1701_AE8MIN_0( iMu ) * 100., FORMAT='(I4)') + $
   ;;        	' MeV / G', $
   ;;        XTITLE = TEXTOIDL( 'L [ R_E ]' ), XMINOR = 8, $
   ;;        YTITLE = TEXTOIDL( 'PSD [ c / MeV cm ]^3' ), /YLOG, $
   ;;        ;; YRANGE = [ 1d-10, 1d0 ], $
   ;;        ;; YTITLE = TEXTOIDL( 'PSD [ c / MeV cm ]^3' ), $
   ;;        YRANGE = [ 0, MAX( psd_1709_AE8MAX_0( 0, *, iMu ) ) ] * 1.2, $
   ;;        PSYM = -7, CHARSIZE = 2, CHARTHICK = 2
   ;; OPLOT, rr_1701_AE8MIN_0, psd_time_average_1701_AE8MIN_0( *, iMu ), $
   ;;        PSYM =  6

   SET_PLOT, 'PS'
   DEVICE, $
      FILENAME = $
      	'Images/PSD_Line_Plot_mu' + $
      	STRTRIM( STRING( iMu, FORMAT = '(I02)' ), 2 ) + $
      	'.eps', $
      XSIZE = 10., YSIZE = 10., /INCHES, $
      /ENCAPSULATED, /PORTRAIT, /COLOR, BITS_PER_PIXEL = 8
   
   LOADCT, 0, /SILENT
   W, 2, 3
    PLOT, rr_1709_AE8MAX_0, psd_1709_AE8MAX_0( 0, *, iMu ), $
          TITLE = 'AE8MAX w/o  Waves, mu = ' + $
          	STRING( xmm_1701_AE8MIN_0( iMu ) * 100., FORMAT='(I4)') + $
          	' MeV / G', $
          XTITLE = TEXTOIDL( 'L [ R_E ]' ), XMINOR = 8, $
          ;; YTITLE = TEXTOIDL( 'PSD [ c / MeV cm ]^3' ), /YLOG, $
          ;; YRANGE = [ 1d-10, 1d0 ], $
          YTITLE = TEXTOIDL( 'PSD [ c / MeV cm ]^3' ), $
          YRANGE = [ 0., MAX( psd_1709_AE8MAX_0( 0, *, iMu ) ) ] * 1.2, $
          PSYM = -7, CHARSIZE = 2, CHARTHICK = 2
   OPLOT, rr_1709_AE8MAX_0, psd_time_average_1709_AE8MAX_0( *, iMu ), $
          PSYM =  6
    PLOT, rr_1709_AE8MAX_0, psd_1709_AE8MAX_0( 0, *, iMu ), $
          TITLE = 'AE8MAX w/o  Waves, mu = ' + $
          	STRING( xmm_1701_AE8MIN_0( iMu ) * 100., FORMAT='(I4)') + $
          	' MeV / G', $
          XTITLE = TEXTOIDL( 'L [ R_E ]' ), XMINOR = 8, $
          YTITLE = TEXTOIDL( 'PSD [ c / MeV cm ]^3' ), /YLOG, $
          ;; YRANGE = [ 1d-10, 1d0 ], $
          ;; YTITLE = TEXTOIDL( 'PSD [ c / MeV cm ]^3' ), $
          YRANGE = [ 0., MAX( psd_1709_AE8MAX_0( 0, *, iMu ) ) ] * 1.2, $
          PSYM = -7, CHARSIZE = 2, CHARTHICK = 2
   OPLOT, rr_1709_AE8MAX_0, psd_time_average_1709_AE8MAX_0( *, iMu ), $
          PSYM =  6

    PLOT, rr_1701_AE8MAX_0, psd_1701_AE8MAX_0( 0, *, iMu ), $
          TITLE = 'AE8MAX w/  Waves, mu = ' + $
          	STRING( xmm_1701_AE8MIN_0( iMu ) * 100., FORMAT='(I4)') + $
          	' MeV / G', $
          XTITLE = TEXTOIDL( 'L [ R_E ]' ), XMINOR = 8, $
          ;; YTITLE = TEXTOIDL( 'PSD [ c / MeV cm ]^3' ), /YLOG, $
          ;; YRANGE = [ 1d-10, 1d0 ], $
          YTITLE = TEXTOIDL( 'PSD [ c / MeV cm ]^3' ), $
          YRANGE = [ 0, MAX( psd_1709_AE8MAX_0( 0, *, iMu ) ) ] * 1.2, $
          PSYM = -7, CHARSIZE = 2, CHARTHICK = 2
   OPLOT, rr_1701_AE8MAX_0, psd_time_average_1701_AE8MAX_0( *, iMu ), $
          PSYM =  6
    PLOT, rr_1701_AE8MAX_0, psd_1701_AE8MAX_0( 0, *, iMu ), $
          TITLE = 'AE8MAX w/  Waves, mu = ' + $
          	STRING( xmm_1701_AE8MIN_0( iMu ) * 100., FORMAT='(I4)') + $
          	' MeV / G', $
          XTITLE = TEXTOIDL( 'L [ R_E ]' ), XMINOR = 8, $
          YTITLE = TEXTOIDL( 'PSD [ c / MeV cm ]^3' ), /YLOG, $
          ;; YRANGE = [ 1d-10, 1d0 ], $
          ;; YTITLE = TEXTOIDL( 'PSD [ c / MeV cm ]^3' ), $
          YRANGE = [ 0, MAX( psd_1709_AE8MAX_0( 0, *, iMu ) ) ] * 1.2, $
          PSYM = -7, CHARSIZE = 2, CHARTHICK = 2
   OPLOT, rr_1701_AE8MAX_0, psd_time_average_1701_AE8MAX_0( *, iMu ), $
          PSYM =  6

    PLOT, rr_1701_AE8MIN_0, psd_1701_AE8MIN_0( 0, *, iMu ), $
          TITLE = 'AE8MIN w/  Waves, mu = ' + $
          	STRING( xmm_1701_AE8MIN_0( iMu ) * 100., FORMAT='(I4)') + $
          	' MeV / G', $
          XTITLE = TEXTOIDL( 'L [ R_E ]' ), XMINOR = 8, $
          ;; YTITLE = TEXTOIDL( 'PSD [ c / MeV cm ]^3' ), /YLOG, $
          ;; YRANGE = [ 1d-10, 1d0 ], $
          YTITLE = TEXTOIDL( 'PSD [ c / MeV cm ]^3' ), $
          YRANGE = [ 0, MAX( psd_1709_AE8MAX_0( 0, *, iMu ) ) ] * 1.2, $
          PSYM = -7, CHARSIZE = 2, CHARTHICK = 2
   OPLOT, rr_1701_AE8MIN_0, psd_time_average_1701_AE8MIN_0( *, iMu ), $
          PSYM =  6
    PLOT, rr_1701_AE8MIN_0, psd_1701_AE8MIN_0( 0, *, iMu ), $
          TITLE = 'AE8MIN w/  Waves, mu = ' + $
          	STRING( xmm_1701_AE8MIN_0( iMu ) * 100., FORMAT='(I4)') + $
          	' MeV / G', $
          XTITLE = TEXTOIDL( 'L [ R_E ]' ), XMINOR = 8, $
          YTITLE = TEXTOIDL( 'PSD [ c / MeV cm ]^3' ), /YLOG, $
          ;; YRANGE = [ 1d-10, 1d0 ], $
          ;; YTITLE = TEXTOIDL( 'PSD [ c / MeV cm ]^3' ), $
          YRANGE = [ 0, MAX( psd_1709_AE8MAX_0( 0, *, iMu ) ) ] * 1.2, $
          PSYM = -7, CHARSIZE = 2, CHARTHICK = 2
   OPLOT, rr_1701_AE8MIN_0, psd_time_average_1701_AE8MIN_0( *, iMu ), $
          PSYM =  6

   ;; W, 1, 1
   ;; LOADCT, 0, /SILENT

   ;;  PLOT, rr_1709_AE8MAX_0, psd_time_average_1709_AE8MAX_0( *, iMu ), $
   ;;        TITLE = 'AE8MAX w/o  Waves, mu = ' + $
   ;;        	STRING( xmm_1701_AE8MIN_0( iMu ) * 100., FORMAT='(I4)') + $
   ;;        	' MeV / G', $
   ;;        XTITLE = TEXTOIDL( 'L [ R_E ]' ), XMINOR = 8, $
   ;;        YTITLE = TEXTOIDL( 'PSD [ c / MeV cm ]^3' ), /YLOG, $
   ;;        YRANGE = 10^[ yr_min, yr_max  ], $
   ;;        PSYM = -7, CHARSIZE = 2, CHARTHICK = 2
   ;; OPLOT, rr_1709_AE8MAX_0, psd_time_average_1701_AE8MAX_0( *, iMu ), $
   ;;        PSYM =  6
   ;; OPLOT, rr_1709_AE8MAX_0, psd_time_average_1701_AE8MIN_0( *, iMu ), $
   ;;        PSYM =  4
   

   DEVICE, /CLOSE
   ;; READ, test_string

   SET_PLOT,'X'
   
ENDFOR

END
