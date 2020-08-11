; Required files:
;
; @idlrc
; .compile reversect interpol cleanlog PSD_Master_Plot_Lstar

; Load the data for the priority wave and the null hypothesis
; simulations initialized with Lstar
IF $
   ( is_lstar_expanded_data_loaded EQ !NULL ) $
THEN BEGIN

   PRINT, 'LOADING run_1701_DEGELING: '
   PSD_Master_plot, $
      'run1701', $
      psd_1701_LSTAR_DEGELING, time_1701_LSTAR_DEGELING, $
      rr_1701_LSTAR_DEGELING, xmm_1701_LSTAR_DEGELING, $
      NTIME_AVERAGE = 200, AE8MIN = 0, DEGELING = 1, $
      PSD_TIME_AVERAGE = psd_Time_Average_1701_LSTAR_DEGELING, $
      PSD_NORMALIZED = psd_normalized_1701_LSTAR_DEGELING

   dim_1701_LSTAR_DEGELING = $
      SIZE( psd_1701_LSTAR_DEGELING, /DIMENSIONS)
   psd0_1701_LSTAR_DEGELING = $
      FLTARR( dim_1701_LSTAR_DEGELING )
   
   PRINT, 'LOADING run_1701_AE8MAX: '
   PSD_Master_plot, $
      'run1701', $
      psd_1701_LSTAR_AE8MAX, time_1701_LSTAR_AE8MAX, $
      rr_1701_LSTAR_AE8MAX, xmm_1701_LSTAR_AE8MAX, $
      NTIME_AVERAGE = 200, AE8MIN = 0, $
      PSD_TIME_AVERAGE = psd_Time_Average_1701_LSTAR_AE8MAX, $
      PSD_NORMALIZED = psd_normalized_1701_LSTAR_AE8MAX

   dim_1701_LSTAR_AE8MAX = $
      SIZE( psd_1701_LSTAR_AE8MAX, /DIMENSIONS)
   psd0_1701_LSTAR_AE8MAX = $
      FLTARR( dim_1701_LSTAR_AE8MAX )
   
   PRINT, 'LOADING run_1701_AE8MIN: '
   PSD_Master_plot, $
      'run1701', $
      psd_1701_LSTAR_AE8MIN, time_1701_LSTAR_AE8MIN, $
      rr_1701_LSTAR_AE8MIN, xmm_1701_LSTAR_AE8MIN, $
      NTIME_AVERAGE = 200, AE8MIN = 1, $
      PSD_TIME_AVERAGE = psd_Time_Average_1701_LSTAR_AE8MIN, $
      PSD_NORMALIZED = psd_normalized_1701_LSTAR_AE8MIN

   dim_1701_LSTAR_AE8MIN = $
      SIZE( psd_1701_LSTAR_AE8MIN, /DIMENSIONS)
   psd0_1701_LSTAR_AE8MIN = $
      FLTARR( dim_1701_LSTAR_AE8MIN )

   PRINT, 'LOADING run_1709_DEGELING: '
   PSD_Master_plot, $
      'run1709', $
      psd_1709_LSTAR_DEGELING, time_1709_LSTAR_DEGELING, $
      rr_1709_LSTAR_DEGELING, xmm_1709_LSTAR_DEGELING, $
      NTIME_AVERAGE = 200, AE8MIN = 0, DEGELING = 1, $
      PSD_TIME_AVERAGE = psd_Time_Average_1709_LSTAR_DEGELING, $
      PSD_NORMALIZED = psd_normalized_1709_LSTAR_DEGELING

   dim_1709_LSTAR_DEGELING = $
      SIZE( psd_1709_LSTAR_DEGELING, /DIMENSIONS)
   psd0_1709_LSTAR_DEGELING = $
      FLTARR( dim_1709_LSTAR_DEGELING )
   
   PRINT, 'LOADING run_1709_AE8MAX: '
   PSD_Master_plot, $
      'run1709', $
      psd_1709_LSTAR_AE8MAX, time_1709_LSTAR_AE8MAX, $
      rr_1709_LSTAR_AE8MAX, xmm_1709_LSTAR_AE8MAX, $
      NTIME_AVERAGE = 200, AE8MIN = 0, $
      PSD_TIME_AVERAGE = psd_Time_Average_1709_LSTAR_AE8MAX, $
      PSD_NORMALIZED = psd_normalized_1709_LSTAR_AE8MAX

   dim_1709_LSTAR_AE8MAX = $
      SIZE( psd_1709_LSTAR_AE8MAX, /DIMENSIONS)
   psd0_1709_LSTAR_AE8MAX = $
      FLTARR( dim_1709_LSTAR_AE8MAX )
   
   PRINT, 'LOADING run_1709_AE8MIN: '
   PSD_Master_plot, $
      'run1709', $
      psd_1709_LSTAR_AE8MIN, time_1709_LSTAR_AE8MIN, $
      rr_1709_LSTAR_AE8MIN, xmm_1709_LSTAR_AE8MIN, $
      NTIME_AVERAGE = 200, AE8MIN = 1, $
      PSD_TIME_AVERAGE = psd_Time_Average_1709_LSTAR_AE8MIN, $
      PSD_NORMALIZED = psd_normalized_1709_LSTAR_AE8MIN

   dim_1709_LSTAR_AE8MIN = $
      SIZE( psd_1709_LSTAR_AE8MIN, /DIMENSIONS)
   psd0_1709_LSTAR_AE8MIN = $
      FLTARR( dim_1709_LSTAR_AE8MIN )
   
   FOR $
      iTime = 0, 2160 $
   DO BEGIN

      psd0_1701_LSTAR_DEGELING(iTime, *, *, * )= $
         psd_1701_LSTAR_DEGELING( 535, *, *, *) 
      psd0_1701_LSTAR_AE8MAX(iTime, *, *, * )= $
         psd_1701_LSTAR_AE8MAX( 535, *, *, *) 
      psd0_1701_LSTAR_AE8MIN(iTime, *, *, * )= $
         psd_1701_LSTAR_AE8MIN( 535, *, *, * ) 
   
      psd0_1709_LSTAR_DEGELING(iTime, *, *, * )= $
         psd_1709_LSTAR_DEGELING( 535, *, *, * ) 
      psd0_1709_LSTAR_AE8MAX(iTime, *, *, * )= $
         psd_1709_LSTAR_AE8MAX( 535, *, *, * ) 
      psd0_1709_LSTAR_AE8MIN(iTime, *, *, * )= $
         psd_1709_LSTAR_AE8MIN( 535, *, *, * ) 

   ENDFOR
   
   ratio_1701_LSTAR_DEGELING = $
      psd_1701_LSTAR_DEGELING / psd_1709_LSTAR_DEGELING
   ratio_1701_LSTAR_AE8MAX = $
      psd_1701_LSTAR_AE8MAX / psd_1709_LSTAR_AE8MAX
   ratio_1701_LSTAR_AE8MIN = $
      psd_1701_LSTAR_AE8MIN / psd_1709_LSTAR_AE8MIN

   is_lstar_expanded_data_loaded = !TRUE

   SAVE, $
      psd_1701_LSTAR_DEGELING, time_1701_LSTAR_DEGELING, $
      rr_1701_LSTAR_DEGELING, xmm_1701_LSTAR_DEGELING, $
      dim_1701_LSTAR_DEGELING, psd0_1701_LSTAR_DEGELING, $
      psd_1701_LSTAR_AE8MAX, time_1701_LSTAR_AE8MAX, $
      rr_1701_LSTAR_AE8MAX, xmm_1701_LSTAR_AE8MAX, $
      dim_1701_LSTAR_AE8MAX, psd0_1701_LSTAR_AE8MAX, $
      psd_1701_LSTAR_AE8MIN, time_1701_LSTAR_AE8MIN, $
      rr_1701_LSTAR_AE8MIN, xmm_1701_LSTAR_AE8MIN, $
      dim_1701_LSTAR_AE8MIN, psd0_1701_LSTAR_AE8MIN, $
      psd_1709_LSTAR_DEGELING, time_1709_LSTAR_DEGELING, $
      rr_1709_LSTAR_DEGELING, xmm_1709_LSTAR_DEGELING, $
      dim_1709_LSTAR_DEGELING, psd0_1709_LSTAR_DEGELING, $
      psd_1709_LSTAR_AE8MAX, time_1709_LSTAR_AE8MAX, $
      rr_1709_LSTAR_AE8MAX, xmm_1709_LSTAR_AE8MAX, $
      dim_1709_LSTAR_AE8MAX, psd0_1709_LSTAR_AE8MAX, $
      psd_1709_LSTAR_AE8MIN, time_1709_LSTAR_AE8MIN, $
      rr_1709_LSTAR_AE8MIN, xmm_1709_LSTAR_AE8MIN, $
      dim_1709_LSTAR_AE8MIN, psd0_1709_LSTAR_AE8MIN, $
      is_lstar_expanded_data_loaded, $
      ratio_1701_LSTAR_DEGELING, $
      ratio_1701_LSTAR_AE8MAX, $
      ratio_1701_LSTAR_AE8MIN, $
      FILENAME = "PSD_Master_Plot_Data.sav"

ENDIF

PRINT, 'FINISHED LOAD'
STOP

mu_minloc = MIN( WHERE( xmm_1701_LSTAR_AE8MAX * 100. GE 3e2 ) )
mu_maxloc = MAX( WHERE( xmm_1701_LSTAR_AE8MAX * 100. LE 1e4 ) )

; ####################################################################
; ####################################################################
; ####################################################################
; ####################################################################
; ####################################################################

SET_PLOT, 'X'
iR = 40
PRINT, '1701 L-shell DEGELING: ', rr_1701_LSTAR_DEGELING( iR )
PRINT, '1709 L-shell DEGELING: ', rr_1709_LSTAR_DEGELING( iR )

W, 1, 5
WINDOW, 0, TITLE = 'DEGELING Comparison', $
        XSIZE = 1000, YSIZE = 1000
LOADCT, 3, /SILENT
IMAGE_CONT, $
   psd_1709_LSTAR_DEGELING( *, *, 0, 91 ), $
   time_1709_LSTAR_DEGELING, rr_1709_LSTAR_DEGELING, $
   TITLE = 'DEGELING w/o Waves'
MAKELINEY, $
   time_1709_LSTAR_DEGELING( 1260 - 55 ), LINESTYLE = 1
MAKELINEY, $
   time_1709_LSTAR_DEGELING( 1260 + 55 ), LINESTYLE = 1
MAKELINEX, $
   rr_1709_LSTAR_DEGELING( iR ), LINESTYLE = 1

LOADCT, 0, /SILENT
PLOT, $
   time_1709_LSTAR_DEGELING( 1260 - 55 : 1260 + 55 ), $
   psd_1709_LSTAR_DEGELING( 1260 - 55 : 1260 + 55, iR, 0, 91 )
MAKELINEX, $
   MEAN( psd_1709_LSTAR_DEGELING( 1260 - 55 : 1260 + 55, iR, 0, 91 ) ), $
   LINESTYLE = 2

LOADCT, 3, /SILENT
IMAGE_CONT, $
   psd_1701_LSTAR_DEGELING( *, *, 0, 91 ), $
   time_1701_LSTAR_DEGELING, rr_1701_LSTAR_DEGELING, $
   TITLE = 'DEGELING w/  Waves'
MAKELINEY, $
   time_1701_LSTAR_DEGELING( 1260 - 55 ), LINESTYLE = 1
MAKELINEY, $
   time_1701_LSTAR_DEGELING( 1260 + 55 ), LINESTYLE = 1
MAKELINEX, $
   rr_1701_LSTAR_DEGELING( iR ), LINESTYLE = 1

LOADCT, 0, /SILENT
PLOT, $
   time_1701_LSTAR_DEGELING( 1260 - 55 : 1260 + 55 ), $
   psd_1701_LSTAR_DEGELING( 1260 - 55 : 1260 + 55, iR, 0, 91 )
MAKELINEX, $
   MEAN( psd_1701_LSTAR_DEGELING( 1260 - 55 : 1260 + 55, iR, 0, 91 ) ), $
   LINESTYLE = 2

LOADCT, 70, /SILENT
REVERSECT
IMAGE_CONT, $
   ( ratio_1701_LSTAR_DEGELING( *, *, 0, 91 ) - 1 ) * 100., $
   time_1701_LSTAR, rr_1701_LSTAR, $
   TITLE = '% Diff.', $
   MIN = -100., MAX = 100.
PRINT, 'FINISHED 1701 DEGELING COMPARISON PLOTS'
STOP

iR = 40
PRINT, '1701 L-shell AE8MAX: ', rr_1701_LSTAR_AE8MAX( iR )
PRINT, '1709 L-shell AE8MAX: ', rr_1709_LSTAR_AE8MAX( iR )

W, 1, 5
WINDOW, 1, TITLE = 'AE8MAX Comparison', $
        XSIZE = 1000, YSIZE = 1000
LOADCT, 3, /SILENT
IMAGE_CONT, $
   psd_1709_LSTAR_AE8MAX( *, *, 0, 91 ), $
   time_1709_LSTAR_AE8MAX, rr_1709_LSTAR_AE8MAX, $
   TITLE = 'AE8MAX w/o Waves'
MAKELINEY, $
   time_1709_LSTAR_AE8MAX( 1260 - 55 ), LINESTYLE = 1
MAKELINEY, $
   time_1709_LSTAR_AE8MAX( 1260 + 55 ), LINESTYLE = 1
MAKELINEX, $
   rr_1709_LSTAR_AE8MAX( iR ), LINESTYLE = 1

LOADCT, 0, /SILENT
PLOT, $
   time_1709_LSTAR_AE8MAX( 1260 - 55 : 1260 + 55 ), $
   psd_1709_LSTAR_AE8MAX( 1260 - 55 : 1260 + 55, iR, 0, 91 )
MAKELINEX, $
   MEAN( psd_1709_LSTAR_AE8MAX( 1260 - 55 : 1260 + 55, iR, 0, 91 ) ), $
   LINESTYLE = 2

LOADCT, 3, /SILENT
IMAGE_CONT, $
   psd_1701_LSTAR_AE8MAX( *, *, 0, 91 ), $
   time_1701_LSTAR_AE8MAX, rr_1701_LSTAR_AE8MAX, $
   TITLE = 'AE8MAX w/  Waves'
MAKELINEY, $
   time_1701_LSTAR_AE8MAX( 1260 - 55 ), LINESTYLE = 1
MAKELINEY, $
   time_1701_LSTAR_AE8MAX( 1260 + 55 ), LINESTYLE = 1
MAKELINEX, $
   rr_1701_LSTAR_AE8MAX( iR ), LINESTYLE = 1

LOADCT, 0, /SILENT
PLOT, $
   time_1701_LSTAR_AE8MAX( 1260 - 55 : 1260 + 55 ), $
   psd_1701_LSTAR_AE8MAX( 1260 - 55 : 1260 + 55, iR, 0, 91 )
MAKELINEX, $
   MEAN( psd_1701_LSTAR_AE8MAX( 1260 - 55 : 1260 + 55, iR, 0, 91 ) ), $
   LINESTYLE = 2

LOADCT, 70, /SILENT
REVERSECT
IMAGE_CONT, $
   ( ratio_1701_LSTAR_AE8MAX( *, *, 0, 91 ) - 1 ) * 100., $
   time_1701_LSTAR, rr_1701_LSTAR, $
   TITLE = '% Diff.', $
   MIN = -100., MAX = 100.
PRINT, 'FINISHED 1701 AE8MAX COMPARISON PLOTS'
STOP

; ####################################################################
; ####################################################################
; ####################################################################
; ####################################################################
; ####################################################################

iR = 40
PRINT, '1701 L-shell AE8MIN: ', rr_1701_LSTAR_AE8MIN( iR )
PRINT, '1709 L-shell AE8MIN: ', rr_1709_LSTAR_AE8MIN( iR )

W, 1, 5
WINDOW, 2, TITLE = 'AE8MIN Comparison', $
        XSIZE = 1000, YSIZE = 1000
LOADCT, 3, /SILENT
IMAGE_CONT, $
   psd_1709_LSTAR_AE8MIN( *, *, 0, 91 ), $
   time_1709_LSTAR_AE8MIN, rr_1709_LSTAR_AE8MIN, $
   TITLE = 'w/o Waves'
MAKELINEY, $
   time_1709_LSTAR_AE8MIN( 1260 - 55 ), LINESTYLE = 1
MAKELINEY, $
   time_1709_LSTAR_AE8MIN( 1260 + 55 ), LINESTYLE = 1
MAKELINEX, $
   rr_1709_LSTAR_AE8MIN( iR ), LINESTYLE = 1

LOADCT, 0, /SILENT
PLOT, $
   time_1709_LSTAR_AE8MIN( 1260 - 55 : 1260 + 55 ), $
   psd_1709_LSTAR_AE8MIN( 1260 - 55 : 1260 + 55, iR, 0, 91 )
MAKELINEX, $
   MEAN( psd_1709_LSTAR_AE8MIN( 1260 - 55 : 1260 + 55, iR, 0, 91 ) ), $
   LINESTYLE = 2

LOADCT, 3, /SILENT
IMAGE_CONT, $
   psd_1701_LSTAR_AE8MIN( *, *, 0, 91 ), $
   time_1701_LSTAR_AE8MIN, rr_1701_LSTAR_AE8MIN, $
   TITLE = 'w/  Waves'
MAKELINEY, $
   time_1701_LSTAR_AE8MIN( 1260 - 55 ), LINESTYLE = 1
MAKELINEY, $
   time_1701_LSTAR_AE8MIN( 1260 + 55 ), LINESTYLE = 1
MAKELINEX, $
   rr_1701_LSTAR_AE8MIN( iR ), LINESTYLE = 1

LOADCT, 0, /SILENT
PLOT, $
   time_1701_LSTAR_AE8MIN( 1260 - 55 : 1260 + 55 ), $
   psd_1701_LSTAR_AE8MIN( 1260 - 55 : 1260 + 55, iR, 0, 91 )
MAKELINEX, $
   MEAN( psd_1701_LSTAR_AE8MIN( 1260 - 55 : 1260 + 55, iR, 0, 91 ) ), $
   LINESTYLE = 2

LOADCT, 70, /SILENT
REVERSECT
IMAGE_CONT, $
   ( ratio_1701_LSTAR_AE8MIN( *, *, 0, 91 ) - 1 ) * 100., $
   time_1701_LSTAR, rr_1701_LSTAR, $
   TITLE = '% Diff.', $
   MIN = -100., MAX = 100.
PRINT, 'FINISHED 1701 AE8MIN COMPARISON PLOTS'
STOP                            ; PLOT TO SCREEN AE8MIN COMPARISON

; ####################################################################
; ####################################################################
; ####################################################################
; ####################################################################
; ####################################################################

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

fmin_DEGELING = 10^(-09.5)
fmax_DEGELING = 10^(-05.5)

fmin_AE8MAX = 10^(-09.5)
fmax_AE8MAX = 10^(-05.5)

fmin_AE8MIN = 10^(-09.5)
fmax_AE8MIN = 10^(-05.5)

ticknames_DEGELING = $
   ;; [ TEXTOIDL( '10^{-10.4}' ), TEXTOIDL( '10^{-9.4}' ), $
   ;;   TEXTOIDL( '10^{-8.4}' ),  TEXTOIDL( '10^{-7.4}' ), $
   [ TEXTOIDL( '10^{-9.5}' ), $
     TEXTOIDL( '10^{-8.5}' ),  TEXTOIDL( '10^{-7.5}' ), $
     TEXTOIDL( '10^{-6.5}' ),  TEXTOIDL( '10^{-5.5}' ) ]
ticknames_AE8MAX = $
   ;; [ TEXTOIDL( '10^{-10.4}' ), TEXTOIDL( '10^{-9.4}' ), $
   ;;   TEXTOIDL( '10^{-8.4}' ),  TEXTOIDL( '10^{-7.4}' ), $
   [ TEXTOIDL( '10^{-9.5}' ), $
     TEXTOIDL( '10^{-8.5}' ),  TEXTOIDL( '10^{-7.5}' ), $
     TEXTOIDL( '10^{-6.5}' ),  TEXTOIDL( '10^{-5.5}' ) ]
ticknames_AE8MIN = $
   [ TEXTOIDL( '10^{-9.5}' ), $
     TEXTOIDL( '10^{-8.5}' ),  TEXTOIDL( '10^{-7.5}' ), $
     TEXTOIDL( '10^{-6.5}' ),  TEXTOIDL( '10^{-5.5}' ) ]

; ####################################################################
; ####################################################################
; ####################################################################
; ####################################################################
; ####################################################################

SET_PLOT, 'PS'
DEVICE, FILENAME = 'Images/Lstar_Expanded/' + $
        'PSD_Sanity_Check_1701_LSTAR_DEGELING_expanded.eps', $
        XSIZE = 10., YSIZE = 10., /INCHES, $
        /PORTRAIT, /COLOR, /ENCAPSULATED, BITS_PER_PIXEL = 8

ny = numy_subplots
LOADCT, 3, /SILENT
IMAGE_CONT, $
   psd_1709_LSTAR_DEGELING( *, *, 0, 91 ), $
   time_1709_LSTAR_DEGELING, rr_1709_LSTAR_DEGELING, $
   TITLE = 'DEGELING w/o Waves', $
   XTITLE = 'Simulation Time [ hours ]', $
   YTITLE = TEXTOIDL( 'L [ R_E ]' ), $
   MIN = fmin_DEGELING, MAX = fmax_DEGELING, $
   POSITION = $
   [ x_start, $
     y_start + ( ny - 1 ) * y_subplot_size + ( ny - 0 ) * y_space, $
     x_start + x_subplot_size, $
     y_start + ( ny - 0 ) * y_subplot_size + ( ny - 1 ) * y_space ], $
   /NORM, CHARTHICK = 2
TVLCT, 255, 255, 255, 255
MAKELINEY, $
   time_1709_LSTAR_DEGELING( 1260 - 55 ), $
   LINESTYLE = 1, COLOR = 255, THICK = 3
MAKELINEY, $
   time_1709_LSTAR_DEGELING( 1260 + 55 ), $
   LINESTYLE = 1, COLOR = 255, THICK = 3
MAKELINEX, $
   rr_1709_LSTAR_DEGELING( iR ), $
   LINESTYLE = 1, COLOR = 255, THICK = 3
LOADCT, 3, /SILENT
COLORBAR, $
   RANGE = [ fmin_DEGELING, fmax_DEGELING ], $
   DIVISIONS = N_ELEMENTS( ticknames_DEGELING ) - 1, $
   /VERTICAL, /RIGHT, $
   TITLE = TEXTOIDL( 'PSD [ c / MeV cm ]^3' ), $
   POSITION = $
   [ x_start + x_subplot_size + .025, $
     y_start + ( ny - 1 ) * y_subplot_size + ( ny - 0 ) * y_space, $
     x_start + x_subplot_size + .050, $
     y_start + ( ny - 0 ) * y_subplot_size + ( ny - 1 ) * y_space ], $
   /NORM, $
   TICKNAMES = ticknames_DEGELING, $
   TICKLEN = -0.25, $
   CHARSIZE = 2, CHARTHICK = 3
ny = ny - 1

LOADCT, 0, /SILENT
PLOT, $
   time_1709_LSTAR_DEGELING( 1260 - 55 : 1260 + 55 ), $
   psd_1709_LSTAR_DEGELING( 1260 - 55 : 1260 + 55, iR, 0, 91 ), $
   XTITLE = 'Simulation Time [ hours ]', $
   YTITLE = TEXTOIDL( 'PSD [ c / MeV cm ]^3' ), $
   POSITION = $
   [ x_start, $
     y_start + ( ny - 1 ) * y_subplot_size + ( ny - 0 ) * y_space, $
     x_start + x_subplot_size, $
     y_start + ( ny - 0 ) * y_subplot_size + ( ny - 1 ) * y_space ], $
   /NORM, CHARTHICK = 2, THICK = 3
MAKELINEX, $
   MEAN( psd_1709_LSTAR_DEGELING( 1260 - 55 : 1260 + 55, iR, 0, 91 ) ), $
   LINESTYLE = 2, THICK = 3
ny = ny - 1

LOADCT, 3, /SILENT
IMAGE_CONT, $
   psd_1701_LSTAR_DEGELING( *, *, 0, 91 ), $
   time_1701_LSTAR_DEGELING, rr_1701_LSTAR_DEGELING, $
   TITLE = 'DEGELING w/  Waves', $
   XTITLE = 'Simulation Time [ hours ]', $
   YTITLE = TEXTOIDL( 'L [ R_E ]' ), $
   min = fmin_DEGELING,max = fmax_DEGELING, $
   POSITION = $
   [ x_start, $
     y_start + ( ny - 1 ) * y_subplot_size + ( ny - 0 ) * y_space, $
     x_start + x_subplot_size, $
     y_start + ( ny - 0 ) * y_subplot_size + ( ny - 1 ) * y_space ], $
   /NORM, CHARTHICK = 2
TVLCT, 255, 255, 255, 255
MAKELINEY, $
   time_1701_LSTAR_DEGELING( 1260 - 55 ), $
   LINESTYLE = 1, COLOR = 255, THICK = 3
MAKELINEY, $
   time_1701_LSTAR_DEGELING( 1260 + 55 ), $
   LINESTYLE = 1, COLOR = 255, THICK = 3
MAKELINEX, $
   rr_1701_LSTAR_DEGELING( iR ), $
   LINESTYLE = 1, COLOR = 255, THICK = 3
LOADCT, 3, /SILENT
COLORBAR, $
   RANGE = [ fmin_DEGELING, fmax_DEGELING ], $
   DIVISIONS = N_ELEMENTS( ticknames_DEGELING ) - 1, $
   /VERTICAL, /RIGHT, $
   TITLE = TEXTOIDL( 'PSD [ c / MeV cm ]^3' ), $
   POSITION = $
   [ x_start + x_subplot_size + .025, $
     y_start + ( ny - 1 ) * y_subplot_size + ( ny - 0 ) * y_space, $
     x_start + x_subplot_size + .050, $
     y_start + ( ny - 0 ) * y_subplot_size + ( ny - 1 ) * y_space ], $
   /NORM, $
   TICKNAMES = ticknames_DEGELING, $
   TICKLEN = -0.25, $
   CHARSIZE = 2, CHARTHICK = 3
ny = ny - 1

LOADCT, 0, /SILENT
PLOT, $
   time_1701_LSTAR_DEGELING( 1260 - 55 : 1260 + 55 ), $
   psd_1701_LSTAR_DEGELING( 1260 - 55 : 1260 + 55, iR, 0, 91 ), $
   XTITLE = 'Simulation Time [ hours ]', $
   YTITLE = TEXTOIDL( 'PSD [ c / MeV cm ]^3' ), $
   POSITION = $
   [ x_start, $
     y_start + ( ny - 1 ) * y_subplot_size + ( ny - 0 ) * y_space, $
     x_start + x_subplot_size, $
     y_start + ( ny - 0 ) * y_subplot_size + ( ny - 1 ) * y_space ], $
   /NORM, CHARTHICK = 2, THICK = 3
MAKELINEX, $
   MEAN( psd_1701_LSTAR_DEGELING( 1260 - 55 : 1260 + 55, iR, 0, 91 ) ), $
   LINESTYLE = 2, THICK = 3
ny = ny - 1

LOADCT, 70, /SILENT
REVERSECT
IMAGE_CONT, $
   ( ratio_1701_LSTAR_DEGELING( *, *, 0, 91 ) - 1 ) * 100., $
   time_1701_LSTAR_DEGELING, rr_1701_LSTAR_DEGELING, $
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
   rr_1701_LSTAR_DEGELING( iR ), $
   LINESTYLE = 1, COLOR = 000, THICK = 3
PLOT, $
   time_1701_LSTAR_DEGELING, rr_1701_LSTAR_DEGELING, /XS, /YS, $
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
PRINT, 'FINISHED OUTPUT OF 1701 DEGELING .EPS PLOT'
STOP                            ; OUTPUT DEGELING .EPS PLOT

DEVICE, FILENAME = 'Images/Lstar_Expanded/' + $
        'PSD_Sanity_Check_1701_LSTAR_AE8MAX_expanded.eps', $
        XSIZE = 10., YSIZE = 10., /INCHES, $
        /PORTRAIT, /COLOR, /ENCAPSULATED, BITS_PER_PIXEL = 8

ny = numy_subplots
LOADCT, 3, /SILENT
IMAGE_CONT, $
   psd_1709_LSTAR_AE8MAX( *, *, 0, 91 ), $
   time_1709_LSTAR_AE8MAX, rr_1709_LSTAR_AE8MAX, $
   TITLE = 'AE8MAX w/o Waves', $
   XTITLE = 'Simulation Time [ hours ]', $
   YTITLE = TEXTOIDL( 'L [ R_E ]' ), $
   MIN = fmin_AE8MAX, MAX = fmax_AE8MAX, $
   POSITION = $
   [ x_start, $
     y_start + ( ny - 1 ) * y_subplot_size + ( ny - 0 ) * y_space, $
     x_start + x_subplot_size, $
     y_start + ( ny - 0 ) * y_subplot_size + ( ny - 1 ) * y_space ], $
   /NORM, CHARTHICK = 2
TVLCT, 255, 255, 255, 255
MAKELINEY, $
   time_1709_LSTAR_AE8MAX( 1260 - 55 ), $
   LINESTYLE = 1, COLOR = 255, THICK = 3
MAKELINEY, $
   time_1709_LSTAR_AE8MAX( 1260 + 55 ), $
   LINESTYLE = 1, COLOR = 255, THICK = 3
MAKELINEX, $
   rr_1709_LSTAR_AE8MAX( iR ), $
   LINESTYLE = 1, COLOR = 255, THICK = 3
LOADCT, 3, /SILENT
COLORBAR, $
   RANGE = [ fmin_AE8MAX, fmax_AE8MAX ], $
   DIVISIONS = N_ELEMENTS( ticknames_AE8MAX ) - 1, $
   /VERTICAL, /RIGHT, $
   TITLE = TEXTOIDL( 'PSD [ c / MeV cm ]^3' ), $
   POSITION = $
   [ x_start + x_subplot_size + .025, $
     y_start + ( ny - 1 ) * y_subplot_size + ( ny - 0 ) * y_space, $
     x_start + x_subplot_size + .050, $
     y_start + ( ny - 0 ) * y_subplot_size + ( ny - 1 ) * y_space ], $
   /NORM, $
   TICKNAMES = ticknames_AE8MAX, $
   TICKLEN = -0.25, $
   CHARSIZE = 2, CHARTHICK = 3
ny = ny - 1

LOADCT, 0, /SILENT
PLOT, $
   time_1709_LSTAR_AE8MAX( 1260 - 55 : 1260 + 55 ), $
   psd_1709_LSTAR_AE8MAX( 1260 - 55 : 1260 + 55, iR, 0, 91 ), $
   XTITLE = 'Simulation Time [ hours ]', $
   YTITLE = TEXTOIDL( 'PSD [ c / MeV cm ]^3' ), $
   POSITION = $
   [ x_start, $
     y_start + ( ny - 1 ) * y_subplot_size + ( ny - 0 ) * y_space, $
     x_start + x_subplot_size, $
     y_start + ( ny - 0 ) * y_subplot_size + ( ny - 1 ) * y_space ], $
   /NORM, CHARTHICK = 2, THICK = 3
MAKELINEX, $
   MEAN( psd_1709_LSTAR_AE8MAX( 1260 - 55 : 1260 + 55, iR, 0, 91 ) ), $
   LINESTYLE = 2, THICK = 3
ny = ny - 1

LOADCT, 3, /SILENT
IMAGE_CONT, $
   psd_1701_LSTAR_AE8MAX( *, *, 0, 91 ), $
   time_1701_LSTAR_AE8MAX, rr_1701_LSTAR_AE8MAX, $
   TITLE = 'AE8MAX w/  Waves', $
   XTITLE = 'Simulation Time [ hours ]', $
   YTITLE = TEXTOIDL( 'L [ R_E ]' ), $
   min = fmin_AE8MAX,max = fmax_AE8MAX, $
   POSITION = $
   [ x_start, $
     y_start + ( ny - 1 ) * y_subplot_size + ( ny - 0 ) * y_space, $
     x_start + x_subplot_size, $
     y_start + ( ny - 0 ) * y_subplot_size + ( ny - 1 ) * y_space ], $
   /NORM, CHARTHICK = 2
TVLCT, 255, 255, 255, 255
MAKELINEY, $
   time_1701_LSTAR_AE8MAX( 1260 - 55 ), $
   LINESTYLE = 1, COLOR = 255, THICK = 3
MAKELINEY, $
   time_1701_LSTAR_AE8MAX( 1260 + 55 ), $
   LINESTYLE = 1, COLOR = 255, THICK = 3
MAKELINEX, $
   rr_1701_LSTAR_AE8MAX( iR ), $
   LINESTYLE = 1, COLOR = 255, THICK = 3
LOADCT, 3, /SILENT
COLORBAR, $
   RANGE = [ fmin_AE8MAX, fmax_AE8MAX ], $
   DIVISIONS = N_ELEMENTS( ticknames_AE8MAX ) - 1, $
   /VERTICAL, /RIGHT, $
   TITLE = TEXTOIDL( 'PSD [ c / MeV cm ]^3' ), $
   POSITION = $
   [ x_start + x_subplot_size + .025, $
     y_start + ( ny - 1 ) * y_subplot_size + ( ny - 0 ) * y_space, $
     x_start + x_subplot_size + .050, $
     y_start + ( ny - 0 ) * y_subplot_size + ( ny - 1 ) * y_space ], $
   /NORM, $
   TICKNAMES = ticknames_AE8MAX, $
   TICKLEN = -0.25, $
   CHARSIZE = 2, CHARTHICK = 3
ny = ny - 1

LOADCT, 0, /SILENT
PLOT, $
   time_1701_LSTAR_AE8MAX( 1260 - 55 : 1260 + 55 ), $
   psd_1701_LSTAR_AE8MAX( 1260 - 55 : 1260 + 55, iR, 0, 91 ), $
   XTITLE = 'Simulation Time [ hours ]', $
   YTITLE = TEXTOIDL( 'PSD [ c / MeV cm ]^3' ), $
   POSITION = $
   [ x_start, $
     y_start + ( ny - 1 ) * y_subplot_size + ( ny - 0 ) * y_space, $
     x_start + x_subplot_size, $
     y_start + ( ny - 0 ) * y_subplot_size + ( ny - 1 ) * y_space ], $
   /NORM, CHARTHICK = 2, THICK = 3
MAKELINEX, $
   MEAN( psd_1701_LSTAR_AE8MAX( 1260 - 55 : 1260 + 55, iR, 0, 91 ) ), $
   LINESTYLE = 2, THICK = 3
ny = ny - 1

LOADCT, 70, /SILENT
REVERSECT
IMAGE_CONT, $
   ( ratio_1701_LSTAR_AE8MAX( *, *, 0, 91 ) - 1 ) * 100., $
   time_1701_LSTAR_AE8MAX, rr_1701_LSTAR_AE8MAX, $
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
   rr_1701_LSTAR_AE8MAX( iR ), $
   LINESTYLE = 1, COLOR = 000, THICK = 3
PLOT, $
   time_1701_LSTAR_AE8MAX, rr_1701_LSTAR_AE8MAX, /XS, /YS, $
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
PRINT, 'FINISHED OUTPUT OF 1701 AE8MAX .EPS PLOT'
STOP                            ; OUTPUT AE8MAX .EPS PLOT

; ####################################################################
; ####################################################################
; ####################################################################
; ####################################################################
; ####################################################################

SET_PLOT, 'PS'
DEVICE, FILENAME = 'Images/Lstar_Expanded/' + $
        'PSD_Sanity_Check_1701_LSTAR_AE8MIN_expanded.eps', $
        XSIZE = 10., YSIZE = 10., /INCHES, $
        /PORTRAIT, /COLOR, /ENCAPSULATED, BITS_PER_PIXEL = 8

ny = numy_subplots
LOADCT, 3, /SILENT
IMAGE_CONT, $
   3.*psd_1709_LSTAR_AE8MIN( *, *, 0, 91 ), $
   time_1709_LSTAR_AE8MIN, rr_1709_LSTAR_AE8MIN, $
   TITLE = 'AE8MIN w/o Waves', $
   XTITLE = 'Simulation Time [ hours ]', $
   YTITLE = TEXTOIDL( 'L [ R_E ]' ), $
   MIN = fmin_AE8MIN, MAX = fmax_AE8MIN, $
   POSITION = $
   [ x_start, $
     y_start + ( ny - 1 ) * y_subplot_size + ( ny - 0 ) * y_space, $
     x_start + x_subplot_size, $
     y_start + ( ny - 0 ) * y_subplot_size + ( ny - 1 ) * y_space ], $
   /NORM, CHARTHICK = 2
TVLCT, 255, 255, 255, 255
MAKELINEY, $
   time_1709_LSTAR_AE8MIN( 1260 - 55 ), $
   LINESTYLE = 1, COLOR = 255, THICK = 3
MAKELINEY, $
   time_1709_LSTAR_AE8MIN( 1260 + 55 ), $
   LINESTYLE = 1, COLOR = 255, THICK = 3
MAKELINEX, $
   rr_1709_LSTAR_AE8MIN( iR ), $
   LINESTYLE = 1, COLOR = 255, THICK = 3
LOADCT, 3, /SILENT
COLORBAR, $
   RANGE = [ fmin_AE8MIN, fmax_AE8MIN ], $
   DIVISIONS = N_ELEMENTS( ticknames_AE8MIN ) - 1, $
   /VERTICAL, /RIGHT, $
   TITLE = TEXTOIDL( '3x PSD [ c / MeV cm ]^3' ), $
   POSITION = $
   [ x_start + x_subplot_size + .025, $
     y_start + ( ny - 1 ) * y_subplot_size + ( ny - 0 ) * y_space, $
     x_start + x_subplot_size + .050, $
     y_start + ( ny - 0 ) * y_subplot_size + ( ny - 1 ) * y_space ], $
   /NORM, $
   TICKNAMES = ticknames_AE8MIN, $
   TICKLEN = -0.25, $
   CHARSIZE = 2, CHARTHICK = 3
ny = ny - 1

LOADCT, 0, /SILENT
PLOT, $
   time_1709_LSTAR_AE8MIN( 1260 - 55 : 1260 + 55 ), $
   psd_1709_LSTAR_AE8MIN( 1260 - 55 : 1260 + 55, iR, 0, 91 ), $
   XTITLE = 'Simulation Time [ hours ]', $
   YTITLE = TEXTOIDL( 'PSD [ c / MeV cm ]^3' ), $
   POSITION = $
   [ x_start, $
     y_start + ( ny - 1 ) * y_subplot_size + ( ny - 0 ) * y_space, $
     x_start + x_subplot_size, $
     y_start + ( ny - 0 ) * y_subplot_size + ( ny - 1 ) * y_space ], $
   /NORM, CHARTHICK = 2, THICK = 3
MAKELINEX, $
   MEAN( psd_1709_LSTAR_AE8MIN( 1260 - 55 : 1260 + 55, iR, 0, 91 ) ), $
   LINESTYLE = 2, THICK = 3
ny = ny - 1

LOADCT, 3, /SILENT
IMAGE_CONT, $
   3.*psd_1701_LSTAR_AE8MIN( *, *, 0, 91 ), $
   time_1701_LSTAR_AE8MIN, rr_1701_LSTAR_AE8MIN, $
   TITLE = 'AE8MIN w/  Waves', $
   XTITLE = 'Simulation Time [ hours ]', $
   YTITLE = TEXTOIDL( 'L [ R_E ]' ), $
   min = fmin_AE8MIN,max = fmax_AE8MIN, $
   POSITION = $
   [ x_start, $
     y_start + ( ny - 1 ) * y_subplot_size + ( ny - 0 ) * y_space, $
     x_start + x_subplot_size, $
     y_start + ( ny - 0 ) * y_subplot_size + ( ny - 1 ) * y_space ], $
   /NORM, CHARTHICK = 2
TVLCT, 255, 255, 255, 255
MAKELINEY, $
   time_1701_LSTAR_AE8MIN( 1260 - 55 ), $
   LINESTYLE = 1, COLOR = 255, THICK = 3
MAKELINEY, $
   time_1701_LSTAR_AE8MIN( 1260 + 55 ), $
   LINESTYLE = 1, COLOR = 255, THICK = 3
MAKELINEX, $
   rr_1701_LSTAR_AE8MIN( iR ), $
   LINESTYLE = 1, COLOR = 255, THICK = 3
LOADCT, 3, /SILENT
COLORBAR, $
   RANGE = [ fmin_AE8MIN, fmax_AE8MIN ], $
   DIVISIONS = N_ELEMENTS( ticknames_AE8MIN ) - 1, $
   /VERTICAL, /RIGHT, $
   TITLE = TEXTOIDL( '3x PSD [ c / MeV cm ]^3' ), $
   POSITION = $
   [ x_start + x_subplot_size + .025, $
     y_start + ( ny - 1 ) * y_subplot_size + ( ny - 0 ) * y_space, $
     x_start + x_subplot_size + .050, $
     y_start + ( ny - 0 ) * y_subplot_size + ( ny - 1 ) * y_space ], $
   /NORM, $
   TICKNAMES = ticknames_AE8MIN, $
   TICKLEN = -0.25, $
   CHARSIZE = 2, CHARTHICK = 3
ny = ny - 1

LOADCT, 0, /SILENT
PLOT, $
   time_1701_LSTAR_AE8MIN( 1260 - 55 : 1260 + 55 ), $
   psd_1701_LSTAR_AE8MIN( 1260 - 55 : 1260 + 55, iR, 0, 91 ), $
   XTITLE = 'Simulation Time [ hours ]', $
   YTITLE = TEXTOIDL( '3x PSD [ c / MeV cm ]^3' ), $
   POSITION = $
   [ x_start, $
     y_start + ( ny - 1 ) * y_subplot_size + ( ny - 0 ) * y_space, $
     x_start + x_subplot_size, $
     y_start + ( ny - 0 ) * y_subplot_size + ( ny - 1 ) * y_space ], $
   /NORM, CHARTHICK = 2, THICK = 3
MAKELINEX, $
   MEAN( psd_1701_LSTAR_AE8MIN( 1260 - 55 : 1260 + 55, iR, 0, 91 ) ), $
   LINESTYLE = 2, THICK = 3
ny = ny - 1

LOADCT, 70, /SILENT
REVERSECT
IMAGE_CONT, $
   ( ratio_1701_LSTAR_AE8MIN( *, *, 0, 91 ) - 1 ) * 100., $
   time_1701_LSTAR_AE8MIN, rr_1701_LSTAR_AE8MIN, $
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
   rr_1701_LSTAR_AE8MIN( iR ), $
   LINESTYLE = 1, COLOR = 000, THICK = 3
PLOT, $
   time_1701_LSTAR_AE8MIN, rr_1701_LSTAR_AE8MIN, /XS, /YS, $
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
PRINT, 'FINISHED OUTPUT OF 1701 AE8MIN .EPS PLOT'
STOP                            ; OUTPUT AE8MIN .EPS PLOT

; ####################################################################
; ####################################################################
; ####################################################################
; ####################################################################
; ####################################################################

FOR $
   iMu = mu_minloc, mu_maxloc $
DO BEGIN
   
   DEVICE, FILENAME = $
           'Images/Lstar_Expanded/PSD_February_Plot_mu'+ $
           STRTRIM( STRING( iMu, FORMAT = '(I03)' ), 2 ) + $
           '_Lstar_expanded.eps', $
           XSIZE = 10., YSIZE = 10., /INCHES, $
           /PORTRAIT, /COLOR, /ENCAPSULATED, BITS_PER_PIXEL = 8
   
   numx_subplots = 2
   x_start = 0.050
   x_end   = 0.875
   x_space = 0.050
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
   
   fmin = -50.0
   fmax =  50.0

   ny = numy_subplots
   
   LOADCT, 70, /SILENT
   REVERSECT

   nx = 1
   IMAGE_CONT, $
      (  psd_1709_LSTAR_DEGELING( 536 : *, *, 0, iMu ) / $
        psd0_1709_LSTAR_DEGELING( 536 : *, *, 0, iMu ) - 1 ) * 100., $
      time_1709_LSTAR_DEGELING( 536 : * ), rr_1709_LSTAR_DEGELING, $
      TITLE = 'DEGELING w/o Waves, mu = ' + $
      STRING( xmm_1709_LSTAR_DEGELING( iMu ) * 100., FORMAT='(I4)') + $
      ' MeV / G', $
      XTITLE = 'Simulation Time [ hours ]', $
      YTITLE = TEXTOIDL( 'L [ R_E ]' ), $
      MIN = fmin, MAX = fmax, $
      POSITION = $
      [ x_start + ( nx - 1 ) * x_subplot_size + ( nx - 1 ) * x_space, $
        y_start + ( ny - 1 ) * y_subplot_size + ( ny - 0 ) * y_space, $
        x_start + ( nx - 0 ) * x_subplot_size + ( nx - 1 ) * x_space, $
        y_start + ( ny - 0 ) * y_subplot_size + ( ny - 1 ) * y_space ], $
      /NORM, CHARTHICK = 2
   nx = nx + 1
   
   IMAGE_CONT, $
      (  psd_1701_LSTAR_DEGELING( 536 : *, *, 0, iMu ) / $
        psd0_1701_LSTAR_DEGELING( 536 : *, *, 0, iMu ) - 1 ) * 100., $
      time_1701_LSTAR_DEGELING( 536 : * ), rr_1701_LSTAR_DEGELING, $
      TITLE = 'DEGELING w/ Waves, mu = ' + $
      	STRING( xmm_1701_LSTAR_DEGELING( iMu ) * 100., FORMAT='(I4)') + $
      	' MeV / G', $
      XTITLE = 'Simulation Time [ hours ]', $
      YTITLE = TEXTOIDL( 'L [ R_E ]' ), $
      MIN = fmin, MAX = fmax, $
      POSITION = $
      [ x_start + ( nx - 1 ) * x_subplot_size + ( nx - 1 ) * x_space, $
        y_start + ( ny - 1 ) * y_subplot_size + ( ny - 0 ) * y_space, $
        x_start + ( nx - 0 ) * x_subplot_size + ( nx - 1 ) * x_space, $
        y_start + ( ny - 0 ) * y_subplot_size + ( ny - 1 ) * y_space ], $
      /NORM, CHARTHICK = 2
   COLORBAR, $
      RANGE = [ fmin, fmax ], $
      DIVISIONS = 4, $
      /VERTICAL, /RIGHT, $
      TITLE = TEXTOIDL( 'PSD [ c / MeV cm ]^3' ), $
      POSITION = $
      [ x_end + .025, $
        y_start + ( ny - 1 ) * y_subplot_size + ( ny - 0 ) * y_space, $
        x_end + .050, $
        y_start + ( ny - 0 ) * y_subplot_size + ( ny - 1 ) * y_space ], $
      /NORM, $
      TICKLEN = -0.25, $
      CHARSIZE = 2, CHARTHICK = 3
   ny = ny - 1

   LOADCT, 70, /SILENT
   REVERSECT
   nx = 1
   IMAGE_CONT, $
      (  psd_1709_LSTAR_AE8MAX( 536 : *, *, 0, iMu ) / $
        psd0_1709_LSTAR_AE8MAX( 536 : *, *, 0, iMu ) - 1 ) * 100., $
      time_1709_LSTAR_AE8MAX( 536 : * ), rr_1709_LSTAR_AE8MAX, $
      TITLE = 'AE8MAX w/o Waves, mu = ' + $
      STRING( xmm_1709_LSTAR_AE8MAX( iMu ) * 100., FORMAT='(I4)') + $
      ' MeV / G', $
      XTITLE = 'Simulation Time [ hours ]', $
      YTITLE = TEXTOIDL( 'L [ R_E ]' ), $
      MIN = fmin, MAX = fmax, $
      POSITION = $
      [ x_start + ( nx - 1 ) * x_subplot_size + ( nx - 1 ) * x_space, $
        y_start + ( ny - 1 ) * y_subplot_size + ( ny - 0 ) * y_space, $
        x_start + ( nx - 0 ) * x_subplot_size + ( nx - 1 ) * x_space, $
        y_start + ( ny - 0 ) * y_subplot_size + ( ny - 1 ) * y_space ], $
      /NORM, CHARTHICK = 2
   nx = nx + 1
   
   IMAGE_CONT, $
      (  psd_1701_LSTAR_AE8MAX( 536 : *, *, 0, iMu ) / $
        psd0_1701_LSTAR_AE8MAX( 536 : *, *, 0, iMu ) - 1 ) * 100., $
      time_1701_LSTAR_AE8MAX( 536 : * ), rr_1701_LSTAR_AE8MAX, $
      TITLE = 'AE8MAX w/ Waves, mu = ' + $
      	STRING( xmm_1701_LSTAR_AE8MAX( iMu ) * 100., FORMAT='(I4)') + $
      	' MeV / G', $
      XTITLE = 'Simulation Time [ hours ]', $
      YTITLE = TEXTOIDL( 'L [ R_E ]' ), $
      MIN = fmin, MAX = fmax, $
      POSITION = $
      [ x_start + ( nx - 1 ) * x_subplot_size + ( nx - 1 ) * x_space, $
        y_start + ( ny - 1 ) * y_subplot_size + ( ny - 0 ) * y_space, $
        x_start + ( nx - 0 ) * x_subplot_size + ( nx - 1 ) * x_space, $
        y_start + ( ny - 0 ) * y_subplot_size + ( ny - 1 ) * y_space ], $
      /NORM, CHARTHICK = 2
   COLORBAR, $
      RANGE = [ fmin, fmax ], $
      DIVISIONS = 4, $
      /VERTICAL, /RIGHT, $
      TITLE = TEXTOIDL( 'PSD [ c / MeV cm ]^3' ), $
      POSITION = $
      [ x_end + .025, $
        y_start + ( ny - 1 ) * y_subplot_size + ( ny - 0 ) * y_space, $
        x_end + .050, $
        y_start + ( ny - 0 ) * y_subplot_size + ( ny - 1 ) * y_space ], $
      /NORM, $
      TICKLEN = -0.25, $
      CHARSIZE = 2, CHARTHICK = 3
   ny = ny - 1

   LOADCT, 70, /SILENT
   REVERSECT
   nx = 1
   IMAGE_CONT, $
      (  psd_1709_LSTAR_AE8MIN( 536 : *, *, 0, iMu ) / $
        psd0_1709_LSTAR_AE8MIN( 536 : *, *, 0, iMu ) - 1 ) * 100., $
      time_1709_LSTAR_AE8MIN( 536 : * ), rr_1709_LSTAR_AE8MIN, $
      TITLE = 'AE8MIN w/o Waves, mu = ' + $
      STRING( xmm_1709_LSTAR_AE8MIN( iMu ) * 100., FORMAT='(I4)') + $
      ' MeV / G', $
      XTITLE = 'Simulation Time [ hours ]', $
      YTITLE = TEXTOIDL( 'L [ R_E ]' ), $
      MIN = fmin, MAX = fmax, $
      POSITION = $
      [ x_start + ( nx - 1 ) * x_subplot_size + ( nx - 1 ) * x_space, $
        y_start + ( ny - 1 ) * y_subplot_size + ( ny - 0 ) * y_space, $
        x_start + ( nx - 0 ) * x_subplot_size + ( nx - 1 ) * x_space, $
        y_start + ( ny - 0 ) * y_subplot_size + ( ny - 1 ) * y_space ], $
      /NORM, CHARTHICK = 2
   nx = nx + 1
   
   IMAGE_CONT, $
      (  psd_1701_LSTAR_AE8MIN( 536 : *, *, 0, iMu ) / $
        psd0_1701_LSTAR_AE8MIN( 536 : *, *, 0, iMu ) - 1 ) * 100., $
      time_1701_LSTAR_AE8MIN( 536 : * ), rr_1701_LSTAR_AE8MIN, $
      TITLE = 'AE8MIN w/ Waves, mu = ' + $
      	STRING( xmm_1701_LSTAR_AE8MIN( iMu ) * 100., FORMAT='(I4)') + $
      	' MeV / G', $
      XTITLE = 'Simulation Time [ hours ]', $
      YTITLE = TEXTOIDL( 'L [ R_E ]' ), $
      MIN = fmin, MAX = fmax, $
      POSITION = $
      [ x_start + ( nx - 1 ) * x_subplot_size + ( nx - 1 ) * x_space, $
        y_start + ( ny - 1 ) * y_subplot_size + ( ny - 0 ) * y_space, $
        x_start + ( nx - 0 ) * x_subplot_size + ( nx - 1 ) * x_space, $
        y_start + ( ny - 0 ) * y_subplot_size + ( ny - 1 ) * y_space ], $
      /NORM, CHARTHICK = 2
   COLORBAR, $
      RANGE = [ fmin, fmax ], $
      DIVISIONS = 4, $
      /VERTICAL, /RIGHT, $
      TITLE = TEXTOIDL( 'PSD [ c / MeV cm ]^3' ), $
      POSITION = $
      [ x_end + .025, $
        y_start + ( ny - 1 ) * y_subplot_size + ( ny - 0 ) * y_space, $
        x_end + .050, $
        y_start + ( ny - 0 ) * y_subplot_size + ( ny - 1 ) * y_space ], $
      /NORM, $
      TICKLEN = -0.25, $
      CHARSIZE = 2, CHARTHICK = 3
   ny = ny - 1
   
   DEVICE, /CLOSE

ENDFOR
PRINT, "FINISHED FEBRUARY PLOTS:"
STOP

SET_PLOT, 'X'

dim = SIZE( psd_1701_LSTAR_AE8MAX, /DIMENSIONS )
nTime = dim( 0 )
nR = dim( 1 )
nMLT = dim( 2 )
nMu = dim( 3 )

psd_2d_Time_average_1701_LSTAR_DEGELING = FLTARR( nR, nMu )
psd_2d_Time_average_1701_LSTAR_AE8MAX = FLTARR( nR, nMu )
psd_2d_Time_average_1701_LSTAR_AE8MIN = FLTARR( nR, nMu )

psd_2d_Time_average_1709_LSTAR_DEGELING = FLTARR( nR, nMu )
psd_2d_Time_average_1709_LSTAR_AE8MAX = FLTARR( nR, nMu )
psd_2d_Time_average_1709_LSTAR_AE8MIN = FLTARR( nR, nMu )


FOR $
   iR = 0, nR - 1 $
DO BEGIN

   FOR $
      iMu = 0, nMu - 1 $
   DO BEGIN

      psd_2d_time_Average_1701_LSTAR_DEGELING( iR, iMu ) = $
         MEAN( PSD_1701_LSTAR_DEGELING( 1980 : 2160, iR, 0, iMu ) )
      psd_2d_time_Average_1709_LSTAR_DEGELING( iR, iMu ) = $
         MEAN( PSD_1709_LSTAR_DEGELING( 1980 : 2160, iR, 0, iMu ) )

      psd_2d_time_Average_1701_LSTAR_AE8MAX( iR, iMu ) = $
         MEAN( PSD_1701_LSTAR_AE8MAX( 1980 : 2160, iR, 0, iMu ) )
      psd_2d_time_Average_1709_LSTAR_AE8MAX( iR, iMu ) = $
         MEAN( PSD_1709_LSTAR_AE8MAX( 1980 : 2160, iR, 0, iMu ) )

      psd_2d_time_Average_1701_LSTAR_AE8MIN( iR, iMu ) = $
         MEAN( PSD_1701_LSTAR_AE8MIN( 1980 : 2160, iR, 0, iMu ) )
      psd_2d_time_Average_1709_LSTAR_AE8MIN( iR, iMu ) = $
         MEAN( PSD_1709_LSTAR_AE8MIN( 1980 : 2160, iR, 0, iMu ) )
      
   ENDFOR

ENDFOR

STOP

test_string = ''
FOR $
   iMu = mu_minloc, mu_maxloc $
DO BEGIN

   PRINT, 'iMu = ', imu

   SET_PLOT, 'X'
   ;; W, 2, 3
   WINDOW, 4, TITLE = 'Line Plot'
   W, 1, 1
   LOADCT, 0, /SILENT
   yr_max = $
      0.
      ;; MAX( CEIL( ALOG10( psd_2d_time_average_1709_LSTAR( 16 : *, iMu ) ) * 1. ) )
   yr_min = yr_max - 15.


   PLOT, $
      rr_1709_LSTAR_AE8MAX, $
      psd_2d_time_average_1709_LSTAR_AE8MAX( *, iMu ), $
      TITLE = 'AE8MAX w/o  Waves, mu = ' + $
      STRING( xmm_1709_Lstar_Ae8max( iMu ) * 100., FORMAT='(I4)') + $
      ' MeV / G', $
      XTITLE = TEXTOIDL( 'L [ R_E ]' ), XMINOR = 8, $
      YTITLE = TEXTOIDL( 'PSD [ c / MeV cm ]^3' ), /YLOG, $
      YRANGE = 10^[ yr_min, yr_max  ], $
      ;; YRANGE = [ 1d-10, 1d0 ], $
      ;; YTITLE = TEXTOIDL( 'PSD [ c / MeV cm ]^3' ), $
      ;; YRANGE = [ 0., MAX( psd_1709_LSTAR_AE8MAX( 0, *, iMu ) ) ] * 1.2, $
      PSYM = -7, CHARSIZE = 2, CHARTHICK = 2
   OPLOT, $
      rr_1701_LSTAR_AE8MAX, $
      psd_2d_time_average_1701_LSTAR_AE8MAX( *, iMu ), $
      PSYM =  4
   TVLCT, 255, 000, 000, 101
   OPLOT, $
      rr_1709_LSTAR_AE8MIN, $
      psd_2d_time_average_1709_LSTAR_AE8MIN( *, iMu ), $
      PSYM = -7, COLOR = 101
   OPLOT, $
      rr_1701_LSTAR_AE8MIN, $
      psd_2d_time_average_1701_LSTAR_AE8MIN( *, iMu ), $
      PSYM =  5, COLOR = 101

   TVLCT, 000, 000, 255, 102
   OPLOT, $
      rr_1709_LSTAR_DEGELING, $
      psd_2d_time_average_1709_LSTAR_DEGELING( *, iMu ), $
      PSYM = -7, COLOR = 102
   OPLOT, $
      rr_1701_LSTAR_DEGELING, $
      psd_2d_time_average_1701_LSTAR_DEGELING( *, iMu ), $
      PSYM =  6, COLOR = 102
   
   SET_PLOT, 'PS'
   DEVICE, $
      FILENAME = 'Images/Lstar_Expanded/PSD_Line_Plot_mu' + $
      		STRTRIM( STRING( iMu, FORMAT = '(I03)' ), 2 ) + $
      '_Lstar_expanded.eps', $
      XSIZE = 10., YSIZE = 10., /INCHES, $
      /ENCAPSULATED, /PORTRAIT, /COLOR, BITS_PER_PIXEL = 8
   
   W, 1, 1
   LOADCT, 0, /SILENT
   TVLCT, 255, 000, 000, 101
   TVLCT, 000, 000, 255, 102

   PLOT, $
      rr_1709_LSTAR_AE8MAX, $
      psd_2d_time_average_1709_LSTAR_AE8MAX( *, iMu ), $
      TITLE = STRING( xmm_1709_Lstar_Ae8max( iMu ) * 100., FORMAT='(I4)') + $
      	' MeV / G', $
      XTITLE = TEXTOIDL( 'L [ R_E ]' ), XMINOR = 8, $
      YTITLE = TEXTOIDL( 'PSD [ c / MeV cm ]^3' ), /YLOG, $
      YRANGE = 10^[ yr_min, yr_max  ], $
      ;; YRANGE = [ 1d-10, 1d0 ], $
      ;; YTITLE = TEXTOIDL( 'PSD [ c / MeV cm ]^3' ), $
      ;; YRANGE = [ 0., MAX( psd_1709_LSTAR_AE8MAX( 0, *, iMu ) ) ] * 1.2, $
      PSYM = -7, CHARSIZE = 2, CHARTHICK = 3, THICK = 2, $
      XTHICK  = 3, YTHICK = 3
   OPLOT, $
      rr_1701_LSTAR_AE8MAX, $
      psd_2d_time_average_1701_LSTAR_AE8MAX( *, iMu ), $
      PSYM =  4
   TVLCT, 255, 000, 000, 101
   OPLOT, $
      rr_1709_LSTAR_AE8MIN, $
      psd_2d_time_average_1709_LSTAR_AE8MIN( *, iMu ), $
      PSYM = -7, COLOR = 101
   OPLOT, $
      rr_1701_LSTAR_AE8MIN, $
      psd_2d_time_average_1701_LSTAR_AE8MIN( *, iMu ), $
      PSYM =  5, COLOR = 101

   TVLCT, 000, 000, 255, 102
   OPLOT, $
      rr_1709_LSTAR_DEGELING, $
      psd_2d_time_average_1709_LSTAR_DEGELING( *, iMu ), $
      PSYM = -7, COLOR = 102
   OPLOT, $
      rr_1701_LSTAR_DEGELING, $
      psd_2d_time_average_1701_LSTAR_DEGELING( *, iMu ), $
      PSYM =  6, COLOR = 102

   LEGEND, $
      [ 'Null AE8MAX', 'Waves AE8MAX', $
        'Null AE8MIN', 'Waves AE8MIN', $
        'Null Degeling', 'Waves Degeling' ], $
      PSYM = [ -7, 4, -7, 5, -7, 6 ], LINESTYLE = 0, $
      COLOR = [ 000, 000, 101, 101, 102, 102 ], $
      BOX = 1, /BOTTOM, /RIGHT, NUMBER = 1.5, $
      CHARTHICK = 3, THICK = 3

   DEVICE, /CLOSE
   ;; READ, test_string

   SET_PLOT,'X'
   
ENDFOR

END
