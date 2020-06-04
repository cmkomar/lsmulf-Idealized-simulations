
IF $
   ( do_load_check_1 EQ !NULL ) $
THEN BEGIN

   PRINT, 'LOADING run1701: '
   PSD_Master_plot,'run1701', $
                   psd_1701_1, $
                   psd_MLT_Average_1701_1, $
                   psd_Time_and_MLT_Average_1701_1, $
                   time_1701_1,rr_1701_1,xmm_1701_1, $
                   AVERAGE = 55
   PRINT, 'LOADING run1709: '
   PSD_Master_plot,'run1709', $
                   psd_1709_1, $
                   psd_MLT_Average_1709_1, $
                   psd_Time_and_MLT_Average_1709_1, $
                   time_1709_1,rr_1709_1,xmm_1709_1, $
                   AVERAGE = 55
   ratio_1 = psd_1709_1 / psd_1701_1

   do_load_check_1 = !false

ENDIF   

iR = 40
print, 'L-shell: ', rr_1701_1( iR )

W, 1, 5
WINDOW, 1
LOADCT, 3, /SILENT
IMAGE_CONT, psd_1709_1( *, *, 0, 37 ), time_1709_1, rr_1709_1, $
            TITLE = 'w/o Waves'
MAKELINEY, time_1709_1( 1260 - 55 ), LINESTYLE = 1
MAKELINEY, time_1709_1( 1260 + 55 ), LINESTYLE = 1
MAKELINEX, rr_1709_1( iR ), LINESTYLE = 1

LOADCT, 0, /SILENT
PLOT, time_1709_1( 1260 - 55 : 1260 + 55 ), $
      psd_1709_1( 1260 - 55 : 1260 + 55, iR, 0, 37 )
MAKELINEX, MEAN( psd_1709_1( 1260 - 55 : 1260 + 55, iR, 0, 37 ) ), $
           LINESTYLE = 2

LOADCT, 3, /SILENT
IMAGE_CONT, psd_1701_1( *, *, 0, 37 ), time_1701_1, rr_1701_1, $
            TITLE = 'w/  Waves'
MAKELINEY, time_1701_1( 1260 - 55 ), LINESTYLE = 1
MAKELINEY, time_1701_1( 1260 + 55 ), LINESTYLE = 1
MAKELINEX, rr_1701_1( iR ), LINESTYLE = 1

LOADCT, 0, /SILENT
PLOT, time_1701_1( 1260 - 55 : 1260 + 55 ), $
      psd_1701_1( 1260 - 55 : 1260 + 55, iR, 0, 37 )
MAKELINEX, MEAN( psd_1701_1( 1260 - 55 : 1260 + 55, iR, 0, 37 ) ), $
           LINESTYLE = 2

LOADCT, 70, /SILENT
REVERSECT
IMAGE_CONT, ( ratio_1( *, *, 0, 37 ) - 1 ) * 100., $
            time_1701_1, rr_1701_1, $
            TITLE = '% Diff.', $
            MIN = -100., MAX = 100.

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

SET_PLOT, 'PS'
DEVICE, FILENAME = 'Images/PSD_Sanity_Check_1.eps', $
        XSIZE = 10., YSIZE = 10., /INCHES, $
        /PORTRAIT, /COLOR, /ENCAPSULATED, BITS_PER_PIXEL = 8

W, numx_subplots, numy_subplots + 1
fmin = 1e-10
fmax = 1e-5
ny = numy_subplots
LOADCT, 3, /SILENT
IMAGE_CONT, psd_1709_1( *, *, 0, 37 ), time_1709_1, rr_1709_1, $
            TITLE = 'w/o Waves', $
            XTITLE = 'Simulation Time [ hours ]', $
            YTITLE = TEXTOIDL( 'L [ R_E ]' ), $
            min = fmin,max = fmax, $
            POSITION = $
            [ x_start, $
              y_start + ( ny - 1 ) * y_subplot_size + ( ny - 0 ) * y_space, $
              x_start + x_subplot_size, $
              y_start + ( ny - 0 ) * y_subplot_size + ( ny - 1 ) * y_space ], $
            /NORM, CHARTHICK = 2
TVLCT, 255, 255, 255, 255
MAKELINEY, time_1709_1( 1260 - 55 ), $
           LINESTYLE = 1, COLOR = 255, THICK = 3
MAKELINEY, time_1709_1( 1260 + 55 ), $
           LINESTYLE = 1, COLOR = 255, THICK = 3
MAKELINEX, rr_1709_1( iR ), $
           LINESTYLE = 1, COLOR = 255, THICK = 3
LOADCT, 3, /SILENT
COLORBAR, $
   range = [ fmin, fmax ], $
   DIVISIONS = 5, $
   /VERTICAL, /RIGHT, $
   TITLE = TEXTOIDL( 'PSD [ c / MeV cm ]^3' ), $
   POSITION = $
   [ x_start + x_subplot_size + .025, $
     y_start + ( ny - 1 ) * y_subplot_size + ( ny - 0 ) * y_space, $
     x_start + x_subplot_size + .050, $
     y_start + ( ny - 0 ) * y_subplot_size + ( ny - 1 ) * y_space ], $
   /NORM, $
   TICKNAMES = [ TEXTOIDL( '10^{-10}' ), TEXTOIDL( '10^{-9}' ), $
                 TEXTOIDL( '10^{-8}' ), TEXTOIDL( '10^{-7}' ), $
                 TEXTOIDL( '10^{-6}' ), TEXTOIDL( '10^{-5}' ) ], $
   TICKLEN = -0.25, $
   CHARSIZE = 2, CHARTHICK = 3
ny = ny - 1

LOADCT, 0, /SILENT
PLOT, time_1709_1( 1260 - 55 : 1260 + 55 ), $
      psd_1709_1( 1260 - 55 : 1260 + 55, iR, 0, 37 ), $
      XTITLE = 'Simulation Time [ hours ]', $
      YTITLE = TEXTOIDL( 'PSD [ c / MeV cm ]^3' ), $
      POSITION = $
      [ x_start, $
        y_start + ( ny - 1 ) * y_subplot_size + ( ny - 0 ) * y_space, $
        x_start + x_subplot_size, $
        y_start + ( ny - 0 ) * y_subplot_size + ( ny - 1 ) * y_space ], $
      /NORM, CHARTHICK = 2, THICK = 3
MAKELINEX, MEAN( psd_1709_1( 1260 - 55 : 1260 + 55, iR, 0, 37 ) ), $
           LINESTYLE = 2, THICK = 3
ny = ny - 1

LOADCT, 3, /SILENT
IMAGE_CONT, psd_1701_1( *, *, 0, 37 ), time_1701_1, rr_1701_1, $
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
MAKELINEY, time_1701_1( 1260 - 55 ), $
           LINESTYLE = 1, COLOR = 255, THICK = 3
MAKELINEY, time_1701_1( 1260 + 55 ), $
           LINESTYLE = 1, COLOR = 255, THICK = 3
MAKELINEX, rr_1701_1( iR ), $
           LINESTYLE = 1, COLOR = 255, THICK = 3
LOADCT, 3, /SILENT
COLORBAR, $
   range = [ fmin, fmax ], $
   DIVISIONS = 5, $
   /VERTICAL, /RIGHT, $
   TITLE = TEXTOIDL( 'PSD [ c / MeV cm ]^3' ), $
   POSITION = $
   [ x_start + x_subplot_size + .025, $
     y_start + ( ny - 1 ) * y_subplot_size + ( ny - 0 ) * y_space, $
     x_start + x_subplot_size + .050, $
     y_start + ( ny - 0 ) * y_subplot_size + ( ny - 1 ) * y_space ], $
   /NORM, $
   TICKNAMES = [ TEXTOIDL( '10^{-10}' ), TEXTOIDL( '10^{-9}' ), $
                 TEXTOIDL( '10^{-8}' ), TEXTOIDL( '10^{-7}' ), $
                 TEXTOIDL( '10^{-6}' ), TEXTOIDL( '10^{-5}' ) ], $
   TICKLEN = -0.25, $
   CHARSIZE = 2, CHARTHICK = 3
ny = ny - 1

LOADCT, 0, /SILENT
PLOT, time_1701_1( 1260 - 55 : 1260 + 55 ), $
      psd_1701_1( 1260 - 55 : 1260 + 55, iR, 0, 37 ), $
      XTITLE = 'Simulation Time [ hours ]', $
      YTITLE = TEXTOIDL( 'PSD [ c / MeV cm ]^3' ), $
      POSITION = $
      [ x_start, $
        y_start + ( ny - 1 ) * y_subplot_size + ( ny - 0 ) * y_space, $
        x_start + x_subplot_size, $
        y_start + ( ny - 0 ) * y_subplot_size + ( ny - 1 ) * y_space ], $
      /NORM, CHARTHICK = 2, THICK = 3
MAKELINEX, MEAN( psd_1701_1( 1260 - 55 : 1260 + 55, iR, 0, 37 ) ), $
           LINESTYLE = 2, THICK = 3
ny = ny - 1

LOADCT, 70, /SILENT
REVERSECT
IMAGE_CONT, ( ratio_1( *, *, 0, 37 ) - 1 ) * 100., time_1701_1, rr_1701_1, $
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
            ;; POSITION = $
            ;; [ 0.075, 0.08, 0.85, 0.28 ], /NORM
TVLCT, 000, 000, 000, 000
MAKELINEX, rr_1701_1( iR ), $
           LINESTYLE = 1, COLOR = 000, THICK = 3
PLOT, time_1701_1, rr_1701_1, /XS, /YS, $
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
   range = [ -100, 100 ], $
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
SET_PLOT, 'X'

END
