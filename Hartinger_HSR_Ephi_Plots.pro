
PRO Make_HSR_Ephi_Plots, $

; INTPUT:
   
   run_number, $

; OUTPUT:
   
   UX = ux, UY = uy, UZ = uz, $
   BX = bx, BY = by, BZ = bz, $
   XX = xx, YY = yy, HOURA = houra, $
   NUMX = numx, NUMY = numy, $

; FLAGS:
   
   DEBUG = debug_check, LOAD = load_check

  CD, '~/Research/Hartinger_HSR/' + run_number + '/GM/', $
      CURRENT = current_dir

  IF $
     KEYWORD_SET( load_check ) $
  THEN BEGIN

     PRINT, 'LOADING GM FILES:'
     loadGMeq, $
        UX = ux, UY = uy, UZ = uz, $
        BX = bx, BY = by, BZ = bz, $
        XX = xx, YY = yy, HOURA = houra, $
        NUMX = numx, NUMY = numy
     
  ENDIF

  nTime = N_ELEMENTS( houra )

  ex = ( uy * bz - uz * by ) * 1e-3
  ey = ( ux * bz - uz * bx ) * 1e-3

  rr = FLTARR( numx, numy )
  phi = FLTARR( numx, numy )
  
  FOR $
     i = 0, numx - 1 $
  DO BEGIN

     FOR $
        j = 0, numy - 1 $
     DO BEGIN

        rr_temp = $
           SQRT( xx( i ) ^ 2 + yy( j ) ^ 2 )
        rr( i, j ) = rr_temp

        IF $
           ( ( yy( j ) eq 0. ) and ( xx( i ) le 0. ) ) $
        THEN $
           phi( i, j ) = !PI $
        ELSE $
           phi( i, j ) = $
           	SIGN( yy( j ) ) * ACOS( xx( i ) / rr_temp )
        
     ENDFOR
     
  ENDFOR

  ones_loc = WHERE( rr GT 5. )
  ones_gt_5re = FLTARR( numx, numy )
  ones_gt_5re( ones_loc ) = 1.
  
  FOR $
     iTime = 0, nTime - 1 $
     ;; iTime = 1140, 1190, 21 $     
     ;; iTime = 1440, 1440 $     
  DO BEGIN

     PRINT, 'iTime = ', iTime
     
     er = $
        ( ex( iTime, *, * ) * COS( phi ) + $
          ey( iTime, *, * ) * SIN( phi ) ) * ones_gt_5re
        
     ephi = $
        ( -ex( iTime, *, * ) * SIN( phi ) + $
           ey( iTime, *, * ) * COS( phi ) ) * ones_gt_5re

     ephi_max =  1.
     ephi_min = -ephi_max

     IF $
        KEYWORD_SET( debug_check ) $
     THEN BEGIN
        
        W, 2, 3
        LOADCT, 70, /SILENT
        REVERSECT
        IMAGE_CONT, $
           ex( iTime, *, * ), xx, yy, $
           XTITLE = TEXTOIDL( 'x [ R_E ]' ), $
           YTITLE = TEXTOIDL( 'y [ R_E ]' ), $
           MINVAL = -2.5, MAXVAL = 2.5
        IMAGE_CONT, rr, xx, yy
        
        IMAGE_CONT, $
           ey( iTime, *, * ), xx, yy, $
           XTITLE = TEXTOIDL( 'x [ R_E ]' ), $
           YTITLE = TEXTOIDL( 'y [ R_E ]' ), $
           MINVAL = -2.5, MAXVAL = 2.5
        IMAGE_CONT, phi, xx, yy, MINVAL = -!PI, MAXVAL = !PI
        
        IMAGE_CONT, $
           er, xx, yy, $
           MINVAL = -2.5, MAXVAL = 2.5
        IMAGE_CONT, $
           ephi, xx, yy, $
           MINVAL = ephi_min, MAXVAL = ephi_max

     ENDIF

     W, 1, 1
     ;; IMAGE_CONT, $
     ;;    ;; ey( 0, *, * ), xx, yy, $
     ;;    ephi, xx, yy, $
     ;;    XTITLE = TEXTOIDL( 'x [ R_E ]' ), $
     ;;    YTITLE = TEXTOIDL( 'y [ R_E ]' ), $
     ;;    TITLE = '', $
     ;;    MINVAL = -1.5, MAXVAL = 1.5, $
     ;;    CHARSIZE = 2., CHARTHICK = 3, XTHICK = 3, YTHICK = 3, $
     ;;    ;; POSITION = [ x_subplot_start, y_subplot_start, $
     ;;    ;;              x_subplot_end, y_subplot_end ], /NORM
     ;;    /ASPECT

     SPAWN, 'mkdir -p Images/Ephi/'
        
     SET_PLOT, 'PS'
     DEVICE, $
        FILENAME = 'Images/Ephi/Ephi_' + $
	        STRTRIM( STRING( iTime * 10L, FORMAT = '(I05)' ), 2 ) + $
        	'.eps', $
        XSIZE = 06.0, $ ;XOFFSET = 1, $
        YSIZE = 09.0, $ ;YOFFSET = 1,
        /INCHES, $
        /ENCAPSULATED, /PORTRAIT, /COLOR, BITS_PER_PIXEL = 8

     x_subplot_start = 0.200
     x_subplot_end = 0.825

     y_subplot_start = 0.100
     y_subplot_end = 0.940

     jultime = julday( 01, 01, 2001, 00, 00, houra )
     caldat, jultime, months, days, years, hours, mins, secs
     W, 1, 1
     LOADCT, 70, /SILENT
     REVERSECT
     IMAGE_CONT, $
        ;; ey( 0, *, * ), xx, yy, $
        ephi, xx, yy, $
        XTITLE = TEXTOIDL( 'x [ R_E ]' ), $
        YTITLE = TEXTOIDL( 'y [ R_E ]' ), $
        TITLE = $
        STRTRIM(STRING( hours( itime ), FORMAT = '(I02)' ), 2 ) + ':' + $
        STRTRIM(STRING( mins( itime ), FORMAT = '(I02)' ), 2 ) + ':' + $
        STRTRIM(STRING( secs( itime ), FORMAT = '(I02)' ), 2 ), $
        MINVAL = ephi_min, MAXVAL = ephi_max, $
        CHARSIZE = 2., CHARTHICK = 3, XTHICK = 3, YTHICK = 3, $
        POSITION = [ x_subplot_start, y_subplot_start, $
                     x_subplot_end, y_subplot_end ], /NORM
     
     COLORBAR, $
        /RIGHT, /VERTICAL, $
        RANGE = [ -1.5, 1.5 ], DIVISIONS = 6, FORMAT = '(F4.1)', $
        POSITION = [ x_subplot_end + 0.025, y_subplot_start, $
                     x_subplot_end + 0.050, y_subplot_end ], /NORM, $
        TITLE = TEXTOIDL( ' E_\phi [ mV / m ] ' ), $
        CHARSIZE = 1.25, CHARTHICK = 3.00, XTHICK = 3.00, YTHICK = 3.00
     DEVICE, /CLOSE
     SET_PLOT, 'X'
     
  ENDFOR


  CD, current_dir

  RETURN
  
END

