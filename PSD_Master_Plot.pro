pro PSD_Master_Plot, $

   run_number, $

   psd_interp, time_interp, rr, xmm, $
   
   AVERAGE_INDEX = average_index, AE8MIN = AE8MIN_check, $
   EXTENDED = extended_check, HELP = help_check


  IF $
     KEYWORD_SET( help_check ) $
  THEN BEGIN

     PRINT, "File dependencies:"
     PRINT, "SWMF idlrc"
     PRINT, "Analyze_Hartinger-10.pro"
     PRINT, "sub.pro"
     PRINT, "reversect.pro"
     
  ENDIF ; Help check
  
  IF $
     KEYWORD_SET( extended_check ) $
  THEN $
     CD, run_number + '/IM_extended/', CURRENT = original_dir $
  ELSE $
     CD, run_number + '/IM_HiRes/', CURRENT = original_dir

  IF NOT( KEYWORD_SET( average_index ) ) THEN $
     average_index = 30

  ANALYZE_HARTINGER, $
     15, er_sat, ephi_sat, sattime, r_sat, $
     BTOT = btot
  dim_sat = SIZE( r_sat, /DIMENSIONS )
  nTime_sat = dim_sat( 0 )
  nR_sat = dim_sat( 1 )
  
  GET_LOG, 'IMF.log', wlog, wlognames, logtime, timeunit
  
  IF $
     KEYWORD_SET( AE8MIN_check ) $
  THEN $
     AE8_condition = 'AE8MIN' $
  ELSE $
     AE8_condition = 'AE8MAX'

  RESTORE, 'PSD_reduced_' + AE8_condition + '.sav'
  
  numtime = 2161
  time_interp = DINDGEN( numtime ) * 10. / 3600.
  
  density = INTERPOL( wlog(*,13), logtime - logtime( 0 ), time_interp )
  
  rmin = 3.0
  rmax = 8.5
  dr = 0.125
  nR = ( rmax - rmin ) / dr + 1
  rr = FINDGEN( nR ) * dr + rmin
  
  iTime0 = 535
  psd0 = psd( iTime0, *, *, * )
  
  dim = SIZE( psd, /DIMENSIONS )
  nTime = dim( 0 )
  nRoa = dim( 1 )
  nMLT = dim( 2 )
  nMu = dim( 3 )
  
  psd_interp = FLTARR( nTime, nR, nMu )
  mu_MeVperG = xmm * 100.
  energy = FLTARR( nR, nMu )
  period = FLTARR( nR, nMu )
  
  E0 = 0.511
  FOR $
     iR = 0, nR - 1 $
  DO BEGIN
     
     beq = 3.11e4 / rr( iR )^3 * 1e-5
     
     FOR $
        iMu = 0, nMu - 1 $
     DO BEGIN
        
        energy( iR, iMu ) = SQRT( E0^2 + 2*mu_MeVperG( iMu )*beq*E0)-E0
        T = energy( iR, iMu ) / E0
        gamma = T + 1
        beta = SQRT( T * ( T + 2 ) ) / ( T + 1 )
        
        period( iR, iMu ) = $
           0.667 * 1.55e4 / rr( iR ) / gamma / beta / beta
        
     ENDFOR
     
  ENDFOR
  
  PRINT, 'INTERPOLATING PSD IN R:'
  
  mu_minloc = MIN( WHERE( mu_MeVperG GE 2e1 ) )
  mu_maxloc = MAX( WHERE( mu_MeVperG LE 1e4 ) )
  
;; print,"mu_minloc, mu_MeVperg(mu_minloc): ", $
;;       mu_minloc, mu_MeVperg(mu_minloc)
;; print,"mu_maxloc, mu_MeVperg(mu_maxloc): ", $
;;       mu_maxloc, mu_MeVperg(mu_maxloc)
;; stop
  FOR $
     iTime = 0, nTime - 1 $
  DO BEGIN
     
     PRINT, 'iTime: ', iTime
     
     FOR $
        ;; iMu = 17, 49 $
        ;; iMu = 0, nMu - 1 $
        iMu = mu_minloc, mu_maxloc $
     DO $
        psd_interp(iTime, *, iMu ) = $
        10 ^ INTERPOL( ALOG10( psd( iTime, *, 0, iMu ) ), $
                       roa( iTime, *, 0 ), rr )
     
  ENDFOR
  
  PSD_average = FLTARR( nTime, nR, nMu )
  dPSD = FLTARR( nTime, nR, nMu )
  min_average = -average_index
  max_average =  average_index
  SPAWN, 'mkdir -p Images/' + AE8_condition + '/PSD_Master/'
  
  FOR $
     ;; iTime = iTime0, nTime - 200 $
     ;; iTime = iTime0, iTime0 $
     ;; iTime = iTime0, 1080, 1080 - iTime0 $
     iTime = iTime0, 1260, 1260 - iTime0 $
     ;; iTime = 1080, 1080 $
  DO BEGIN
     
     PRINT,'################################'
     PRINT,'iTime='+STRTRIM(STRING(iTime),2)
     PRINT,'################################'
     
     FOR $
        iMu = 0, nMu - 1 $
     DO BEGIN

        FOR $
           iAverage = min_average, max_average, 1 $
        DO BEGIN
           
           PSD_average( iTime, *, iMu ) = $
              PSD_average( iTime, *, iMu ) + $
              PSD_interp( iTime + iAverage, *, iMu )
           
        ENDFOR                  ; iAverage Loop
        
        dPSD( iTime, *, iMu ) =  $
           psd_average( iTime , *, iMu ) / $
           PSD_average( iTime0, *, iMu ) - 1.
        
     ENDFOR                     ; iMu Loop

     PSD_average = $
        PSD_average / ( max_average - min_average + 1 )
     
     FFT_Movie, $
        EPHI = ephi_window, TIME_IMIN = iTime, FREQUENCIES = freq
     
     ;; PRINT,"freq: ", freq
     ;; STOP
     
     SET_PLOT, 'PS'
     DEVICE, $
        FILENAME = 'Images/' + AE8_Condition + $
        	'/PSD_Master/PSD_Master_Plot' + $
        STRTRIM( STRING( iTime, FORMAT = '(I04)'), 2) + $
        '_min' + STRTRIM( STRING( min_average, FORMAT = '(I04)'), 2) + $
        '_max' + STRTRIM( STRING( max_average, FORMAT = '(I04)'), 2) + $
        '_0.eps', $
        XSIZE = 10., YSIZE = 10., /INCHES, $
        /COLOR,/ENCAPSULATED, /PORTRAIT, BITS_PER_PIXEL = 8
     
     W, 1, 7
     LOADCT, 16, /SILENT
     ;; REVERSECT
     IMAGE_CONT, $
        ALOG10( PSD_Average( iTime0, *, mu_minloc : mu_maxloc ) ), $
        rr, mu_MeVperG( mu_minloc : mu_maxloc ), $
        MINVALUE = -10., MAXVALUE = 0., $
        TITLE = TEXTOIDL( 'PSD_0' ), $
        XTITLE = TEXTOIDL( 'L [ R_E ]' ), $
        YTITLE = TEXTOIDL( '\mu [ MeV / G ] ' ), /YLOG, $
        CHARTHICK = 3, CHARSIZE = 2
     TVLCT, 000, 255, 000, 000
     ;; TVLCT, 000, 128, 000, 000
     CONTOUR, period, rr, mu_MeVperG, $
              level = 1 / 0.003, $
              C_THICK = 3, C_LINESTYLE = 0, COLOR = 000, /OVER
     CONTOUR, period, rr, mu_MeVperG, $
              level = 2 / 0.003, $
              C_THICK = 3, C_LINESTYLE = 2, COLOR = 000, /OVER
     CONTOUR, period, rr, mu_MeVperG, $
              level = 3 / 0.003, $
              C_THICK = 3, C_LINESTYLE = 3, COLOR = 000, /OVER
     ;; CONTOUR, period, rr, mu_MeVperG, $
     ;;          level = 1 / 0.003, $
     ;;          C_THICK = 3, C_LINESTYLE = 0, COLOR = 000, /OVER
     TVLCT, 255, 000, 255, 100
     CONTOUR, energy, rr, mu_MeVperG, $
              LEVEL = 1, $
              C_THICK = 3, C_LINESTYLE = 0, COLOR = 100, /OVER
     
     LOADCT, 16, /SILENT
     ;; REVERSECT
     IMAGE_CONT, $
        ALOG10( PSD_Average( iTime , *, mu_minloc : mu_maxloc ) ), $
        rr, mu_MeVperG( mu_minloc : mu_maxloc ), $
        MINVALUE = -10., MAXVALUE = 0., $
        TITLE = TEXTOIDL( 'PSD_1' ), $
        XTITLE = TEXTOIDL( 'L [ R_E ]' ), $
        YTITLE = TEXTOIDL( '\mu [ MeV / G ] ' ), /YLOG, $
        CHARTHICK = 3, CHARSIZE = 2
     TVLCT, 000, 255, 000, 000
     ;; TVLCT, 000, 128, 000, 000
     CONTOUR, period, rr, mu_MeVperG, $
              level = 1 / 0.003, $
              C_THICK = 3, C_LINESTYLE = 0, COLOR = 000, /OVER
     CONTOUR, period, rr, mu_MeVperG, $
              level = 2 / 0.003, $
              C_THICK = 3, C_LINESTYLE = 2, COLOR = 000, /OVER
     CONTOUR, period, rr, mu_MeVperG, $
              level = 3 / 0.003, $
              C_THICK = 3, C_LINESTYLE = 3, COLOR = 000, /OVER
     TVLCT, 255, 000, 255, 100
     CONTOUR, energy, rr, mu_MeVperG, $
              LEVEL = 1, $
              C_THICK = 3, C_LINESTYLE = 0, COLOR = 100, /OVER
     
     LOADCT, 0, /SILENT
     PLOT, time_interp, density, $
           XRANGE = [ 0, 6 ], /XS, $
           XTITLE = 'Simulation Time [ hours ]', XTHICK = 3, $
           YRANGE = [ 3, 7 ], /YS, $
           YTITLE = TEXTOIDL( 'n [ cm^{-3} ]' ), YTHICK = 3, $
           THICK = 3, CHARSIZE = 2, CHARTHICK = 2        
     MAKELINEY, time_interp( iTime0 ), LINESTYLE = 0, THICK = 3
     MAKELINEY, time_interp( iTime + min_average ), LINESTYLE = 1, THICK = 3
     MAKELINEY, time_interp( iTime + max_average ), LINESTYLE = 1, THICK = 3
     MAKELINEY, time_interp( iTime  ), LINESTYLE = 2, THICK = 3
     
     LOADCT, 70, /SILENT
     REVERSECT
     IMAGE_CONT, $
        DDERIV( rr, $
                REFORM( ALOG10( PSD_Average( iTime0, $
                                             *, mu_minloc : mu_maxloc ) ) + $
                        1d-15 ), /X ), $
        rr, mu_MeVperG( mu_minloc : mu_maxloc ), $
        MINVALUE = -alog10(50.), MAXVALUE = alog10(50.), $
        TITLE = TEXTOIDL( '\nabla_r PSD_0' ), $
        XTITLE = TEXTOIDL( 'L [ R_E ]' ), $
        YTITLE = TEXTOIDL( '\mu [ MeV / G ] ' ), /YLOG, $
        CHARTHICK = 3, CHARSIZE = 2
     TVLCT, 000, 255, 000, 000
     ;; TVLCT, 000, 128, 000, 000
     CONTOUR, period, rr, mu_MeVperG, $
              level = 1 / 0.003, $
              C_THICK = 3, C_LINESTYLE = 0, COLOR = 000, /OVER
     CONTOUR, period, rr, mu_MeVperG, $
              level = 2 / 0.003, $
              C_THICK = 3, C_LINESTYLE = 2, COLOR = 000, /OVER
     CONTOUR, period, rr, mu_MeVperG, $
              level = 3 / 0.003, $
              C_THICK = 3, C_LINESTYLE = 3, COLOR = 000, /OVER
     ;; CONTOUR, period, rr, mu_MeVperG, $
     ;;          LEVEL = 1 / 0.003, $
     ;;          C_THICK = 3, C_LINESTYLE = 0, COLOR = 000, /OVER
     TVLCT, 255, 000, 255, 100
     CONTOUR, energy, rr, mu_MeVperG, $
              LEVEL = 1, $
              C_THICK = 3, C_LINESTYLE = 0, COLOR = 100, /OVER
     
     LOADCT, 70, /SILENT
     REVERSECT
     IMAGE_CONT, $
        DDERIV( rr, $
                REFORM( ALOG10( PSD_Average( iTime , *, $
                                             mu_minloc : mu_maxloc ) ) + $
                        1d-15) , /X ), $
        rr, mu_MeVperG( mu_minloc : mu_maxloc ), $
        MINVALUE = -alog10(50.), MAXVALUE = alog10(50.), $
        TITLE = TEXTOIDL( '\nabla_r PSD_1' ), $
        XTITLE = TEXTOIDL( 'L [ R_E ]' ), $
        YTITLE = TEXTOIDL( '\mu [ MeV / G ] ' ), /YLOG, $
        CHARTHICK = 3, CHARSIZE = 2
     TVLCT, 000, 255, 000, 000
     ;; TVLCT, 000, 128, 000, 000
     CONTOUR, period, rr, mu_MeVperG, $
              level = 1 / 0.003, $
              C_THICK = 3, C_LINESTYLE = 0, COLOR = 000, /OVER
     CONTOUR, period, rr, mu_MeVperG, $
              level = 2 / 0.003, $
              C_THICK = 3, C_LINESTYLE = 2, COLOR = 000, /OVER
     CONTOUR, period, rr, mu_MeVperG, $
              level = 3 / 0.003, $
              C_THICK = 3, C_LINESTYLE = 3, COLOR = 000, /OVER
     ;; CONTOUR, period, rr, mu_MeVperG, $
     ;;          level = 1 / 0.003, $
     ;;          C_THICK = 3, C_LINESTYLE = 0, COLOR = 000, /OVER
     TVLCT, 255, 000, 255, 100
     CONTOUR, energy, rr, mu_MeVperG, $
              LEVEL = 1, $
              C_THICK = 3, C_LINESTYLE = 0, COLOR = 100, /OVER
     
     fmin = -00.
     fmax =  25.
     LOADCT, 33, /SILENT
     REVERSECT
     IMAGE_CONT, $
        dPSD( iTime, *, mu_minloc : mu_maxloc ), $
        rr, mu_MeVperG( mu_minloc : mu_maxloc ), $
        ;; MIN = fmin, MAX = fmax, $
        ;; TITLE = TEXTOIDL( 'PSD_1 / PSD_0 - 1' ), $
        XTITLE = TEXTOIDL( 'L [ R_E ]' ), $
        YTITLE = TEXTOIDL( '\mu [ MeV / G ] ' ), /YLOG, $
        CHARTHICK = 3, CHARSIZE = 2
     TVLCT, 000, 255, 000, 000
     ;; TVLCT, 000, 128, 000, 000
     CONTOUR, period, rr, mu_MeVperG, $
              level = 1 / 0.003, $
              C_THICK = 3, C_LINESTYLE = 0, COLOR = 000, /OVER
     CONTOUR, period, rr, mu_MeVperG, $
              level = 2 / 0.003, $
              C_THICK = 3, C_LINESTYLE = 2, COLOR = 000, /OVER
     CONTOUR, period, rr, mu_MeVperG, $
              level = 3 / 0.003, $
              C_THICK = 3, C_LINESTYLE = 3, COLOR = 000, /OVER
     ;; CONTOUR, period, rr, mu_MeVperG, $
     ;;          level = 1 / 0.003, $
     ;;          C_THICK = 3, C_LINESTYLE = 0, COLOR = 000, /OVER
     TVLCT, 255, 000, 255, 100
     CONTOUR, energy, rr, mu_MeVperG, $
              LEVEL = 1., $
              C_THICK = 3, C_LINESTYLE = 0, COLOR = 100, /OVER
     
     LOADCT, 0, /SILENT
     PLOT, $
        REFORM( r_sat( 0 , * ) ), ephi_window( *, 6 ), $
        XTITLE = TEXTOIDL( 'L [ R_E ]' ), $
        XRANGE = [ rmin, rmax ], $
        YTITLE = TEXTOIDL( 'PSD Amp. [ mV/m ]^2' ), $
        YRANGE = [1d-12,1d0 ], /YLOG, THICK = 3
     
     DEVICE, /CLOSE
     
  ENDFOR                        ; iTime loop

  SET_PLOT, 'X'
  CD, original_dir
  
END
