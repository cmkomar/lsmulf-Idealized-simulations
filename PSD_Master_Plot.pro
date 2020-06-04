pro PSD_Master_Plot, $
; Created 2019-11-12
; Code that generates 7-plot multiplot comparing electron Phase space
; prior to wave driver turning on in the solar wind to a specific
; later time. Averages over Time window given by the AVERAGE_INDEX
; input flag.
;
; NEW IN VERSION 1.0: Added averaging over MLT - CMK
   
; INPUT:
   
   run_number, $

; OUTPUT:
   
   psd_interp, psd_MLT_Average, PSD_Time_and_MLT_Average, $
   time_interp, rr, xmm, $

; FLAGS:
   
   AVERAGE_INDEX = average_index, AE8MIN = AE8MIN_check, ITIME1 = iTime1_input

; Change to the run directory with the High Resolution (10s output)
  CD, run_number + '/IM_HiRes/', CURRENT = original_dir

; Sets the number of indices to average over.  
  IF NOT( KEYWORD_SET( average_index ) ) THEN $
     average_index = 30

  IF NOT( KEYWORD_SET( iTime1_input ) ) THEN $
     iTime1_input = 1080
  
  
; Load and calculate FFTs of the satellite data
  ANALYZE_HARTINGER, $
     15, er_sat, ephi_sat, sattime, r_sat, $
     BTOT = btot

; Obtain the dimensions of the satellite variables
  dim_sat = SIZE( r_sat, /DIMENSIONS )
  nTime_sat = dim_sat( 0 )
  nR_sat = dim_sat( 1 )

; Use SWMF IDL tools to load the solar wind input file using
; SWMF/BATSRUS log data format
  GET_LOG, 'IMF.log', wlog, wlognames, logtime, timeunit

; Load the reduced electron PSD data (Reduced using only the minimum
; K-value, i.e. equatorially trapped particles); Use if switch to load
; between AE8 initial conditions.

  IF $
     KEYWORD_SET( AE8MIN_check ) $
  THEN $
     AE8_condition = 'AE8MIN' $
  ELSE $
     AE8_condition = 'AE8MAX'

  RESTORE, 'PSD_reduced_' + AE8_Condition + '.sav'
  
; Currently hardcoded to interpolate the solar wind density data to
; 6-hour data with 10s resolution.
;  
; ####################################################################
;  
; NOTE: PROBABLY NEEDS TO BE MORE  ROBUST FOR ARBITRARY SIMULATION
; LENGTH
;  
; ####################################################################
  numtime = 2161
  time_interp = DINDGEN( numtime ) * 10. / 3600.

  density = INTERPOL( wlog(*,13), logtime - logtime( 0 ), time_interp )

; Sets up the interpolation parameters to interpolate electron PSD to
; regularly spaced radial distances
  rmin = 3.0
  rmax = 8.5
  dr = 0.125
  nR = ( rmax - rmin ) / dr + 1
  rr = FINDGEN( nR ) * dr + rmin

; Finds the dimensionality of the electron PSD array.
  
  dim = SIZE( psd, /DIMENSIONS )
  nTime = dim( 0 )
  nRoa = dim( 1 )
  nMLT = dim( 2 )
  nMu = dim( 3 )
  
; Instantiates the variables for the calculation of the period for
; energetic electrons in a dipolar magnetic field. Energy variable has
; units of [ keV ] and period has units in [ seconds ].
  energy = FLTARR( nR, nMu )
  period = FLTARR( nR, nMu )

; Calculates the time period of particles for given mu (in keV / nT )
; at a given dipolar L-shell distance.
  E0 = 511.
  FOR $
     iR = 0, nR - 1 $
  DO BEGIN ; iR loop begins

; Calculates the equatorial dipolar magnetic field strength at the
; given radial distance
     beq = 3.11e4 / rr( iR )^3

; Loops over mu     
     FOR $
        iMu = 0, nMu - 1 $
     DO BEGIN ; iMu loop
        
; Calculates the kinetic energy of the particles in keV.
        energy( iR, iMu ) = $
           SQRT( E0 ^ 2 + 2. * xmm( iMu ) * beq * E0 ) - E0

; Calculate the Kinetic to rest mass energy ratio: T     
        T = energy( iR, iMu ) / E0

; Calculate the relativistic correction factor: gamma
        gamma = T + 1

; Calculate the velocity to speed of light ratio: beta        
        beta = SQRT( T * ( T + 2 ) ) / ( T + 1 )

; Calculate the period (in seconds) of equatorially trapped electrons
; in a dipolar magnetic field (see Eq. 5 of Komar, C. M., Glocer, A.,
; Hartinger, M. D., et al. (2017). DOI:10.1002/2017JA024163 and
; references therein)
        period( iR, iMu ) = $
           0.667 * 1.55e4 / rr( iR ) / gamma / beta / beta
        
     ENDFOR ; iMu loop
     
  ENDFOR ; iR loop
  
  PRINT, 'INTERPOLATING PSD IN R:'

; Convert the xmm (CIMI output) array in [ keV / nT ] to
; 100 [ MeV / Gauss ]
  mu_MeVperG = xmm * 100.

; Set the boundaries of the mu array for displaying in the output plots.
  mu_minloc = MIN( WHERE( mu_MeVperG GE 2e1 ) )
  mu_maxloc = MAX( WHERE( mu_MeVperG LE 1e4 ) )

; Instantiates the variable for electron PSD array to interpolate to
; regularly space radial distances.
  psd_interp = FLTARR( nTime, nR, nMLT - 1, nMu )
  PSD_MLT_average = FLTARR( nTime, nR, nMu )
  
; Initial start time of the ULF wave driver packet in the solar wind.
  iTime0 = 535

; Begin loop over the entire simulation time to interpolate electron
; Phase Space Density to the regularly spaced regular grid and sum
; over MLT slice.
  FOR $
     iTime1 = iTime0, 1080, 1080 - iTime0 $
     ;; iTime1 = iTime0, nTime - 1 $
     ;; iTime1 = 0, nTime - 1 $
  DO BEGIN ; iTime loop
     
     PRINT, 'iTime1: ', iTime1

; Loop over the mu indices necessary for plotting
     FOR $
        ;; iMu = 17, 49 $
        iMu = 0, nMu - 1 $
        ;; iMu = mu_minloc, mu_maxloc $
     DO BEGIN ; iMu loop

; Loop over MLT indices to interpolate to regularly spaced radial grid
        FOR $
           ;; iMLT = 0, 0 $
           iMLT = 0, nMLT - 2 $
        DO BEGIN ; iMLT loop

           psd_interp( iTime1, *, iMLT, iMu ) = $
              10 ^ INTERPOL( ALOG10( psd( iTime1, *, iMLT, iMu ) ), $
                             roa( iTime1, *, 0 ), rr )
           
        ENDFOR ; iMLT loop                 

; Loop over radius to calculate MLT average

        FOR $
           iRadius = 0, nR - 1 $
        DO BEGIN

           psd_MLT_average( iTime1, iRadius, iMu ) = $
              MEAN( PSD_interp( iTime1, iRadius, 0 : nMLT - 2, iMu ) )
           
        ENDFOR ; iRadius loop
        
     ENDFOR ; iMu loop
     
  ENDFOR ; iTime loop

; Instantiates the PSD averaging array and the dPSD array 
  PSD_Time_and_MLT_average = FLTARR( nTime, nR, nMu )
  dPSD = FLTARR( nTime, nR, nMu )
  min_average = -average_index
  max_average =  average_index

; Makes the directory to store all the PSD Master plots  
  SPAWN, 'mkdir -p Images/' + AE8_Condition + '/PSD_Master/'
  
  FOR $
     iTime1 = iTime0, nTime - 200 $
     ;; iTime1 = iTime0, iTime0 $
     ;; iTime1 = iTime0, 1080, 1080 - iTime0 $
     ;; iTime1 = iTime0, iTime1_input, iTime1_input - iTime0 $
     ;; iTime1 = 1080, 1080 $
  DO BEGIN ; iTime1 loop
     
     PRINT,'################################'
     PRINT,'iTime1 = ' + STRTRIM( STRING( iTime1 ), 2 )
     PRINT,'################################'

     FOR $
        iRadius = 0, nR - 1 $
     DO BEGIN
        
        FOR $
           ;; iMu = 0, nMu - 1 $
           iMu = mu_minloc, mu_maxloc $
        DO BEGIN
           
           PSD_Time_and_MLT_average( iTime1, iRadius, iMu ) = $
              MEAN( PSD_MLT_average( iTime1 - Average_Index : $
                                     iTime1 + Average_Index, $
                                     iRadius, iMu ) )
        ENDFOR ; iMu Loop

     ENDFOR ; iRadius loop
     
     dPSD( iTime1, *, iMu ) =  $
        PSD_Time_and_MLT_average( iTime1, *, iMu ) / $
        PSD_Time_and_MLT_average( iTime0, *, iMu ) - 1.
     

     FFT_Movie, $
        EPHI = ephi_window, TIME_IMIN = iTime1, FREQUENCIES = freq
     
     ;; PRINT,"freq: ", freq
     ;; STOP
     
     SET_PLOT, 'PS'
     DEVICE, $
        FILENAME = 'Images/AE8MAX/PSD_Master/PSD_Master_Plot' + $
        STRTRIM( STRING( iTime1, FORMAT = '(I04)'), 2) + $
        '_min' + STRTRIM( STRING( -average_index, FORMAT = '(I04)'), 2) + $
        '_max' + STRTRIM( STRING(  average_index, FORMAT = '(I04)'), 2) + $
        '_new.eps', $
        XSIZE = 10., YSIZE = 10., /INCHES, $
        /COLOR,/ENCAPSULATED, /PORTRAIT, BITS_PER_PIXEL = 8

     psd_min = -10.
     psd_max =  01.

     gradr_max = -1.0
     gradr_min =  1.0

     dPSD_min = -2.0
     dPSD_max =  2.0

     W, 1, 7
     LOADCT, 16, /SILENT
     ;; REVERSECT
     IMAGE_CONT, $
        ALOG10( PSD_Time_And_MLT_Average( iTime0, *, $
                                          mu_minloc : mu_maxloc ) ), $
        rr, mu_MeVperG( mu_minloc : mu_maxloc ), $
        MINVALUE = psd_min, MAXVALUE = psd_max, $
        TITLE = TEXTOIDL( 'PSD_0' ), $
        XTITLE = TEXTOIDL( 'L [ R_E ]' ), $
        YTITLE = TEXTOIDL( '\mu [ MeV / G ] ' ), /YLOG, $
        CHARTHICK = 3, CHARSIZE = 2
     TVLCT, 000, 255, 000, 000
     ;; TVLCT, 000, 128, 000, 000
     CONTOUR, period, rr, mu_MeVperG, $
              LEVEL = 1 / 0.003, $
              C_THICK = 3, C_LINESTYLE = 0, COLOR = 000, /OVER
     CONTOUR, period, rr, mu_MeVperG, $
              LEVEL = 2 / 0.003, $
              C_THICK = 3, C_LINESTYLE = 2, COLOR = 000, /OVER
     CONTOUR, period, rr, mu_MeVperG, $
              LEVEL = 3 / 0.003, $
              C_THICK = 3, C_LINESTYLE = 3, COLOR = 000, /OVER
     ;; CONTOUR, period, rr, mu_MeVperG, $
     ;;          level = 1 / 0.003, $
     ;;          C_THICK = 3, C_LINESTYLE = 0, COLOR = 000, /OVER
     TVLCT, 255, 000, 255, 100
     CONTOUR, energy, rr, mu_MeVperG, $
              LEVEL = 1e3, $
              C_THICK = 3, C_LINESTYLE = 0, COLOR = 100, /OVER
     
     LOADCT, 16, /SILENT
     ;; REVERSECT
     IMAGE_CONT, $
        ALOG10( PSD_Time_And_MLT_Average( iTime1 , *, $
                                          mu_minloc : mu_maxloc ) ), $
        rr, mu_MeVperG( mu_minloc : mu_maxloc ), $
        MINVALUE = psd_min, MAXVALUE = psd_max, $
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
              LEVEL = 1e3, $
              C_THICK = 3, C_LINESTYLE = 0, COLOR = 100, /OVER
     
     LOADCT, 0, /SILENT
     PLOT, time_interp, density, $
           XRANGE = [ 0, 6 ], /XS, $
           XTITLE = 'Simulation Time [ hours ]', XTHICK = 3, $
           YRANGE = [ 3, 7 ], /YS, $
           YTITLE = TEXTOIDL( 'n [ cm^{-3} ]' ), YTHICK = 3, $
           THICK = 3, CHARSIZE = 2, CHARTHICK = 2        
     MAKELINEY, time_interp( iTime0 ), LINESTYLE = 0, THICK = 3
     MAKELINEY, time_interp( iTime1 + min_average ), LINESTYLE = 1, THICK = 3
     MAKELINEY, time_interp( iTime1 + max_average ), LINESTYLE = 1, THICK = 3
     MAKELINEY, time_interp( iTime1  ), LINESTYLE = 2, THICK = 3
     
     LOADCT, 70, /SILENT
     REVERSECT
     IMAGE_CONT, $
        DDERIV( rr, $
                REFORM( $
        ;; ALOG10( PSD_Time_And_MLT_Average( iTime0, *, $
        ;;                                   mu_minloc : $
        ;;                                   mu_maxloc ) ) + $
        PSD_Time_And_MLT_Average( iTime0, *, $
                                  mu_minloc : $
                                  mu_maxloc ) + $
                        1d-15 ), /X ), $
        rr, mu_MeVperG( mu_minloc : mu_maxloc ), $
        ;; MINVALUE = gradR_min, MAXVALUE = gradR_max, $
        ;; TITLE = TEXTOIDL( '\nabla_r PSD_0' ), $
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
              LEVEL = 1e3, $
              C_THICK = 3, C_LINESTYLE = 0, COLOR = 100, /OVER
     
     LOADCT, 70, /SILENT
     REVERSECT
     IMAGE_CONT, $
        DDERIV( rr, $
                REFORM( ALOG10( PSD_Time_And_MLT_Average( iTime1 , *, $
                                                          mu_minloc : $
                                                          mu_maxloc ) ) + $
                        1d-15) , /X ), $
        rr, mu_MeVperG( mu_minloc : mu_maxloc ), $
        MINVALUE = gradR_min, MAXVALUE = gradR_max, $
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
              LEVEL = 1e3, $
              C_THICK = 3, C_LINESTYLE = 0, COLOR = 100, /OVER
     
     LOADCT, 70, /SILENT
     REVERSECT
     IMAGE_CONT, $
        dPSD( iTime1, *, mu_minloc : mu_maxloc ), $
        rr, mu_MeVperG( mu_minloc : mu_maxloc ), $
        MIN = dPSD_min, MAX = dPSD_max, $
        TITLE = TEXTOIDL( 'PSD_1 / PSD_0 - 1' ), $
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
              LEVEL = 1e3, $
              C_THICK = 3, C_LINESTYLE = 0, COLOR = 100, /OVER
     
     LOADCT, 0, /SILENT
     PLOT, $
        REFORM( r_sat( 0 , * ) ), ephi_window( *, 6 ), $
        XTITLE = TEXTOIDL( 'L [ R_E ]' ), $
        XRANGE = [ rmin, rmax ], $
        YTITLE = TEXTOIDL( 'PSD Amp. [ mV/m ]^2' ), $
        YRANGE = [1d-12,1d0 ], /YLOG, THICK = 3
     
     DEVICE, /CLOSE
     
  ENDFOR ; iTime1 loop

  SET_PLOT, 'X'
  CD, original_dir
  
END
