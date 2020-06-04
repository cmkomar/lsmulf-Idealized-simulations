pro PSD_Master_Plot, $
; Created 2019-11-12
; Rebuilt 2020-03-16
;   
; Code that generates 7-plot multiplot comparing electron Phase space
; prior to wave driver turning on in the solar wind to a specific
; later time. Averages over Time window given by the AVERAGE_INDEX
; input flag.
;
; INPUT:
   
   run_number, $

; OUTPUT:
   
   psd_interp, time_interp, rr, xmm, $

; FLAGS:
   
   AVERAGE_INDEX = nTime_average, AE8MIN = AE8MIN_check, $
   ITIME1 = iTime1_input, TIME_AVERAGE = psd_time_average, $
   MLT_AVERAGE = PSD_MLT_AVERAGE

; Change to the run directory with the High Resolution (10s output)
  CD, run_number + '/IM_Lstar/', CURRENT = original_dir

; Sets the number of indices to average over.  
  IF NOT( KEYWORD_SET( nTime_average ) ) THEN $
     nTime_average = 33

  IF NOT( KEYWORD_SET( iTime1_input ) ) THEN $
     iTime1_input = 1080
  
; Load and calculate FFTs of the satellite data at 15 MLT
  ANALYZE_HARTINGER, $
     15, er_sat, ephi_sat, sattime, r_sat, $
     BTOT = btot

; Obtain the dimensions of the satellite variables
  dim_sat = SIZE( r_sat, /DIMENSIONS )
  nTime_sat = dim_sat( 0 )
  nR_sat = dim_sat( 1 )

; Use SWMF IDL tools to load the solar wind input file using
; SWMF/BATSRUS log data format
  GET_LOG, 'IMF.log', wlog0, wlognames, logtime0, timeunit
  CLEANLOG, logtime0, wlog0, logtime, wlog
  
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
  
; Interpolate the solar wind density data to 10s resolution.
  nTime_PSD = N_ELEMENTS( houra )
  time_interp = DINDGEN( nTime_PSD ) * 10. / 3600.

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
  energy = FLTARR( nR, nMu ) ; in keV
  period = FLTARR( nR, nMu ) ; in seconds

; Calculates the time period of particles for given mu (in keV / nT )
; at a given dipolar L-shell distance.
  E0 = 511. ; in keV / c / c
  FOR $
     iR = 0, nR - 1 $
  DO BEGIN ; iR loop begins

; Calculates the equatorial dipolar magnetic field strength at the
; given radial distance
     beq = 3.11e4 / rr( iR )^3 ; in nT

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
        period( iR, iMu ) = $ ; in seconds
           0.667 * 1.55e4 / rr( iR ) / gamma / beta / beta
        
     ENDFOR ; iMu loop
     
  ENDFOR ; iR loop
  
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
  dPSD_MLT_average = FLTARR( nTime, nR, nMu )
  
; Initial start time of the ULF wave driver packet in the solar wind.
  iTime0 = 535

; Begin loop over the entire simulation time to interpolate electron
; Phase Space Density to the regularly spaced regular grid and sum
; over MLT slice.

  PRINT, 'INTERPOLATING PSD IN R:'
  FOR $
     ;; iTime1 = iTime0, 1080, 1080 - iTime0 $
     ;; iTime1 = iTime0, nTime - 1 $
     iTime1 = 0, nTime - 1 $
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

        dPSD_MLT_average( iTime1, *, iMu ) = $
           ( PSD_MLT_average( iTime1, *, iMu ) / $
             PSD_MLT_average( iTime0, *, iMu ) - 1. ) * 100.
        
     ENDFOR ; iMu loop
     
  ENDFOR ; iTime loop

; Instantiates the PSD time averaging array and the time averaged dPSD
; array
  PSD_Time_average = FLTARR( nTime, nR, nMLT, nMu )
  dPSD_Time_Average = FLTARR( nTime, nR, nMLT, nMu )
  min_average = -ABS( nTime_average )
  max_average =  0

; Makes the directory to store all the PSD Master plots  
  SPAWN, 'mkdir -p Images/' + AE8_Condition + '/PSD_Master/'

; Calculates the time Averaged PSD
  PRINT, "CALCULATING TIME AVERAGE NOW: "
  FOR $
     iTime1 = iTime0, ( nTime - 1 ) $
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

        for $
           iMLT = 0, nMLT - 2 $
        DO BEGIN
           
           FOR $
              ;; iMu = 0, nMu - 1 $
              iMu = mu_minloc, mu_maxloc $
           DO BEGIN
           
              PSD_Time_average( iTime1, iRadius, iMLT, iMu ) = $
                 MEAN( PSD_interp( iTime1 - nTime_Average : iTime1, $
                                   iRadius, iMLT, iMu ) )

; Calculate the percent difference in time-averaged PSD
              dPSD_Time_average( iTime1, *, iMLT, iMu ) =  $
                 ( PSD_Time_average( iTime1, *, iMLT, iMu ) / $
                   PSD_Time_average( iTime0, *, iMLT, iMu ) - 1. ) * 100.

           ENDFOR ; iMu Loop

        ENDFOR ; iMLT loop

     ENDFOR ; iRadius loop


     ; Run the Analyze Hartinger code to get the FFT information
     FFT_Movie, $
        EPHI = ephi_window, ER = er_window, $
        TIME_IMAX = iTime1, FREQUENCIES = freq, $
        TIME_AVERAGE = nTime_average
     
     ;; PRINT,"freq: ", freq
     ;; STOP

; Set the PSD profile color table indices
; 16 - Purple (min) - Gold (max) color table
; 70 - Red (min) - Blue (max) color table     
     PSD_profile_CT_index = 16
     gradR_PSD_profile_CT_index = 70
     dPSD_profile_CT_index = 70
     
; Set the minimum and maximum log_10 PSD range
     psd_min = -10.
     psd_max =  01.

; Set the minimum and maximum r-gradient of PSD range
     gradr_max = -.01
     gradr_min =  .01

; Set the minimum and maximum PSD percent difference range     
     dPSD_min = -200.0
     dPSD_max =  200.0

; Change output to Postscript     
     SET_PLOT, 'PS'

; Setup the output file characteristics
     DEVICE, $
        FILENAME = $
        'Images/' + AE8_Condition + $
        '/PSD_Master/PSD_Master_Plot_MLT_average_' + $
        STRTRIM( STRING( iTime1, FORMAT = '(I04)'), 2) + '-1.eps', $
        XSIZE = 10., YSIZE = 30., /INCHES, $
        /COLOR,/ENCAPSULATED, /PORTRAIT, BITS_PER_PIXEL = 8

; Prepare the device to display eight plots     
     W, 1, 8

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;							;;;;;;;;
;;;;;;;;							;;;;;;;;
;;;;;;;;			PLOT 1				;;;;;;;;
;;;;;;;;							;;;;;;;;
;;;;;;;;	CONTOUR PLOT OF ITIME0 MLT-AVERAGED PSD		;;;;;;;;
;;;;;;;;							;;;;;;;;
;;;;;;;;							;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
     
; Load the color tabel for the overall PSD profile
     LOADCT, PSD_profile_CT_index, /SILENT
     
; Make a contour plot of the log_10 MLT-Averaged PSD at the initial
; time (iTime0) before the ULF-driver turns on.
     IMAGE_CONT, $
        ALOG10( PSD_MLT_Average( iTime0, *, $
                                 mu_minloc : mu_maxloc ) ), $
        rr, mu_MeVperG( mu_minloc : mu_maxloc ), $
        MINVALUE = psd_min, MAXVALUE = psd_max, $
        TITLE = TEXTOIDL( 'PSD_0' ), $
        XTITLE = TEXTOIDL( 'L [ R_E ]' ), $
        YTITLE = TEXTOIDL( '\mu [ MeV / G ] ' ), /YLOG, $
        CHARTHICK = 3, CHARSIZE = 2

; Load green color to display harmonic resonance curves
     TVLCT, 000, 255, 000, 000

; Plot the first harmonic resonance curve
     CONTOUR, period, rr, mu_MeVperG, $
              LEVEL = 1 / 0.003, $
              C_THICK = 3, C_LINESTYLE = 0, COLOR = 000, /OVER

; Plot the second harmonic resonance curve
     CONTOUR, period, rr, mu_MeVperG, $
              LEVEL = 2 / 0.003, $
              C_THICK = 3, C_LINESTYLE = 2, COLOR = 000, /OVER

; Plot the third harmonic resonance curve
     CONTOUR, period, rr, mu_MeVperG, $
              LEVEL = 3 / 0.003, $
              C_THICK = 3, C_LINESTYLE = 3, COLOR = 000, /OVER

; Load purple color to display the 1 MeV resonance curve     
     TVLCT, 255, 000, 255, 100
     CONTOUR, energy, rr, mu_MeVperG, $
              LEVEL = 1e3, $
              C_THICK = 3, C_LINESTYLE = 0, COLOR = 100, /OVER
     
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;							;;;;;;;;
;;;;;;;;							;;;;;;;;
;;;;;;;;			PLOT 2				;;;;;;;;
;;;;;;;;							;;;;;;;;
;;;;;;;;	CONTOUR PLOT OF ITIME1 MLT-AVERAGED PSD		;;;;;;;;
;;;;;;;;							;;;;;;;;
;;;;;;;;							;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Load the purple-gold color table
     LOADCT, PSD_profile_CT_index, /SILENT

; Make a contour plot of the log_10 MLT-Averaged PSD at the current
; time (iTime1) after the ULF-driver turns on.
     IMAGE_CONT, $
        ALOG10( PSD_MLT_Average( iTime1 , *, $
                                 mu_minloc : mu_maxloc ) ), $
        rr, mu_MeVperG( mu_minloc : mu_maxloc ), $
        MINVALUE = psd_min, MAXVALUE = psd_max, $
        TITLE = TEXTOIDL( 'PSD_1' ), $
        XTITLE = TEXTOIDL( 'L [ R_E ]' ), $
        YTITLE = TEXTOIDL( '\mu [ MeV / G ] ' ), /YLOG, $
        CHARTHICK = 3, CHARSIZE = 2

; Load green color to display harmonic resonance curves
     TVLCT, 000, 255, 000, 000

; Plot the first harmonic resonance curve
     CONTOUR, period, rr, mu_MeVperG, $
              LEVEL = 1 / 0.003, $
              C_THICK = 3, C_LINESTYLE = 0, COLOR = 000, /OVER

; Plot the second harmonic resonance curve
     CONTOUR, period, rr, mu_MeVperG, $
              LEVEL = 2 / 0.003, $
              C_THICK = 3, C_LINESTYLE = 2, COLOR = 000, /OVER

; Plot the third harmonic resonance curve
     CONTOUR, period, rr, mu_MeVperG, $
              LEVEL = 3 / 0.003, $
              C_THICK = 3, C_LINESTYLE = 3, COLOR = 000, /OVER

; Load purple color to display the 1 MeV resonance curve     
     TVLCT, 255, 000, 255, 100
     CONTOUR, energy, rr, mu_MeVperG, $
              LEVEL = 1e3, $
              C_THICK = 3, C_LINESTYLE = 0, COLOR = 100, /OVER
     
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;							;;;;;;;;
;;;;;;;;							;;;;;;;;
;;;;;;;;			PLOT 3				;;;;;;;;
;;;;;;;;							;;;;;;;;
;;;;;;;;	   PLOT OF SOLAR WIND DENSITY DRIVER		;;;;;;;;
;;;;;;;;							;;;;;;;;
;;;;;;;;							;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Load the B-W color table to make a plot of the ULF wave driver profile
     LOADCT, 0, /SILENT
     PLOT, time_interp, density, $
           XRANGE = [ 0, 6 ], /XS, $
           XTITLE = 'Simulation Time [ hours ]', XTHICK = 3, $
           YRANGE = [ 3, 7 ], /YS, $
           YTITLE = TEXTOIDL( 'n [ cm^{-3} ]' ), YTHICK = 3, $
           THICK = 3, CHARSIZE = 2, CHARTHICK = 2        

; Make a vertical line at the initial comparison time     
     MAKELINEY, time_interp( iTime0 ), LINESTYLE = 0, THICK = 3

; Make a vertical line at the final comparison time      
     MAKELINEY, time_interp( iTime1  ), LINESTYLE = 2, THICK = 3
     
;; ; Make vertical lines at the beginning and end of the time-averaging
;; ; window
     
     ;; MAKELINEY, time_interp( iTime1 + min_average ), LINESTYLE = 1, THICK = 3
     ;; MAKELINEY, time_interp( iTime1 + min_average ), LINESTYLE = 1, THICK = 3

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;							;;;;;;;;
;;;;;;;;							;;;;;;;;
;;;;;;;;			PLOT 4				;;;;;;;;
;;;;;;;;							;;;;;;;;
;;;;;;;;		 CONTOUR PLOT OF ITIME0			;;;;;;;;
;;;;;;;;							;;;;;;;;
;;;;;;;;	  RADIAL GRADIENT OF MLT-AVERAGED PSD		;;;;;;;;
;;;;;;;;							;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Load the red(min)-blue(max) color table     
     LOADCT, gradR_PSD_profile_CT_index, /SILENT

; Reverse the color table so that red is maximum
     REVERSECT

; Plot contour of the radial gradient of the initial (iTime0)
; MLT-Averaged PSD profile
     IMAGE_CONT, $
        DDERIV( $
        rr, $
        ;; REFORM( ALOG10( 1d-15 + PSD_MLT_Average( iTime0, *, $
        ;;                                                   mu_minloc : $
        ;;                                                   mu_maxloc ) ), $
        REFORM( PSD_MLT_Average( iTime0, *, $
                                 mu_minloc : $
                                 mu_maxloc ) ), $
        /X ), $
        rr, mu_MeVperG( mu_minloc : mu_maxloc ), $
        MINVALUE = gradR_min*1d4, MAXVALUE = gradR_max*1d4, $
        TITLE = TEXTOIDL( '\nabla_r PSD_0' ), $
        XTITLE = TEXTOIDL( 'L [ R_E ]' ), $
        YTITLE = TEXTOIDL( '\mu [ MeV / G ] ' ), /YLOG, $
        CHARTHICK = 3, CHARSIZE = 2

; Load green color to display harmonic resonance curves
     TVLCT, 000, 255, 000, 000

; Plot the first harmonic resonance curve
     CONTOUR, period, rr, mu_MeVperG, $
              LEVEL = 1 / 0.003, $
              C_THICK = 3, C_LINESTYLE = 0, COLOR = 000, /OVER

; Plot the second harmonic resonance curve
     CONTOUR, period, rr, mu_MeVperG, $
              LEVEL = 2 / 0.003, $
              C_THICK = 3, C_LINESTYLE = 2, COLOR = 000, /OVER

; Plot the third harmonic resonance curve
     CONTOUR, period, rr, mu_MeVperG, $
              LEVEL = 3 / 0.003, $
              C_THICK = 3, C_LINESTYLE = 3, COLOR = 000, /OVER

; Load purple color to display the 1 MeV resonance curve     
     TVLCT, 255, 000, 255, 100
     CONTOUR, energy, rr, mu_MeVperG, $
              LEVEL = 1e3, $
              C_THICK = 3, C_LINESTYLE = 0, COLOR = 100, /OVER
     
; Load the red(min)-blue(max) color table     
     LOADCT, gradR_PSD_profile_CT_index, /SILENT

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;							;;;;;;;;
;;;;;;;;							;;;;;;;;
;;;;;;;;			PLOT 5				;;;;;;;;
;;;;;;;;							;;;;;;;;
;;;;;;;;		 CONTOUR PLOT OF ITIME1			;;;;;;;;
;;;;;;;;							;;;;;;;;
;;;;;;;;	  RADIAL GRADIENT OF MLT-AVERAGED PSD		;;;;;;;;
;;;;;;;;							;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Plot contour of the radial gradient of the initial (iTime1)
; MLT-Averaged PSD profile
     IMAGE_CONT, $
        DDERIV( $
        rr, $
        ;; REFORM( ALOG10( 1d-15 + PSD_MLT_Average( iTime1, *, $
        ;;                                           mu_minloc : $
        ;;                                           mu_maxloc ) ), $
        REFORM( PSD_MLT_Average( iTime1, *, $
                                 mu_minloc : $
                                 mu_maxloc ) ), $
        /X ), $
        rr, mu_MeVperG( mu_minloc : mu_maxloc ), $
        MINVALUE = gradR_min*1d4, MAXVALUE = gradR_max*1d4, $
        TITLE = TEXTOIDL( '\nabla_r PSD_1' ), $
        XTITLE = TEXTOIDL( 'L [ R_E ]' ), $
        YTITLE = TEXTOIDL( '\mu [ MeV / G ] ' ), /YLOG, $
        CHARTHICK = 3, CHARSIZE = 2

; Load green color to display harmonic resonance curves
     TVLCT, 000, 255, 000, 000

; Plot the first harmonic resonance curve
     CONTOUR, period, rr, mu_MeVperG, $
              LEVEL = 1 / 0.003, $
              C_THICK = 3, C_LINESTYLE = 0, COLOR = 000, /OVER

; Plot the second harmonic resonance curve
     CONTOUR, period, rr, mu_MeVperG, $
              LEVEL = 2 / 0.003, $
              C_THICK = 3, C_LINESTYLE = 2, COLOR = 000, /OVER

; Plot the third harmonic resonance curve
     CONTOUR, period, rr, mu_MeVperG, $
              LEVEL = 3 / 0.003, $
              C_THICK = 3, C_LINESTYLE = 3, COLOR = 000, /OVER

; Load purple color to display the 1 MeV resonance curve     
     TVLCT, 255, 000, 255, 100
     CONTOUR, energy, rr, mu_MeVperG, $
              LEVEL = 1e3, $
              C_THICK = 3, C_LINESTYLE = 0, COLOR = 100, /OVER

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;							;;;;;;;;
;;;;;;;;							;;;;;;;;
;;;;;;;;			PLOT 6				;;;;;;;;
;;;;;;;;							;;;;;;;;
;;;;;;;;		 CONTOUR PLOT OF ITIME1			;;;;;;;;
;;;;;;;;							;;;;;;;;
;;;;;;;;	 PERCENT DIFFERENCE OF MLT-AVERAGED PSD		;;;;;;;;
;;;;;;;;							;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Load the precent difference color table
     LOADCT, dPSD_profile_CT_index, /SILENT

; Reverse the red-blue color table.
     REVERSECT

; Make a contour plot of the percent difference of the MLT-averaged
; PSD
     IMAGE_CONT, $
        dPSD_MLT_Average( iTime1, *, mu_minloc : mu_maxloc ), $
        rr, mu_MeVperG( mu_minloc : mu_maxloc ), $
        MIN = dPSD_min, MAX = dPSD_max, $
        TITLE = TEXTOIDL( 'PSD_1 / PSD_0 - 1' ), $
        XTITLE = TEXTOIDL( 'L [ R_E ]' ), $
        YTITLE = TEXTOIDL( '\mu [ MeV / G ] ' ), /YLOG, $
        CHARTHICK = 3, CHARSIZE = 2

; Load green color to display harmonic resonance curves
     TVLCT, 000, 255, 000, 000

; Plot the first harmonic resonance curve
     CONTOUR, period, rr, mu_MeVperG, $
              LEVEL = 1 / 0.003, $
              C_THICK = 3, C_LINESTYLE = 0, COLOR = 000, /OVER

; Plot the second harmonic resonance curve
     CONTOUR, period, rr, mu_MeVperG, $
              LEVEL = 2 / 0.003, $
              C_THICK = 3, C_LINESTYLE = 2, COLOR = 000, /OVER

; Plot the third harmonic resonance curve
     CONTOUR, period, rr, mu_MeVperG, $
              LEVEL = 3 / 0.003, $
              C_THICK = 3, C_LINESTYLE = 3, COLOR = 000, /OVER

; Load purple color to display the 1 MeV resonance curve     
     TVLCT, 255, 000, 255, 100
     CONTOUR, energy, rr, mu_MeVperG, $
              LEVEL = 1e3, $
              C_THICK = 3, C_LINESTYLE = 0, COLOR = 100, /OVER

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;							;;;;;;;;
;;;;;;;;							;;;;;;;;
;;;;;;;;			PLOT 7				;;;;;;;;
;;;;;;;;							;;;;;;;;
;;;;;;;;	LINE PLOT OF THE EPHI WAVE POWER AT 3 MHZ	;;;;;;;;
;;;;;;;;	  AS A FUNCTION OF L IN 15 MLT MERIDIAN		;;;;;;;;
;;;;;;;;							;;;;;;;;
;;;;;;;;							;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Load the black-white color tabel
     LOADCT, 0, /SILENT

; Make a line plot of the ULF wave power in the E_phi component at 3
; mHz along the 15 MLT meridian
     PLOT, $
        REFORM( r_sat( 0 , * ) ), ephi_window( *, 6 ), $
        XTITLE = TEXTOIDL( 'L [ R_E ]' ), $
        XRANGE = [ rmin, rmax ], $
        YTITLE = TEXTOIDL( 'PSD Amp. [ mV/m ]^2' ), $
        YRANGE = [1d-12,1d0 ], /YLOG, THICK = 3
     
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;							;;;;;;;;
;;;;;;;;							;;;;;;;;
;;;;;;;;			PLOT 8				;;;;;;;;
;;;;;;;;							;;;;;;;;
;;;;;;;;	 LINE PLOT OF THE ER WAVE POWER AT 3 MHZ	;;;;;;;;
;;;;;;;;	  AS A FUNCTION OF L IN 15 MLT MERIDIAN		;;;;;;;;
;;;;;;;;							;;;;;;;;
;;;;;;;;							;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Make a line plot of the ULF wave power in the E_r component at 3
; mHz along the 15 MLT meridian
     LOADCT, 0, /SILENT
     PLOT, $
        REFORM( r_sat( 0 , * ) ), er_window( *, 6 ), $
        XTITLE = TEXTOIDL( 'L [ R_E ]' ), $
        XRANGE = [ rmin, rmax ], $
        YTITLE = TEXTOIDL( 'PSD Amp. [ mV/m ]^2' ), $
        YRANGE = [1d-12,1d0 ], /YLOG, THICK = 3
     
     DEVICE, /CLOSE

; Begin plotting the time-averaged PSD profiles.
     DEVICE, $
        FILENAME = $
        'Images/' + AE8_Condition + $
        '/PSD_Master/PSD_Master_Plot_Time_average_' + $
        STRTRIM( STRING( nTime_average * 10., FORMAT = '(I05)'), 2) + $
        's_' + $
        STRTRIM( STRING( iTime1, FORMAT = '(I04)'), 2) + '-1.eps', $
        XSIZE = 10., YSIZE = 30., /INCHES, $
        /COLOR,/ENCAPSULATED, /PORTRAIT, BITS_PER_PIXEL = 8
     
     
; Prepare the device to display eight plots     
     W, 1, 8

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;							;;;;;;;;
;;;;;;;;							;;;;;;;;
;;;;;;;;			PLOT 1				;;;;;;;;
;;;;;;;;							;;;;;;;;
;;;;;;;;	CONTOUR PLOT OF ITIME0 TIME-AVERAGED PSD	;;;;;;;;
;;;;;;;;							;;;;;;;;
;;;;;;;;							;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
     
; Load the color tabel for the overall PSD profile
     LOADCT, PSD_profile_CT_index, /SILENT
     
; Make a contour plot of the log_10 Time-Averaged PSD at the initial
; time (iTime0) before the ULF-driver turns on.
     IMAGE_CONT, $
        ALOG10( PSD_Time_Average( iTime0, *, 0, $
                                  mu_minloc : mu_maxloc ) ), $
        rr, mu_MeVperG( mu_minloc : mu_maxloc ), $
        MINVALUE = psd_min, MAXVALUE = psd_max, $
        TITLE = TEXTOIDL( 'PSD_0' ), $
        XTITLE = TEXTOIDL( 'L [ R_E ]' ), $
        YTITLE = TEXTOIDL( '\mu [ MeV / G ] ' ), /YLOG, $
        CHARTHICK = 3, CHARSIZE = 2

; Load green color to display harmonic resonance curves
     TVLCT, 000, 255, 000, 000

; Plot the first harmonic resonance curve
     CONTOUR, period, rr, mu_MeVperG, $
              LEVEL = 1 / 0.003, $
              C_THICK = 3, C_LINESTYLE = 0, COLOR = 000, /OVER

; Plot the second harmonic resonance curve
     CONTOUR, period, rr, mu_MeVperG, $
              LEVEL = 2 / 0.003, $
              C_THICK = 3, C_LINESTYLE = 2, COLOR = 000, /OVER

; Plot the third harmonic resonance curve
     CONTOUR, period, rr, mu_MeVperG, $
              LEVEL = 3 / 0.003, $
              C_THICK = 3, C_LINESTYLE = 3, COLOR = 000, /OVER

; Load purple color to display the 1 MeV resonance curve     
     TVLCT, 255, 000, 255, 100
     CONTOUR, energy, rr, mu_MeVperG, $
              LEVEL = 1e3, $
              C_THICK = 3, C_LINESTYLE = 0, COLOR = 100, /OVER
     
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;							;;;;;;;;
;;;;;;;;							;;;;;;;;
;;;;;;;;			PLOT 2				;;;;;;;;
;;;;;;;;							;;;;;;;;
;;;;;;;;	CONTOUR PLOT OF ITIME1 TIME-AVERAGED PSD	;;;;;;;;
;;;;;;;;							;;;;;;;;
;;;;;;;;							;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Load the purple-gold color table
     LOADCT, PSD_profile_CT_index, /SILENT

; Make a contour plot of the log_10 Time-Averaged PSD at the current
; time (iTime1) after the ULF-driver turns on.
     IMAGE_CONT, $
        ALOG10( PSD_Time_Average( iTime1, *, 0, $
                                  mu_minloc : mu_maxloc ) ), $
        rr, mu_MeVperG( mu_minloc : mu_maxloc ), $
        MINVALUE = psd_min, MAXVALUE = psd_max, $
        TITLE = TEXTOIDL( 'PSD_1' ), $
        XTITLE = TEXTOIDL( 'L [ R_E ]' ), $
        YTITLE = TEXTOIDL( '\mu [ MeV / G ] ' ), /YLOG, $
        CHARTHICK = 3, CHARSIZE = 2

; Load green color to display harmonic resonance curves
     TVLCT, 000, 255, 000, 000

; Plot the first harmonic resonance curve
     CONTOUR, period, rr, mu_MeVperG, $
              LEVEL = 1 / 0.003, $
              C_THICK = 3, C_LINESTYLE = 0, COLOR = 000, /OVER

; Plot the second harmonic resonance curve
     CONTOUR, period, rr, mu_MeVperG, $
              LEVEL = 2 / 0.003, $
              C_THICK = 3, C_LINESTYLE = 2, COLOR = 000, /OVER

; Plot the third harmonic resonance curve
     CONTOUR, period, rr, mu_MeVperG, $
              LEVEL = 3 / 0.003, $
              C_THICK = 3, C_LINESTYLE = 3, COLOR = 000, /OVER

; Load purple color to display the 1 MeV resonance curve     
     TVLCT, 255, 000, 255, 100
     CONTOUR, energy, rr, mu_MeVperG, $
              LEVEL = 1e3, $
              C_THICK = 3, C_LINESTYLE = 0, COLOR = 100, /OVER
     
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;							;;;;;;;;
;;;;;;;;							;;;;;;;;
;;;;;;;;			PLOT 3				;;;;;;;;
;;;;;;;;							;;;;;;;;
;;;;;;;;	   PLOT OF SOLAR WIND DENSITY DRIVER		;;;;;;;;
;;;;;;;;							;;;;;;;;
;;;;;;;;							;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Load the B-W color table to make a plot of the ULF wave driver profile
     LOADCT, 0, /SILENT
     PLOT, time_interp, density, $
           XRANGE = [ 0, 6 ], /XS, $
           XTITLE = 'Simulation Time [ hours ]', XTHICK = 3, $
           YRANGE = [ 3, 7 ], /YS, $
           YTITLE = TEXTOIDL( 'n [ cm^{-3} ]' ), YTHICK = 3, $
           THICK = 3, CHARSIZE = 2, CHARTHICK = 2        

; Make a vertical line at the initial comparison time     
     MAKELINEY, time_interp( iTime0 ), LINESTYLE = 0, THICK = 3

; Make a vertical line at the final comparison time      
     MAKELINEY, time_interp( iTime1  ), LINESTYLE = 2, THICK = 3
     
;; ; Make vertical lines at the beginning and end of the time-averaging
;; ; window
     
     MAKELINEY, time_interp( iTime1 + min_average ), LINESTYLE = 1, THICK = 3

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;							;;;;;;;;
;;;;;;;;							;;;;;;;;
;;;;;;;;			PLOT 4				;;;;;;;;
;;;;;;;;							;;;;;;;;
;;;;;;;;		 CONTOUR PLOT OF ITIME0			;;;;;;;;
;;;;;;;;							;;;;;;;;
;;;;;;;;	  RADIAL GRADIENT OF TIME-AVERAGED PSD		;;;;;;;;
;;;;;;;;							;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Load the red(min)-blue(max) color table     
     LOADCT, gradR_PSD_profile_CT_index, /SILENT

; Reverse the color table so that red is maximum
     REVERSECT

; Plot contour of the radial gradient of the initial (iTime0)
; Time-Averaged PSD profile
     IMAGE_CONT, $
        DDERIV( $
        rr, $
        ;; REFORM( ALOG10( 1d-15 + PSD_Time_Average( iTime0, *, 0, $
        ;;                                           mu_minloc : $
        ;;                                           mu_maxloc ) ), $
        REFORM( PSD_Time_Average( iTime0, *, 0, $
                                  mu_minloc : $
                                  mu_maxloc ) ), $
        /X ), $
        rr, mu_MeVperG( mu_minloc : mu_maxloc ), $
        MINVALUE = gradR_min, MAXVALUE = gradR_max, $
        TITLE = TEXTOIDL( '\nabla_r PSD_0' ), $
        XTITLE = TEXTOIDL( 'L [ R_E ]' ), $
        YTITLE = TEXTOIDL( '\mu [ MeV / G ] ' ), /YLOG, $
        CHARTHICK = 3, CHARSIZE = 2

; Load green color to display harmonic resonance curves
     TVLCT, 000, 255, 000, 000

; Plot the first harmonic resonance curve
     CONTOUR, period, rr, mu_MeVperG, $
              LEVEL = 1 / 0.003, $
              C_THICK = 3, C_LINESTYLE = 0, COLOR = 000, /OVER

; Plot the second harmonic resonance curve
     CONTOUR, period, rr, mu_MeVperG, $
              LEVEL = 2 / 0.003, $
              C_THICK = 3, C_LINESTYLE = 2, COLOR = 000, /OVER

; Plot the third harmonic resonance curve
     CONTOUR, period, rr, mu_MeVperG, $
              LEVEL = 3 / 0.003, $
              C_THICK = 3, C_LINESTYLE = 3, COLOR = 000, /OVER

; Load purple color to display the 1 MeV resonance curve     
     TVLCT, 255, 000, 255, 100
     CONTOUR, energy, rr, mu_MeVperG, $
              LEVEL = 1e3, $
              C_THICK = 3, C_LINESTYLE = 0, COLOR = 100, /OVER
     
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;							;;;;;;;;
;;;;;;;;							;;;;;;;;
;;;;;;;;			PLOT 5				;;;;;;;;
;;;;;;;;							;;;;;;;;
;;;;;;;;		 CONTOUR PLOT OF ITIME1			;;;;;;;;
;;;;;;;;							;;;;;;;;
;;;;;;;;	  RADIAL GRADIENT OF TIME-AVERAGED PSD		;;;;;;;;
;;;;;;;;							;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Load the red(min)-blue(max) color table     
     LOADCT, gradR_PSD_profile_CT_index, /SILENT

; Reverse the color table so that red is maximum
     REVERSECT

; Plot contour of the radial gradient of the initial (iTime1)
; Time-Averaged PSD profile
     IMAGE_CONT, $
        DDERIV( $
        rr, $
        ;; REFORM( ALOG10( 1d-15 + PSD_Time_Average( iTime1, *, 0, $
        ;;                                           mu_minloc : $
        ;;                                           mu_maxloc ) ), $
        REFORM( PSD_Time_Average( iTime1, *, 0, $
                                  mu_minloc : $
                                  mu_maxloc ) ), $
        /X ), $
        rr, mu_MeVperG( mu_minloc : mu_maxloc ), $
        MINVALUE = gradR_min, MAXVALUE = gradR_max, $
        TITLE = TEXTOIDL( '\nabla_r PSD_1' ), $
        XTITLE = TEXTOIDL( 'L [ R_E ]' ), $
        YTITLE = TEXTOIDL( '\mu [ MeV / G ] ' ), /YLOG, $
        CHARTHICK = 3, CHARSIZE = 2

; Load green color to display harmonic resonance curves
     TVLCT, 000, 255, 000, 000

; Plot the first harmonic resonance curve
     CONTOUR, period, rr, mu_MeVperG, $
              LEVEL = 1 / 0.003, $
              C_THICK = 3, C_LINESTYLE = 0, COLOR = 000, /OVER

; Plot the second harmonic resonance curve
     CONTOUR, period, rr, mu_MeVperG, $
              LEVEL = 2 / 0.003, $
              C_THICK = 3, C_LINESTYLE = 2, COLOR = 000, /OVER

; Plot the third harmonic resonance curve
     CONTOUR, period, rr, mu_MeVperG, $
              LEVEL = 3 / 0.003, $
              C_THICK = 3, C_LINESTYLE = 3, COLOR = 000, /OVER

; Load purple color to display the 1 MeV resonance curve     
     TVLCT, 255, 000, 255, 100
     CONTOUR, energy, rr, mu_MeVperG, $
              LEVEL = 1e3, $
              C_THICK = 3, C_LINESTYLE = 0, COLOR = 100, /OVER

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;							;;;;;;;;
;;;;;;;;							;;;;;;;;
;;;;;;;;			PLOT 6				;;;;;;;;
;;;;;;;;							;;;;;;;;
;;;;;;;;		 CONTOUR PLOT OF ITIME1			;;;;;;;;
;;;;;;;;							;;;;;;;;
;;;;;;;;	 PERCENT DIFFERENCE OF TIME-AVERAGED PSD	;;;;;;;;
;;;;;;;;							;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Load the precent difference color table
     LOADCT, dPSD_profile_CT_index, /SILENT

; Reverse the red-blue color table.
     REVERSECT

; Make a contour plot of the percent difference of the Time-averaged
; PSD
     IMAGE_CONT, $
        dPSD_Time_Average( iTime1, *, 0, mu_minloc : mu_maxloc ), $
        rr, mu_MeVperG( mu_minloc : mu_maxloc ), $
        MIN = dPSD_min, MAX = dPSD_max, $
        TITLE = TEXTOIDL( 'PSD_1 / PSD_0 - 1' ), $
        XTITLE = TEXTOIDL( 'L [ R_E ]' ), $
        YTITLE = TEXTOIDL( '\mu [ MeV / G ] ' ), /YLOG, $
        CHARTHICK = 3, CHARSIZE = 2

; Load green color to display harmonic resonance curves
     TVLCT, 000, 255, 000, 000

; Plot the first harmonic resonance curve
     CONTOUR, period, rr, mu_MeVperG, $
              LEVEL = 1 / 0.003, $
              C_THICK = 3, C_LINESTYLE = 0, COLOR = 000, /OVER

; Plot the second harmonic resonance curve
     CONTOUR, period, rr, mu_MeVperG, $
              LEVEL = 2 / 0.003, $
              C_THICK = 3, C_LINESTYLE = 2, COLOR = 000, /OVER

; Plot the third harmonic resonance curve
     CONTOUR, period, rr, mu_MeVperG, $
              LEVEL = 3 / 0.003, $
              C_THICK = 3, C_LINESTYLE = 3, COLOR = 000, /OVER

; Load purple color to display the 1 MeV resonance curve     
     TVLCT, 255, 000, 255, 100
     CONTOUR, energy, rr, mu_MeVperG, $
              LEVEL = 1e3, $
              C_THICK = 3, C_LINESTYLE = 0, COLOR = 100, /OVER

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;							;;;;;;;;
;;;;;;;;							;;;;;;;;
;;;;;;;;			PLOT 7				;;;;;;;;
;;;;;;;;							;;;;;;;;
;;;;;;;;	LINE PLOT OF THE EPHI WAVE POWER AT 3 MHZ	;;;;;;;;
;;;;;;;;	  AS A FUNCTION OF L IN 15 MLT MERIDIAN		;;;;;;;;
;;;;;;;;							;;;;;;;;
;;;;;;;;							;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Load the black-white color tabel
     LOADCT, 0, /SILENT

; Make a line plot of the ULF wave power in the E_phi component at 3
; mHz along the 15 MLT meridian
     PLOT, $
        REFORM( r_sat( 0 , * ) ), ephi_window( *, 6 ), $
        XTITLE = TEXTOIDL( 'L [ R_E ]' ), $
        XRANGE = [ rmin, rmax ], $
        YTITLE = TEXTOIDL( 'PSD Amp. [ mV/m ]^2' ), $
        YRANGE = [1d-12,1d0 ], /YLOG, THICK = 3
     
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;							;;;;;;;;
;;;;;;;;							;;;;;;;;
;;;;;;;;			PLOT 8				;;;;;;;;
;;;;;;;;							;;;;;;;;
;;;;;;;;	 LINE PLOT OF THE ER WAVE POWER AT 3 MHZ	;;;;;;;;
;;;;;;;;	  AS A FUNCTION OF L IN 15 MLT MERIDIAN		;;;;;;;;
;;;;;;;;							;;;;;;;;
;;;;;;;;							;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Make a line plot of the ULF wave power in the E_r component at 3
; mHz along the 15 MLT meridian
     LOADCT, 0, /SILENT
     PLOT, $
        REFORM( r_sat( 0 , * ) ), er_window( *, 6 ), $
        XTITLE = TEXTOIDL( 'L [ R_E ]' ), $
        XRANGE = [ rmin, rmax ], $
        YTITLE = TEXTOIDL( 'PSD Amp. [ mV/m ]^2' ), $
        YRANGE = [1d-12,1d0 ], /YLOG, THICK = 3
     
     DEVICE, /CLOSE
     
  ENDFOR ; iTime1 loop

  SET_PLOT, 'X'
  CD, original_dir
  
END
