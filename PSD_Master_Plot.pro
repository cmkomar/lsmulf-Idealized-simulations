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
   
   NTIME_AVERAGE = nTime_average, AE8MIN = AE8MIN_check, $
   ITIME1 = iTime1_input, $
   PSD_TIME_AVERAGE = psd_time_average, $
   PSD_NORMALIZED = PSD_normalized

; Change to the run directory with the High Resolution (10s output)
  CD, run_number + '/IM_Lstar_expanded/', CURRENT = original_dir

; Sets the number of indices to average over.  
  IF NOT( KEYWORD_SET( nTime_average ) ) THEN $
     nTime_average = 33

  IF NOT( KEYWORD_SET( iTime1_input ) ) THEN $
     iTime1_input = 1080
  
; Load and calculate FFTs of the satellite data at 15 MLT
;;   ANALYZE_HARTINGER, $
;;      15, er_sat, ephi_sat, sattime, r_sat, $
;;      BTOT = btot

; Obtain the dimensions of the satellite variables
;;   dim_sat = SIZE( r_sat, /DIMENSIONS )
;;   nTime_sat = dim_sat( 0 )
;;   nR_sat = dim_sat( 1 )

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

  logtime_interp = $
     INTERPOL( logtime - logtime( 0 ), logtime - logtime( 0 ), time_interp )
  
  density = $
     INTERPOL( wlog( *, 13 ), logtime - logtime( 0 ), time_interp )

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
  mu_minloc = MIN( WHERE( mu_MeVperG GE 3e2 ) )
  mu_maxloc = MAX( WHERE( mu_MeVperG LE 1e4 ) )

; Instantiates the variable for electron PSD array to interpolate to
; regularly space radial distances.
  psd_interp = FLTARR( nTime, nR, nMLT - 1, nMu )
  ;; PSD_MLT_average = FLTARR( nTime, nR, nMu )
  ;; dPSD_MLT_average = FLTARR( nTime, nR, nMu )
  
; Initial start time of the ULF wave driver packet in the solar wind.
  iTime0 = 535

; Instantiates the PSD time averaging array and the time averaged dPSD
; array
  PSD_Time_average = FLTARR( nTime, nR, nMLT, nMu )
  PSD_Normalized = FLTARR( nTime, nR, nMLT, nMu )
  dPSD_Time_Average = FLTARR( nTime, nR, nMLT, nMu )
  min_average = -ABS( nTime_average )
  max_average =  0

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
           iMLT = 0, 0 $
           ;; iMLT = 0, nMLT - 2 $
        DO BEGIN ; iMLT loop

           psd_interp( iTime1, *, iMLT, iMu ) = $
              10 ^ INTERPOL( ALOG10( psd( iTime1, *, iMLT, iMu ) ), $
                             roa( iTime1, *, 0 ), rr )

           IF $
              ( iTime1 ge nTime_average ) $
           THEN BEGIN

              ;; PRINT, "CALCULATING TIME AVERAGE NOW: "

; Calculates the time Averaged PSD
              FOR $
                 iRadius = 0, nR - 1 $
              DO $
                 PSD_Time_average( iTime1, iRadius, iMLT, iMu ) = $
                 	MEAN( PSD_interp( iTime1 - nTime_Average : iTime1, $
                                          iRadius, iMLT, iMu ) ) + $
                 1e-15

              PSD_Normalized( iTime1, *, iMLT, iMu ) =  $
                 PSD_Time_Average( iTime1, *, iMLT, iMu ) / $
                 MAX( PSD_Time_Average( iTime1, *, iMLT, iMu ) )
              
; Calculate the percent difference in time-averaged PSD
              dPSD_Time_average( iTime1, *, iMLT, iMu ) =  $
                 ( PSD_Time_average( iTime1, *, iMLT, iMu ) - $
                   PSD_Time_average( iTime0, *, iMLT, iMu ) ) / $
                 PSD_Time_average( iTime0, *, iMLT, iMu ) * 100.

           ENDIF

; End iMLT loop
        ENDFOR
                                
; End iMu loop        
     ENDFOR

  ;; ENDFOR ; iTime loop

     images_filepath = 'Images/PSD_Master/' + AE8_Condition + '/'
; Makes the directory to store all the PSD Master plots  
     SPAWN, 'mkdir -p ' + images_filepath

  ;; FOR $
  ;;    iTime1 = iTime0, ( nTime - 1 ) $
  ;;    ;; iTime1 = iTime0, iTime0 $
  ;;    ;; iTime1 = iTime0, 1080, 1080 - iTime0 $
  ;;    ;; iTime1 = iTime0, iTime1_input, iTime1_input - iTime0 $
  ;;    ;; iTime1 = 1080, 1080 $
  ;; DO BEGIN ; iTime1 loop

     IF $
        ( ( iTime1 GE nTime_average ) AND $
          ( iTime1 GE iTime0 ) ) $
     THEN BEGIN
        PRINT,'################################'
        PRINT,'iTime1 = ' + STRTRIM( STRING( iTime1 ), 2 )
        PRINT,'################################'
        
; Run the Analyze Hartinger code to get the FFT information
        ;; FFT_Movie, $
        ;;    EPHI = ephi_window, ER = er_window, $
        ;;    TIME_IMAX = iTime1, FREQUENCIES = freq, $
        ;;    TIME_AVERAGE = nTime_average
     
        ;; PRINT,"freq: ", freq

; Set the PSD profile color table indices
; 16 - Purple (min) - Gold (max) color table
; 70 - Red (min) - Blue (max) color table     
        gradR_PSD_profile_CT_index = 70
        dPSD_profile_CT_index = 70
        
; Set the minimum and maximum log_10 PSD range
        psd_normalized_min = -4.0
        psd_normalized_max =  0.0
        
; Set the minimum and maximum r-gradient of PSD range
        gradr_max = -1.000
        gradr_min =  1.000

; Set the minimum and maximum PSD percent difference range     
        dPSD_min = -030.0
        dPSD_max =  030.0
        
; Change output to Postscript     
        SET_PLOT, 'PS'

; Begin plotting the time-averaged PSD profiles.
        DEVICE, $
           FILENAME = $
           images_filepath + $
           'PSD_Master_Plot_' + AE8_condition + '_Time_average_' + $
           STRTRIM( STRING( nTime_average * 10., FORMAT = '(I05)'), 2) + $
           's_' + $
           STRTRIM( STRING( iTime1, FORMAT = '(I04)'), 2) + '-4.eps', $
           XSIZE = 10., YSIZE = 17., /INCHES, $
           /COLOR,/ENCAPSULATED, /PORTRAIT, BITS_PER_PIXEL = 8
     
; Prepare the device to output 6 plots plots     
        numx_subplots = 1
        x_start = 0.100
        x_end   = 0.850
        x_space = 0.001
        x_subplot_size = $
           ( x_end - x_start - $
             ( numx_subplots - 1 ) * x_space ) / $
           numx_subplots

        x_colorbar_start = 0.875
        x_colorbar_end = 0.900

        numy_subplots = 6
        y_start = 0.020
        y_end   = 0.970
        y_space = 0.035
        y_subplot_size = $
           ( y_end - y_start - $
             ( numy_subplots - 1 ) * y_space ) / $
           numy_subplots

        
        line_thick = 3.
        c_line_thick = 3.
        
        char_thick = 3.
        char_size = 2.

        cb_char_size = 1.33
        
        ytick_length = -0.025
        
        xyouts_char_size = 1.25
        xyouts_char_thick = 3.0

        W, numx_subplots, numy_subplots + 4
        
        min_density_plot = 3.
        max_density_plot = 7.
        delta_density_plot = 0.25
        numy_density_plot = $
           ( max_density_plot - min_density_plot ) / $
           	delta_density_plot + 1

        density_plot_arr = $
           FINDGEN( numy_density_plot ) * delta_density_plot + min_density_plot
        
        slider = $
           INTARR( nTime_PSD, numy_density_plot ) + 128
        slider( iTime0 - nTime_Average : iTime0, * ) = 128 * 0.75
        slider( iTime1 - nTime_Average : iTime1, * ) = 128 * 1.25

        ny = numy_subplots
        LOADCT, 70, /SILENT
        IMAGE_CONT, $
           slider, logtime_interp, density_plot_arr, $
           TITLE = '', $
           MINVAL = 0, MAXVAL = 255, $
           XTITLE = 'Simulation Time [ hours ]', $
           XRANGE = [ 0, 6 ], /XS, $
           XTICKS = 12, XTHICK = char_thick, $
           YTITLE = TEXTOIDL("n_{SW} [ cm^{-3} ]"), $
           YRANGE = [ 3, 7 ], /YS, $
           YTICKS = 4, YMINOR = 4, YTHICK = char_thick, $
           CHARSIZE = char_size, CHARTHICK = char_thick, $
           POSITION = $
           [ x_start, $
             y_start + ( ny - 1 ) * y_subplot_size + ( ny - 0 ) * y_space, $
             x_start + x_subplot_size, $
             y_start + ( ny - 0 ) * y_subplot_size + ( ny - 1 ) * y_space ], $
           /NORM

        LOADCT, 0, /SILENT
        PLOT, $
           logtime_interp, density, $
           XTITLE = 'Simulation Time [ hours ]', $
           XRANGE = [ 0, 6 ], /XS, $
           XTICKS = 12, XTHICK = char_thick, $
           YTITLE = TEXTOIDL("n_{SW} [ cm^{-3} ]"), $
           YRANGE = [ 3, 7 ], /YS, $
           YTICKS = 4, YMINOR = 4, YTHICK = char_thick, $
           THICK = line_thick, CHARSIZE = char_size, CHARTHICK = char_thick, $
           POSITION = $
           [ x_start, $
             y_start + ( ny - 1 ) * y_subplot_size + ( ny - 0 ) * y_space, $
             x_start + x_subplot_size, $
             y_start + ( ny - 0 ) * y_subplot_size + ( ny - 1 ) * y_space ], $
           /NORM
        XYOUTS, $
           logtime_interp( iTime0 - 0.5 * nTime_Average ), $
           6.5, $
           TEXTOIDL( 't_{0}' ), $
           CHARSIZE = xyouts_char_size, $
           CHARTHICK = xyouts_char_thick, /DATA
        XYOUTS, $
           logtime_interp( iTime1 - 0.5 * nTime_Average ), $
           6.5, $
           TEXTOIDL( 't_{1}' ), $
           CHARSIZE = xyouts_char_size, $
           CHARTHICK = xyouts_char_thick, /DATA
        ny = ny - 1
        
        LOADCT, 3, /SILENT
        IMAGE_CONT, $
           ALOG10( PSD_Normalized( iTime0, *, 0, mu_minloc : mu_maxloc ) ), $
           ;; PSD_Normalized( iTime0, *, 0, mu_minloc : mu_maxloc ), $
           rr, xmm( mu_minloc : mu_maxloc ) * 100, $
           MIN = psd_normalized_min, MAX = psd_normalized_max, $
           TITLE = TEXTOIDL( 'Normalized PSD_{0}' ), $
           XTITLE = TEXTOIDL( 'L [ R_E ]' ), $
           YTITLE = TEXTOIDL( '\mu [ MeV / G ] ' ), /YLOG, $
           XTHICK = char_thick, $
           YTHICK = char_thick, $
           CHARSIZE = char_size, CHARTHICK = char_thick, $
           YTICKLEN = ytick_length, $
           POSITION = $
           [ x_start, $
             y_start + ( ny - 1 ) * y_subplot_size + ( ny - 0 ) * y_space, $
             x_start + x_subplot_size, $
             y_start + ( ny - 0 ) * y_subplot_size + ( ny - 1 ) * y_space ], $
           /NORM

        COLORBAR, $
           /VERTICAL, /RIGHT, $
           TITLE = TEXTOIDL( 'Normalized PSD_0 [ A. U. ]' ), $
           RANGE = 10^[ psd_normalized_min, psd_normalized_max ], $
           FORMAT = "(E7.1)", DIVISIONS = 4, $
           /YLOG, YMINOR = 9, YTICKLEN = -0.1, $
           ;; RANGE = [ psd_normalized_min, psd_normalized_max ], $
           ;; FORMAT = "(F3.1)", DIVISIONS = 5, $
           CHARTHICK = char_thick, CHARSIZE = cb_char_size, $
           POSITION = $
           [ x_colorbar_start, $
             y_start + ( ny - 1 ) * y_subplot_size + ( ny - 0 ) * y_space, $
             x_colorbar_end, $
             y_start + ( ny - 0 ) * y_subplot_size + ( ny - 1 ) * y_space ], $
           /NORM
        ny = ny - 1

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;							;;;;;;;;
;;;;;;;;							;;;;;;;;
;;;;;;;;			PLOT 1				;;;;;;;;
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
           ;; REFORM( PSD_Time_Average( iTime0, *, 0, $
           ;;                           mu_minloc : $
           ;;                           mu_maxloc ) ), $
           REFORM( PSD_Normalized( iTime0, *, 0, mu_minloc : mu_maxloc ) ), $
           /X ), $
           rr, mu_MeVperG( mu_minloc : mu_maxloc ), $
           MINVALUE = gradR_min, MAXVALUE = gradR_max, $
           TITLE = TEXTOIDL( '\nabla_r PSD_0' ), $
           XTITLE = TEXTOIDL( 'L [ R_E ]' ), $
           YTITLE = TEXTOIDL( '\mu [ MeV / G ] ' ), /YLOG, $
           XTHICK = line_thick, YTHICK = line_thick, $
           CHARSIZE = char_size, CHARTHICK = char_thick, $
           YTICKLEN = ytick_length, $
           POSITION = $
           [ x_start, $
             y_start + ( ny - 1 ) * y_subplot_size + ( ny - 0 ) * y_space, $
             x_start + x_subplot_size, $
             y_start + ( ny - 0 ) * y_subplot_size + ( ny - 1 ) * y_space ], $
           /NORM

        COLORBAR, $
           /VERTICAL, /RIGHT, $
           TITLE = TEXTOIDL( '\nabla_r Normalized PSD_0 [ A. U. ]' ), $
           RANGE = [ gradR_max, gradR_min ], $
           FORMAT = "(F4.1)", DIVISIONS = 8, $
           CHARTHICK = char_thick, CHARSIZE = cb_char_size, $
           POSITION = $
           [ x_colorbar_start, $
             y_start + ( ny - 1 ) * y_subplot_size + ( ny - 0 ) * y_space, $
             x_colorbar_end, $
             y_start + ( ny - 0 ) * y_subplot_size + ( ny - 1 ) * y_space ], $
           /NORM

; Load green color to display harmonic resonance curves
        TVLCT, 000, 255, 000, 000
        
; Plot the first harmonic resonance curve
        CONTOUR, period, rr, mu_MeVperG, $
                 LEVEL = 1 / 0.003, $
                 C_THICK = c_line_thick, C_LINESTYLE = 0, COLOR = 000, /OVER
        
; Plot the second harmonic resonance curve
        CONTOUR, period, rr, mu_MeVperG, $
                 LEVEL = 2 / 0.003, $
                 C_THICK = c_line_thick, C_LINESTYLE = 2, COLOR = 000, /OVER
        
; Plot the third harmonic resonance curve
        CONTOUR, period, rr, mu_MeVperG, $
                 LEVEL = 3 / 0.003, $
                 C_THICK = c_line_thick, C_LINESTYLE = 3, COLOR = 000, /OVER
        
; Load purple color to display the 1 MeV resonance curve     
        TVLCT, 255, 000, 255, 100
        CONTOUR, energy, rr, mu_MeVperG, $
                 LEVEL = 1e3, $
                 C_THICK = c_line_thick, C_LINESTYLE = 0, COLOR = 100, /OVER
        
        LOADCT, 0, /SILENT
        PLOT, $
           rr, mu_MeVperG( mu_minloc : mu_maxloc ), $
           COLOR = 0, $
           TITLE = TEXTOIDL( '\nabla_r PSD_0' ), $
           XTITLE = TEXTOIDL( 'L [ R_E ]' ), /XS, $
           YTITLE = TEXTOIDL( '\mu [ MeV / G ] ' ), /YS, /YLOG, $
           XTHICK = line_thick, YTHICK = line_thick, $
           THICK = line_thick, CHARSIZE = char_size, CHARTHICK = char_thick, $
           YTICKLEN = ytick_length, $
           POSITION = $
           [ x_start, $
             y_start + ( ny - 1 ) * y_subplot_size + ( ny - 0 ) * y_space, $
             x_start + x_subplot_size, $
             y_start + ( ny - 0 ) * y_subplot_size + ( ny - 1 ) * y_space ], $
           /NORM, /NODATA
        ny = ny - 1
        
        LOADCT, 3, /SILENT
        IMAGE_CONT, $
           ALOG10( PSD_Normalized( iTime1, *, 0, mu_minloc : mu_maxloc ) ), $
           ;; PSD_Normalized( iTime1, *, 0, mu_minloc : mu_maxloc ), $
           rr, xmm( mu_minloc : mu_maxloc ) * 100, $
           MIN = psd_normalized_min, MAX = psd_normalized_max, $
           TITLE = TEXTOIDL( 'Normalized PSD_{1}' ), $
           XTITLE = TEXTOIDL( 'L [ R_E ]' ), $
           YTITLE = TEXTOIDL( '\mu [ MeV / G ] ' ), $
           XTHICK = char_thick, $
           YTHICK = char_thick, /YLOG, $
           CHARSIZE = char_size, CHARTHICK = char_thick, $
           YTICKLEN = ytick_length, $
           POSITION = $
           [ x_start, $
             y_start + ( ny - 1 ) * y_subplot_size + ( ny - 0 ) * y_space, $
             x_start + x_subplot_size, $
             y_start + ( ny - 0 ) * y_subplot_size + ( ny - 1 ) * y_space ], $
           /NORM

        COLORBAR, $
           /VERTICAL, /RIGHT, $
           TITLE = TEXTOIDL( 'Normalized PSD_1 [ A. U. ]' ), $
           RANGE = 10^[ psd_normalized_min, psd_normalized_max ], $
           FORMAT = "(E7.1)", DIVISIONS = 4, $
           /YLOG, YMINOR = 9, YTICKLEN = -0.1, $
           ;; RANGE = [ psd_normalized_min, psd_normalized_max ], $
           ;; FORMAT = "(F3.1)", DIVISIONS = 5, $
           CHARTHICK = char_thick, CHARSIZE = cb_char_size, $
           POSITION = $
           [ x_colorbar_start, $
             y_start + ( ny - 1 ) * y_subplot_size + ( ny - 0 ) * y_space, $
             x_colorbar_end, $
             y_start + ( ny - 0 ) * y_subplot_size + ( ny - 1 ) * y_space ], $
           /NORM
        ny = ny - 1
           

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;							;;;;;;;;
;;;;;;;;							;;;;;;;;
;;;;;;;;			PLOT 2				;;;;;;;;
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
           ;; REFORM( PSD_Time_Average( iTime1, *, 0, $
           ;;                           mu_minloc : $
           ;;                           mu_maxloc ) ), $
           REFORM( PSD_Normalized( iTime1, *, 0, mu_minloc : mu_maxloc ) ), $
           /X ), $
           rr, mu_MeVperG( mu_minloc : mu_maxloc ), $
           MINVALUE = gradR_min, MAXVALUE = gradR_max, $
           TITLE = TEXTOIDL( '\nabla_r PSD_1' ), $
           XTITLE = TEXTOIDL( 'L [ R_E ]' ), $
           YTITLE = TEXTOIDL( '\mu [ MeV / G ] ' ), /YLOG, $
           XTHICK = line_thick, YTHICK = line_thick, $
           CHARSIZE = char_size, CHARTHICK = char_thick, $
           YTICKLEN = ytick_length, $
           POSITION = $
           [ x_start, $
             y_start + ( ny - 1 ) * y_subplot_size + ( ny - 0 ) * y_space, $
             x_start + x_subplot_size, $
             y_start + ( ny - 0 ) * y_subplot_size + ( ny - 1 ) * y_space ], $
           /NORM

        COLORBAR, $
           /VERTICAL, /RIGHT, $
           TITLE = TEXTOIDL( '\nabla_r Normalized PSD_1 [ A. U. ]' ), $
           RANGE = [ gradR_max, gradR_min ], $
           FORMAT = "(F4.1)", DIVISIONS = 8, $
           CHARTHICK = char_thick, CHARSIZE = cb_char_size, $
           POSITION = $
           [ x_colorbar_start, $
             y_start + ( ny - 1 ) * y_subplot_size + ( ny - 0 ) * y_space, $
             x_colorbar_end, $
             y_start + ( ny - 0 ) * y_subplot_size + ( ny - 1 ) * y_space ], $
           /NORM
        
; Load green color to display harmonic resonance curves
        TVLCT, 000, 255, 000, 000
        
; Plot the first harmonic resonance curve
        CONTOUR, period, rr, mu_MeVperG, $
                 LEVEL = 1 / 0.003, $
                 C_THICK = c_line_thick, C_LINESTYLE = 0, COLOR = 000, /OVER
        
; Plot the second harmonic resonance curve
        CONTOUR, period, rr, mu_MeVperG, $
                 LEVEL = 2 / 0.003, $
                 C_THICK = c_line_thick, C_LINESTYLE = 2, COLOR = 000, /OVER
        
; Plot the third harmonic resonance curve
        CONTOUR, period, rr, mu_MeVperG, $
                 LEVEL = 3 / 0.003, $
                 C_THICK = c_line_thick, C_LINESTYLE = 3, COLOR = 000, /OVER
        
; Load purple color to display the 1 MeV resonance curve     
        TVLCT, 255, 000, 255, 100
        CONTOUR, energy, rr, mu_MeVperG, $
                 LEVEL = 1e3, $
                 C_THICK = c_line_thick, C_LINESTYLE = 0, COLOR = 100, /OVER

        LOADCT, 0, /SILENT
        PLOT, $
           rr, mu_MeVperG( mu_minloc : mu_maxloc ), $
           COLOR = 0, $
           TITLE = TEXTOIDL( '\nabla_r PSD_1' ), $
           XTITLE = TEXTOIDL( 'L [ R_E ]' ), /XS, $
           YTITLE = TEXTOIDL( '\mu [ MeV / G ] ' ), /YS, /YLOG, $
           XTHICK = line_thick, YTHICK = line_thick, $
           THICK = line_thick, CHARSIZE = char_size, CHARTHICK = char_thick, $
           YTICKLEN = ytick_length, $
           POSITION = $
           [ x_start, $
             y_start + ( ny - 1 ) * y_subplot_size + ( ny - 0 ) * y_space, $
             x_start + x_subplot_size, $
             y_start + ( ny - 0 ) * y_subplot_size + ( ny - 1 ) * y_space ], $
           /NORM, /NODATA
        ny = ny - 1

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;							;;;;;;;;
;;;;;;;;							;;;;;;;;
;;;;;;;;			PLOT 3				;;;;;;;;
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
           TITLE = TEXTOIDL( '% Difference'), $
           XTITLE = TEXTOIDL( 'L [ R_E ]' ), $
           YTITLE = TEXTOIDL( '\mu [ MeV / G ] ' ), /YLOG, $
           XTHICK = line_thick, YTHICK = line_thick, $
           CHARSIZE = char_size, CHARTHICK = char_thick, $
           YTICKLEN = ytick_length, $
           POSITION = $
           [ x_start, $
             y_start + ( ny - 1 ) * y_subplot_size + ( ny - 0 ) * y_space, $
             x_start + x_subplot_size, $
             y_start + ( ny - 0 ) * y_subplot_size + ( ny - 1 ) * y_space ], $
           /NORM

        COLORBAR, $
           /VERTICAL, /RIGHT, $
           TITLE = TEXTOIDL( '% Difference [ Unitless ]' ), $
           RANGE = [ dPSD_min, dPSD_max ], $
           FORMAT = "(I3)", DIVISIONS = 6, $
           CHARTHICK = char_thick, CHARSIZE = cb_char_size, $
           POSITION = $
           [ x_colorbar_start, $
             y_start + ( ny - 1 ) * y_subplot_size + ( ny - 0 ) * y_space, $
             x_colorbar_end, $
             y_start + ( ny - 0 ) * y_subplot_size + ( ny - 1 ) * y_space ], $
           /NORM
        
; Load green color to display harmonic resonance curves
        TVLCT, 000, 255, 000, 000
        
; Plot the first harmonic resonance curve
        CONTOUR, period, rr, mu_MeVperG, $
                 LEVEL = 1 / 0.003, $
                 C_THICK = c_line_thick, C_LINESTYLE = 0, COLOR = 000, /OVER
        
; Plot the second harmonic resonance curve
        CONTOUR, period, rr, mu_MeVperG, $
                 LEVEL = 2 / 0.003, $
                 C_THICK = c_line_thick, C_LINESTYLE = 2, COLOR = 000, /OVER
        
; Plot the third harmonic resonance curve
        CONTOUR, period, rr, mu_MeVperG, $
                 LEVEL = 3 / 0.003, $
                 C_THICK = c_line_thick, C_LINESTYLE = 3, COLOR = 000, /OVER
        
; Load purple color to display the 1 MeV resonance curve     
        TVLCT, 255, 000, 255, 100
        CONTOUR, energy, rr, mu_MeVperG, $
                 LEVEL = 1e3, $
                 C_THICK = c_line_thick, C_LINESTYLE = 0, COLOR = 100, /OVER

        LOADCT, 0, /SILENT
        PLOT, $
           rr, mu_MeVperG( mu_minloc : mu_maxloc ), $
           COLOR = 0, $
           TITLE = TEXTOIDL( '% Difference'), $
           XTITLE = TEXTOIDL( 'L [ R_E ]' ), $
           YTITLE = TEXTOIDL( '\mu [ MeV / G ] ' ), /YS, /YLOG, $
           CHARSIZE = char_size, CHARTHICK = char_thick, $
           YTICKLEN = ytick_length, $
           POSITION = $
           [ x_start, $
             y_start + ( ny - 1 ) * y_subplot_size + ( ny - 0 ) * y_space, $
             x_start + x_subplot_size, $
             y_start + ( ny - 0 ) * y_subplot_size + ( ny - 1 ) * y_space ], $
           /NORM, /NODATA
        ny = ny - 1
        
        DEVICE, /CLOSE

; End If check for nTime_Average
     ENDIF

; End iTime1 loop     
  ENDFOR

  SET_PLOT, 'X'
  CD, original_dir
  
END
