
SPAWN, 'ls 2*_e.psd > psdlist.txt'

nFiles = FILE_LINES( 'psdlist.txt' )
PRINT, "nFiles: ", nFiles

CLOSE, /ALL
filenames = STRARR( nFiles )

OPENR, psdlist_lun, 'psdlist.txt', /GET_LUN
READF, psdlist_lun, filenames
FREE_LUN, psdlist_lun
PRINT, filenames

OPENR, psd0_lun, filenames( 0 ), /GET_LUN
READF, psd0_lun, rc0, nr0, nmlt0, nm0, nk0, nTime
PRINT, "rc0, nr0, nmlt0, nm0, nk0, nTime: ", rc0, nr0, nmlt0, nm0, nk0, nTime

xk = FLTARR( nk0 )
xmm = FLTARR( nm0 )
lat = FLTARR( nmlt0 )
READF, psd0_lun, xk
READF, psd0_lun, xmm
READF, psd0_lun, lat
FREE_LUN, psd0_lun
CLOSE, /ALL

psd = FLTARR( nFiles, nr0, nmlt0 + 1, nm0 )
roa = FLTARR( nFiles, nr0, nmlt0 + 1 )
houra = FLTARR( nFiles )
mltoa = roa   

print,'Getting PSD:'

FOR $
   ;; iFile = 0, 0 $
   iFile = 0, nFiles - 1 $
DO BEGIN

   PRINT, filenames( iFile )
   OPENR, psd_lun, filenames( iFile ), /GET_LUN
   READF, psd_lun, rc, ir, ip, nm, nk       ; nm=number of mu grid, nk=number of K grid
   nr = ir                        ; number of radial grid
   nmlt = ip                      ; number of local-time grid
   xk_temp=fltarr(nk)    &   xmm_temp=fltarr(nm)   &   lat_temp=fltarr(ir)
   READF, psd_lun, xk_temp
   READF, psd_lun, xmm_temp
   READF, psd_lun, lat_temp

   ro = FLTARR( nr, nmlt + 1 )    &  mlto = ro  	& plspsd = ro
   psdy = FLTARR( nk )
   
   READF, psd_lun, hour
   houra( iFile ) = hour
   PRINT, iFile, hour
   
   FOR $
      iLat = 0, nr - 1 $
   DO BEGIN
      
      FOR $
         iMLT = 0, nmlt - 1 $
      DO BEGIN
         
         READF, psd_lun, lat1, mlt, ro1, mlto1, bo 
         roa( iFile, iLat, iMLT ) = ro1
         mltoa( iFile, iLat, iMLT ) = mlto1
         
         FOR $
            iMu = 0, nm - 1 $
         DO BEGIN
            
            READF, psd_lun, psdy
            psd( iFile, iLat, iMLT, iMu ) = psdy( 0 )
            
         ENDFOR                 ; iM loop
         
      ENDFOR                    ; iMLT loop
      
   ENDFOR                       ; iRadius loop
   
   mltoa( iFile, *, * ) = mltoa( iFile, *, * ) * !PI / 12. ; mlto in radian
                                ; periodic boundary condition
   roa( iFile, *, nmlt ) = roa( iFile, *, 0 )
   mltoa( iFile, *, nmlt ) = mltoa( iFile, *, 0 )
   psd( iFile, *, nmlt, * ) = psd( iFile, *, 0, * )

   FREE_LUN, psd_lun
   CLOSE, psd_lun
   
ENDFOR                          ; iTime loop

PRINT, 'SAVING'
SAVE, roa, mltoa, psd, houra, xk, xmm, lat, lat_temp, $
      FILENAME = 'PSD_reduced.sav'

PRINT, 'EXITING'

END
