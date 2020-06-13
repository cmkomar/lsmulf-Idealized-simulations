pro FFT_Profiles, $

   run_number

  FORWARD_FUNCTION Analyze_Hartinger, REVERSECT
  
  CD, '~/Research/Hartinger_HSR/' + run_number + '/GM/', $
     CURRENT = original_dir
  
  ANALYZE_HARTINGER, $
     15, er, ephi, logtime, r_sat, $
     BTOT = btot
  
  n = 360
  x = FINDGEN( ( n - 1 ) / 2 ) + 1
  freq = [0.0, X, N/2, -N/2 + X]/(N*10.)*1000.
  
  iTime0 = 900
  iTime1 = itime0 + N - 1
  
  der_5Re = ts_diff( er(*, 16 ), 1 )
  dephi_5Re = ts_diff( ephi(*, 16 ), 1 )
  dbtot_5Re = ts_diff( btot(*, 16 ), 1 )
  
  der_geo = ts_diff( er(*, 29 ), 1 )
  dephi_geo = ts_diff( ephi(*, 29 ), 1 )
  dbtot_geo = ts_diff( btot(*, 29 ), 1 )
  
  er_5Re_fft=2.*abs(fft(der_5Re(iTime0 : iTime1)))^2
  ephi_5Re_fft=2.*abs(fft(dephi_5Re(iTime0 : iTime1)))^2
  btot_5Re_fft=2.*abs(fft(dbtot_5Re(iTime0 : iTime1)))^2
  
  er_geo_fft=2.*abs(fft(der_geo(iTime0 : iTime1)))^2
  ephi_geo_fft=2.*abs(fft(dephi_geo(iTime0 : iTime1)))^2
  btot_geo_fft=2.*abs(fft(dbtot_geo(iTime0 : iTime1)))^2
  
  
  SET_PLOT, 'PS'
  
  w,1,2
;; window,0,title='er@5Re'
  device, $
     filename=run_number+'er_at_5Re.eps', $
     xsize=10,ysize=10,/inches, $
     /encapsulated,bits_per_pixel=8,/color,/portrait
  plot,logtime(iTime0 : iTime1),der_5Re(iTime0 : iTime1), $
       title=TEXTOIDL('E_r @ 5.0 R_E'), $
       xtitle='Simulation Time [hours]', $
       ytitle=TEXTOIDL( '\Delta E_r [mV/m]'), $
       yrange = [ -0.4, 0.4 ], /ys, $
       thick=3,xthick=3,ythick=3,charthick=3
  makeliney,logtime(900),linestyle=2,thick=2
  makeliney,logtime(1259),linestyle=2,thick=2
  plot,freq(0:n/2),er_5Re_fft(0:n/2),/ylog, $
       xtitle = 'Frequency [mHz]', $
       ytitle = TEXTOIDL( 'PSD Amp. [mV/m]^2' ), $
       yrange=[1d-12,1d0],/ys, $
       thick=3,xthick=3,ythick=3,charthick=3
  device,/close
  
;; window,1,title='ephi@5Re'
  device, $
     filename=run_number+'ephi_at_5Re.eps', $
     xsize=10,ysize=10,/inches, $
     /encapsulated,bits_per_pixel=8,/color,/portrait
  plot,logtime(iTime0 : iTime1),dephi_5Re(iTime0 : iTime1), $
       title=TEXTOIDL('E_\phi @ 5.0 R_E'), $
       xtitle='Simulation Time [hours]', $
       ytitle=TEXTOIDL( '\Delta E_\phi [mV/m]'), $
       yrange = [ -0.6, 0.6 ], /ys, $
       thick=3,xthick=3,ythick=3,charthick=3
  makeliney,logtime(900),linestyle=2,thick=2
  makeliney,logtime(1259),linestyle=2,thick=2
  plot,freq(0:n/2),ephi_5Re_fft(0:n/2),/ylog, $
       xtitle = 'Frequency [mHz]', $
       ytitle = TEXTOIDL( 'PSD Amp. [mV/m]^2' ), $
       yrange=[1d-12,1d0],/ys, $
       thick=3,xthick=3,ythick=3,charthick=3
  device,/close
  
;; window,2,title='btot@5Re'
  device, $
     filename=run_number+'btot_at_5Re.eps', $
     xsize=10,ysize=10,/inches, $
     /encapsulated,bits_per_pixel=8,/color,/portrait
  plot,logtime(iTime0 : iTime1),dbtot_5Re(iTime0 : iTime1), $
       title=TEXTOIDL('B_{Tot} @ 5.0 R_E'), $
       xtitle='Simulation Time [hours]', $
       ytitle=TEXTOIDL( '\Delta B_{Tot} [nT]'), $
       yrange = [ -0.4, 0.4 ], /ys, $
       thick=3,xthick=3,ythick=3,charthick=3
  makeliney,logtime(900),linestyle=2,thick=2
  makeliney,logtime(1259),linestyle=2,thick=2
  plot,freq(0:n/2),btot_5Re_fft(0:n/2),/ylog, $
       xtitle = 'Frequency [mHz]', $
       ytitle = TEXTOIDL( 'PSD Amp. [nT]^2' ), $
       yrange=[1d-12,1d0],/ys, $
       thick=3,xthick=3,ythick=3,charthick=3
  device,/close
;stop
  
;; window,4,title='er@geo'
  device, $
     filename=run_number+'er_at_geo.eps', $
     xsize=10,ysize=10,/inches, $
     /encapsulated,bits_per_pixel=8,/color,/portrait
  plot,logtime(iTime0 : iTime1),der_geo(iTime0 : iTime1), $
       title=TEXTOIDL('E_r @ 6.6 R_E'), $
       xtitle='Simulation Time [hours]', $
       ytitle=TEXTOIDL( '\Delta E_r [mV/m]'), $
       yrange = [ -0.4, 0.4 ], /ys, $
       thick=3,xthick=3,ythick=3,charthick=3
  makeliney,logtime(900),linestyle=2,thick=2
  makeliney,logtime(1259),linestyle=2,thick=2
  plot,freq(0:n/2),er_geo_fft(0:n/2),/ylog, $
       xtitle = 'Frequency [mHz]', $
       ytitle = TEXTOIDL( 'PSD Amp. [mV/m]^2' ), $
       yrange=[1d-12,1d0],/ys, $
       thick=3,xthick=3,ythick=3,charthick=3
  device,/close
  
;; window,5,title='ephi@geo'
  device, $
     filename=run_number+'ephi_at_geo.eps', $
     xsize=10,ysize=10,/inches, $
     /encapsulated,bits_per_pixel=8,/color,/portrait
  plot,logtime(iTime0 : iTime1),dephi_geo(iTime0 : iTime1), $
       title=TEXTOIDL('E_\phi @ 6.6 R_E'), $
       xtitle='Simulation Time [hours]', $
       ytitle=TEXTOIDL( '\Delta E_\phi [mV/m]'), $
       yrange = [ -0.6, 0.6 ], /ys, $
       thick=3,xthick=3,ythick=3,charthick=3
  makeliney,logtime(900),linestyle=2,thick=2
  makeliney,logtime(1259),linestyle=2,thick=2
  plot,freq(0:n/2),ephi_geo_fft(0:n/2),/ylog, $
       xtitle = 'Frequency [mHz]', $
       ytitle = TEXTOIDL( 'PSD Amp. [mV/m]^2' ), $
       yrange=[1d-12,1d0],/ys, $
       thick=3,xthick=3,ythick=3,charthick=3
  device,/close
  
;; window,6,title='btot@geo'
  device, $
     filename=run_number+'btot_at_geo.eps', $
     xsize=10,ysize=10,/inches, $
     /encapsulated,bits_per_pixel=8,/color,/portrait
  plot,logtime(iTime0 : iTime1),dbtot_geo(iTime0 : iTime1), $
       title=TEXTOIDL('B_{Tot} @ 6.6 R_E'), $
       xtitle='Simulation Time [hours]', $
       ytitle=TEXTOIDL( '\Delta B_{Tot} [nT]'), $
       yrange = [ -0.4, 0.4 ], /ys, $
       thick=3,xthick=3,ythick=3,charthick=3
  makeliney,logtime(900),linestyle=2,thick=2
  makeliney,logtime(1259),linestyle=2,thick=2
  plot,freq(0:n/2),btot_geo_fft(0:n/2),/ylog, $
       xtitle = 'Frequency [mHz]', $
       ytitle = TEXTOIDL( 'PSD Amp. [nT]^2' ), $
       yrange=[1d-12,1d0],/ys, $
       thick=3,xthick=3,ythick=3,charthick=3
  device,/close
  set_plot,'x'

  cd,original_dir

end
