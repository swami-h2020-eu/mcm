      program moddtm_test
!***********************************************************************
! s. bruinsma (22/10/20) 
!
! compile [double precision] dtm2020_F30_Hp-subr.f90 Mod_DTM2020F30Hp60_Benchmark.f90
!
  parameter (nlatm=96)
  common/cons/pi,deupi,cdr,sard
  common/cles/iklm
  dimension par_ro(6),ap60(10),fl(2),fml(2),fh(2),fmh(2)
  pi=acos(-1.)
  deupi=2.*pi
  cdr=pi/180.
  pis12=pi/12.
!
  open(10,file='DTM_2020_F30_ap60',status='old') 
  call lecdtm(10)	!read in coefficients
! *****************  input test values  ********************************
  altl=300.		
  alth=800
  xlat=0.
  hl=3.1415
  day=180.
  xlon=0.
  fmh(1)=180.
  fh(1) =fmh(1)
  fmh(2)=0.
  fh(2)=0.		
  fml(1)=80.
  fl(1) =fml(1)
  fml(2)=0.
  fl(2)=0.		  
  ap60(1)=15.
  ap60(2)=15.
  ap60(3)=15.
  ap60(4)=15.
  ap60(5)=15.
  ap60(6)=15.
  ap60(7)=15.
  ap60(8)=15.  	
  ap60(9)=15.  	
  ap60(10)=15.  	
!
  write(*,*)'     Model DTM2020_F30_ap60'  
  write(*,*)'     Benchmark'
  write(*,*) 'tz  :    835.545   1280.280    836.285   1297.687'
  write(*,*) 'tinf:    836.285   1297.688    836.285   1297.688'
  write(*,*) 'ro  : 0.93545E-14 0.28889E-13 0.29765E-17 0.31045E-16'
  write(*,*) 'this run:'    
! *********************************************************************
    call dtm5(day,fl,fml,ap60,altl,hl,alat,xlon,tz_ll,tinf_ll,ro_ll,par_ro,wmm)
    call dtm5(day,fh,fmh,ap60,altl,hl,alat,xlon,tz_lh,tinf_lh,ro_lh,par_ro,wmm)
    call dtm5(day,fl,fml,ap60,alth,hl,alat,xlon,tz_hl,tinf_hl,ro_hl,par_ro,wmm)
    call dtm5(day,fh,fmh,ap60,alth,hl,alat,xlon,tz_hh,tinf_hh,ro_hh,par_ro,wmm)
    !
    write(*,10) tz_ll,tz_lh,tz_hl,tz_hh
    write(*,11) tinf_ll,tinf_lh,tinf_hl,tinf_hh
    write(*,12) ro_ll,ro_lh,ro_hl,ro_hh
10 format('tz  :',4f11.3)   
11 format('tinf:',4f11.3)   
12 format('ro  :',4e12.5)   

  end program moddtm_test
