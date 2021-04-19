      program moddtm_test
!***********************************************************************
! s. bruinsma (22/10/20) 
!
! compile [double precision] dtm2020_F107_Kp-subr.f90 Mod_DTM2020F107Kp_Benchmark.f90
!
  parameter (nlatm=96)
  common/cons/pi,deupi,cdr,sard
  common/cles/iklm
  dimension par_ro(6),akp(4),fl(2),fml(2),fh(2),fmh(2)
  pi=acos(-1.)
  deupi=2.*pi
  cdr=pi/180.
  pis12=pi/12.
!
  open(10,file='DTM_2020_F107_Kp',status='old') 
  call lecdtm(10)	!read in coefficients
! *****************  input test values  ********************************
  altl=300.		
  alth=800
  alat=0.
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
  akp(1)=3.
  akp(2)=0.
  akp(3)=3.
  akp(4)=0.	
!
  write(*,*)'     Model DTM2020_F107_Kp'  
  write(*,*)'     Benchmark'
  write(*,*) 'tz  :    843.243   1312.266    844.099   1334.874'  
  write(*,*) 'tinf:    844.099   1334.875    844.099   1334.875' 
  write(*,*) 'ro  : 0.94719E-14 0.29053E-13 0.26600E-17 0.39104E-16' 
  write(*,*) 'this run:'  
! *********************************************************************
    call dtm3(day,fl,fml,akp,altl,hl,alat,xlon,tz_ll,tinf_ll,ro_ll,par_ro,wmm)
    call dtm3(day,fh,fmh,akp,altl,hl,alat,xlon,tz_lh,tinf_lh,ro_lh,par_ro,wmm)
    call dtm3(day,fl,fml,akp,alth,hl,alat,xlon,tz_hl,tinf_hl,ro_hl,par_ro,wmm)
    call dtm3(day,fh,fmh,akp,alth,hl,alat,xlon,tz_hh,tinf_hh,ro_hh,par_ro,wmm)
    !
    write(*,10) tz_ll,tz_lh,tz_hl,tz_hh
    write(*,11) tinf_ll,tinf_lh,tinf_hl,tinf_hh
    write(*,12) ro_ll,ro_lh,ro_hl,ro_hh
10 format('tz  :',4f11.3)   
11 format('tinf:',4f11.3)   
12 format('ro  :',4e12.5)   
  end program moddtm_test
