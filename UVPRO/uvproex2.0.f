      program uvproEX

C     NILU-UV Data Processing Program 

C     EXTENDED VERSION 18.12.2002. OZONE ALSO DEROVED FROM
C                      340/305  380/305 313/305 CHANNEL PAIRS

C     Version 1.0    06.10.2001

C     Author: Arne Dahlback

C     Extracts total ozone, UV doses and cloud transmissions
C     from NILU-UV raw data. Accepts only raw data with
c     file names and format as produced by UVDAC.
c     If you have not used UVDAC to collect data from the
c     instrument you must use CONVLOG to convert the files
c     to the required file names and format



      PARAMETER(NOZONE=36,MINZ=0,MAXZ=95)
    

      REAL NA(NOZONE,MINZ:MAXZ),NB(NOZONE,MINZ:MAXZ),
     $     Nc(NOZONE,MINZ:MAXZ),Nd(NOZONE,MINZ:MAXZ),
     $     Ne(NOZONE,MINZ:MAXZ)
      REAL F(MINZ:MAXZ), o3store(1440)

      real dcoef(20,6), cr(6)


      character calfile*14, serial*3, no(5), yr*2
      character chlat*7,chlong*7, o3pair*7, o3clim*13
      character*2 chmon(12), chday(31), century*2

 

      character*9 actname(20)
      character*10  uintname(20),uintname2(20)
      character*30 comment(20)
 

      data no/'a','b','c','d','e'/

      data chmon/'01','02','03','04','05','06','07','08',
     $           '09','10','11','12'/
      data chday/'01','02','03','04','05','06','07','08',
     $           '09','10','11','12','13','14','15','16',
     $           '17','18','19','20','21','22','23','24',
     $           '25','26','27','28','29','30','31'/


 
C     inputdir='/nadir/norway/noroz/data/brewer/andoya/20'
C        open(1,file=inputdir//yy//'/UQ'//julian//yy//'.'//instr,
C     $   status='old',err=791)



      do iact=1,20
         do j = 1,6
            dcoef(iact,j) = 0.0
         end do
      end do

c     read  site info
      open(1,file='site.inp',status='old',err=488)
      go to 489
 488  continue

      call skipline(6)

      write(6,*) '***Error:  Cannot find site.inp' 
      write(6,*)
      write(6,*) 'Hit ENTER-key to end program'
      read(5,*)
      write(6,*)
      write(6,*) 'stop'
      stop
 489  continue

      read(1,*)
      read(1,*) rlat
      if(rlat.lt. -90.0 .or. rlat .gt. 90.0) then
        call skipline(6)
        write(6,*) '***Error: Illegal latitude. Edit file site.inp'
        write(6,*)
        write(6,*) 'Hit ENTER-key to end program'
        read(5,*)
        write(6,*)
        write(6,*) 'stop'
      stop
      end if

      read(1,*)
      read(1,*) rlong
      if(rlat.lt. -180.0 .or. rlat .gt. 180.0) then
        call skipline(6)
        write(6,*) '***Error: Illegal longitude. Edit file site.inp'
        write(6,*)
        write(6,*) 'Hit ENTER-key to end program'
        read(5,*)
        write(6,*)
        write(6,*) 'stop'
      stop
      end if

      read(1,*)
      read(1,*) rheight
      read(1,*)
      read(1,13) calfile
      close(1)



      open(1,file='usr.inp',err=600)
      go to 601
 600  continue
      call skipline(6)
      write(6,*) '***Error:  Cannot find file usr.inp' 
      write(6,*)
      write(6,*) 'Hit ENTER-key to end program'
      read(5,*)
      write(6,*)
      write(6,*) 'stop'
      stop
 601  continue
      read(1,*)
      read(1,*) iusrnoon
      if(iusrnoon.ne.2) iusrnoon=1
      read(1,*) 
      read(1,*) usrnoon
      if(usrnoon.lt.0.0 .or. usrnoon.gt.24.0) usrnoon=12.0
      read(1,*) 
      read(1,*) usrtdiff1
      if(usrtdiff1.le.0.0 .or. usrtdiff1.gt.24.0) usrtdiff1=1.0
      read(1,*)
      read(1,*) o3scale1
      read(1,*)
      read(1,*) o3scale2
      read(1,*)
      read(1,*) clto3
      read(1,*)
      read(1,*) szao3 
      read(1,*)
      read(1,*) cltscale1
      read(1,*)
      read(1,*) cltscale2
      read(1,*)
      read(1,*) iprof
      if(iprof.lt.1 .or. iprof .gt.3) iprof=2
      read(1,*)
      read(1,*) irato3
      if(irato3 .lt. 1 .or. irato3.gt.5) irato3=2
      read(1,*)
      read(1,*) iauto3
      if(iauto3.lt.1 .or. iauto3.gt.2) iauto3=2
      read(1,*)
      read(1,*) zswitcho3pair
      read(1,*)
      read(1,*) iclt
      if(iclt.lt.1 .or. iclt .gt.2) iclt = 1
      read(1,*)
      read(1,*) idateformat
      if(idateformat.lt.1 .or. idateformat.gt.2) idateformat=1
      read(1,*)
      read(1,*) timeintegr
      t=timeintegr
      j = 0
      if(t.eq. 1.0) j=1
      if(t.eq. 2.0) j=1
      if(t.eq. 5.0) j=1
      if(t.eq. 15.0) j=1
      if(j.eq.0) then
         write(6,*) t
         do i = 1,6
            write(6,*)
         end do
         write(6,*) 
     $ '*****Error: Illegal NILU-UV integration time in USR.INP file'
         write(6,*)
         write(6,*) 'Hit ENTER-key to end program'
         read(5,*)
         stop
      end if
      
      if(irato3.eq.1) o3scale = o3scale1
      if(irato3.eq.2) o3scale = o3scale2

c extended
      if(irato3.gt.2) o3scale = 1.0

      if(iclt.eq.1) cltscale=cltscale1
      if(iclt.eq.2) cltscale=cltscale2

      if(iprof.eq.1) o3clim='Low  Latitude'
      if(iprof.eq.2) o3clim='Mid  Latitude'
      if(iprof.eq.3) o3clim='High Latitude'
      close(1)


c     read calibration file given in site.inp
      open(1,file=calfile,status='old',err=498)
      go to 499
 498  continue

      call skipline(6)
      write(6,70) calfile
      write(6,*)
      write(6,*) 'Hit ENTER-key to end program'
      read(5,*)
      write(6,*)
      write(6,*) 'stop'
      stop      
 499  continue

      do i=1,8
         read(1,*)
      end do
      read(1,3) serial
      read(1,*)
      read(1,*) 
      read(1,*) istrayd
      if(istrayd.lt.1 .or. istrayd .gt.2) istrayd = 1
      read(1,*)
      read(1,*) nact
      read(1,*)
      do i = 1,nact
        read(1,9) actname(i)
        read(1,30) comment(i)
        read(1,10) uintname(i)
        read(1,10) uintname2(i)
        read(1,*) (dcoef(i,j), j=1,6)
      end do
      
      read(1,*)
      read(1,*) drk1,drk2,drk3,drk4,drk5,drk6
      read(1,*) 
      read(1,*) rawcal1,rawcal2,rawcal3,rawcal4,rawcal5,rawcal6
      read(1,*)
      read(1,*) ref1,ref2,ref3,ref4,ref5,ref6
      read(1,*)
      read(1,*) a1,b1,c1
      read(1,*)
      read(1,*) a2,b2,c2
      do i = 1,5
         read(1,*)
      end do

c read model calculated channel ratios  (later used to calculate ozone
c     in subroutine deriveozone )
      do ii = 1,36 
         do jj = 0,95
            read(1,*) ioz,iz,r32L,r31L,r41L,r51L,r21L,
     $                       r32M,r31M,r41M,r51M,r21M,
     $                       r32H,r31H,r41H,r51H,r21H
            if(iprof.eq.1) r1=r32L
            if(iprof.eq.1) r2=r31L
            if(iprof.eq.1) r3=r41L
            if(iprof.eq.1) r4=r51L
            if(iprof.eq.1) r5=r21L
           

            if(iprof.eq.2) r1=r32M
            if(iprof.eq.2) r2=r31M
            if(iprof.eq.2) r3=r41M
            if(iprof.eq.2) r4=r51M
            if(iprof.eq.2) r5=r21M
           


            if(iprof.eq.3) r1=r32H
            if(iprof.eq.3) r2=r31H
            if(iprof.eq.3) r3=r41H
            if(iprof.eq.3) r4=r51H
            if(iprof.eq.3) r5=r21H
         

            i=1+ioz/20        
            na(i,iz) = r1
            nb(i,iz) = r2
            nc(i,iz) = r3
            nd(i,iz) = r4
            ne(i,iz) = r5
           

         end do
      end do

c read model calculated clear-sky raw counts. Later used to
c calculate clt
      read(1,*)
      do i = 0,95
        read(1,*) iz,r1,r2
        if(iclt.eq.1) f(iz)=r1
        if(iclt.eq.2) f(iz)=r2
      end do
      close(1)



C Use station longitude to calculate 'hnoon'. Later the 
C averages are calculated in a time interval around the time 'hnoon'.
      hnoon = 12.0 -rlong/15.0
      tdiff1 = 1.0
C If iusrnoon=1 then use  user defined time for calculaton of averages
      if(iusrnoon .ne. 1) then
         hnoon = usrnoon
         tdiff1 = usrtdiff1
      end if



      tdiff = tdiff1/2.0

c averages will be calculated in the time interval
c (hnoon-tdiff, hnoon+tdiff)

       

 35   continue

      call skipline(20)
      write(6,*) 
     $   '        NILU-UV Data Processing Program V1.0 extended'
      call skipline(5)
      write(6,*) 'Available action spectra:'
      write(6,*)

      do i = 1, nact
        write(6,19) actname(i),i
      end do

      write(6,*)
      write(6,20) nact
      read(5,*,err=2000) iusract

      if(iusract.lt.1 .or. iusract.gt.nact) then
        call skipline(3)
        write(6,*) '****Error: Illegal value'
        write(6,*) 'Try again'
        call skipline(1)
        write(6,*) 'Hit ENTER-key to continue'
        read(5,*)
        call skipline(6)
        go to 35
      end if

 25   continue

      call skipline(20)
      write(6,*) 'Specify year and day(s) to be processed'
      call skipline(2)
 
      write(6,*) 'year (yyyy):'
      read(5,*,err=2000) iym

      if(iym.lt.1000 .or. iym.gt.9999) then
         call skipline(6)
         write(6,*) '**** Error: Illegal format. Use yyyy'
         write(6,*) 
         write(6,*) 'Please try again'
         write(6,*)
         write(6,*) 'Hit ENTER to continue'
         read(5,*)
         write(6,*)
         go to 25
      end if
      
      iyx=int(iym/100)*100
      iy=iym-iyx

      if(idateformat.eq.1) then
        write(6,*) 'Start at day number (jjj):'
        read(5,*,err=2000) julsrt
        call julcheck(iy,julsrt)
        call jul2date(iy,julsrt,monthstart,idaystart)
        write(6,*) 'End day number (jjj):'
        read(5,*,err=2000) julend
        call julcheck(iy,julend)
        call jul2date(iy,julend,monthend,idayend)
      end if


      if(idateformat.ne.1) then
         write(6,*) 'Start date (day month):'
         read(5,*,err=2000) idaystart,monthstart
         call daymonthcheck(idaystart,monthstart,iy)
         call date2jul(iy,julsrt,monthstart,idaystart) 
         write(6,*) 'End date (day month):'
         read(5,*,err=2000) idayend,monthend
         call daymonthcheck(idayend,monthend,iy)
         call date2jul(iy,julend,monthend,idayend)
      end if
 
      call skipline(3)
      write(6,*) 'Create summary file only?     (yes=1 no=2) :'
      read(5,*,err=2000) idetail
      if(idetail .lt.1 .or. idetail .gt. 2) idetail = 1

      iy1 = int(iy/10.0)
      iy2 = iy - iy1*10
      yr=char(iy1+48)//char(iy2+48)

      open(2,file='./results/summary.'//serial)

      write(2,400) serial
      write(2,*)
      write(2,111)
      chlong  = ' (West)'
      chlat = '(South)'
      if(rlat.ge.0.0)  chlat='(North)'
      if(rlong.ge.0.0) chlong=' (East)'
      write(2,420) rlat,chlat
      write(2,421) rlong,chlong
      write(2,422) calfile
      write(2,424) rheight
      write(2,425) hnoon-tdiff,hnoon+tdiff
      write(2,426) iclt+3
      write(2,429) cltscale
      write(2,436) clto3
 436  format
     $('Ozone average contain only measurements with CLT >',f6.2)
      write(2,437) szao3
 437  format
     $('Ozone average contain only measurements with SZA <',f6.2)
      if(iauto3.eq.1) then
        if(irato3.eq.1) o3pair='ch3/ch2'
        if(irato3.eq.2) o3pair='ch3/ch1'
        if(irato3.eq.3) o3pair='ch4/ch1'
        if(irato3.eq.4) o3pair='ch5/ch1'
        if(irato3.eq.5) o3pair='ch2/ch1'
        write(2,427) o3pair
      else
        write(2,423) zswitcho3pair
      end if
      write(2,428) o3scale
      write(2,434) o3clim
      write(2,438) actname(iusract),comment(iusract)
      write(2,439)
      write(2,*)
      write(2,*)
      write(2,430) 
      write(2,410) 
      write(2,440) uintname(iusract),uintname(iusract),
     $uintname2(iusract)

      numfile = 0

      id1 = int(idaystart/10.0)
      id2 = idaystart - id1*10
      id3 = idayend/10
      id4 = idayend - id3*10

      im1 = monthstart/10
      im2 = monthstart - im1*10
      im3 = monthend/10
      im4 = monthend - im3*10
       
      iy1 = int(iym/1000)
      iy2 = int(iym - iy1*1000)/100
      iy3 = (iym - iy1*1000 - iy2*100)/10
      iy4 = iym  -iy1*1000 -iy2*100 -iy3*10

      century = char(iy1+48)//char(iy2+48)

      call skipline(20)

      write(6,800) char(id1+48)//char(id2+48),
     $char(im1+48)//char(im2+48),char(iy1+48)//char(iy2+48)//
     $char(iy3+48)//char(iy4+48),
     $char(id3+48)//char(id4+48),
     $char(im3+48)//char(im4+48),char(iy1+48)//char(iy2+48)//
     $char(iy3+48)//char(iy4+48)




      call skipline(2)
      write(6,*) 'working ...'
      call skipline(4)

c if julsrt, julend is not in chronological order
      if(julsrt .gt. julend) then
          jultmp = julsrt
          julsrt = julend
          julend = jultmp
      end if

      do 1000 jul = julsrt,julend

c account for drift
      call drift(cr,iym,jul,serial,1,iid,iim,iiy)

      call jul2date(iy,jul,mon,id)

      jino = 0
      sig = 0.0
      sigo3 = 0.0
      sumo3 = 0.0
      dratesum = 0.0
      sum340 = 0.0
      fmsum = 0.0
      dratem = 0.0
      rtimeold = 0.0
      o3n = 0.0
      draten = 0.0
      doseday = 0.0
      drate=0.0
      clt=0.0
      cltn = 0.0
      drateold=0.0
      nn = 0
      no3 = 0
      do i = 1,1440
         o3store(i) = 0.0
      end do


      do 900 ino = 1,5

      open(1,file=
C     $'./data/R'//chday(id)//chmon(mon)//yr//no(ino)//'.'//serial,
C     $status='old', err = 902)
     $'./data/R'//chday(id)//chmon(mon)//yr//no(ino)//'.'//serial,
     $status='old', err = 902)
      go to 901
 902  continue
      open(1,file=
     $'./data/r'//chday(id)//chmon(mon)//yr//no(ino)//'.'//serial,
     $status='old', err = 900)
 901  continue
      numfile = numfile+1

c    if jino=1 open detailed output file ('pr' files). 
     
      jino = jino + 1
      
      if(jino.eq.1 .and. idetail.eq.2) then 
        open(3,file=
     $  './results/pr'//century//yr//chmon(mon)//chday(id)
     $  //'.'//serial)

        write(3,110) serial
        write(3,*)
        write(3,111) 
        write(3,420) rlat,chlat
        write(3,421) rlong,chlong
        write(3,422) calfile
        write(3,424) rheight
        write(3,426) iclt+3
        write(3,429) cltscale
        write(3,427) o3pair
        write(3,428) o3scale
        write(3,442) cr(1),cr(2),cr(3),cr(4),cr(5),cr(6)
        write(3,434) o3clim
        write(3,438) actname(iusract),comment(iusract)
        write(3,*)
        write(3,120) 
        write(3,130) uintname(iusract)

      end if


 1    read(1,100,end=900, err=1) iy,im,id,ih,min,is,f300,f305,
     $f320,f340,f380,parn,tmp






      f300 = (f300-drk1)*ref1/cr(1)
      f305 = (f305-drk2)*ref2/cr(2)
      f320 = (f320-drk3)*ref3/cr(3)
      f340 = (f340-drk4)*ref4/cr(4)
      f380 = (f380-drk5)*ref5/cr(5)
      parn = (parn-drk6)*ref6/cr(6)




c save uncorrected  irradiances
      f300m = f300
      f305m = f305

c do stray light correction on channel 1 and 2 for UV-doses
      r1 = 1.0
      r2 = 1.0
      if(istrayd .ne. 1) then
        r300 = a1*f340 + b1*f380 + c1*parn
        f300 = f300 - r300
        r1 = 1.0/(1.0 - r300/rawcal1)

        r305 = a2*f340 + b2*f380 + c2*parn
        f305 = f305 - r305
        r2 = 1.0/(1.0 - r305/rawcal1)
      end if      


c dose rates (iusract = user selected dose type)
      drate = r1*dcoef(iusract,1)*f300 + r2*dcoef(iusract,2)*f305 
     $+ dcoef(iusract,3)*f320 + dcoef(iusract,4)*f340
     $+ dcoef(iusract,5)*f380 + dcoef(iusract,6)*parn



c return to uncorrected irradiances
      f300 = f300m
      f305 = f305m

c find max values
      if(drate .gt. dratem) dratem = drate

c subtract 0.5*timeintegr minutes because NILU-UV measurements are
c based on 'timeintegr' minutes integration time and the representative
c time for SZA calculation is therefor 0.5*timeintegr minutes
c before the time given in the raw data file. 'rhour' is only
c used to calculate solar zenith angle.
      rhour = ih*1.0 + min/60.0 + is/3600.0 - 0.5*timeintegr/60.0

      rmonth = im*1.0
      day = 1.0*id
      year = iym*1.0
      rtime = ih*1.0 + min/60.0 + is/3600.0

c calculate solar zenith angle      
c rlat: north is positive, rlong: east is positive        
      CALL ZENANG( YEAR, RMONTH, DAY, RHOUR, Z,
     $RLAT,-RLONG)
           

       if(z.lt.90.0) then
         if(iclt .eq.1) channel = f340
         if(iclt .eq.2) channel = f380

          
C Find clear-sky value of irradiance in variable 'channel'
C (i.e. what would be measured in 'channel' if the sky was 
C clear at sea level and with zero surface albedo)
	 CALL  CLRSKY(Z, F, fm) 


c factrans: correction factor for variable Earth-Sun distance
         aaa=0.035
         bbb=1.0
         pi = asin(1.0)*2
	 factrans=aaa*cos(jul*2*pi/365) + bbb



c compute cloud transmission
c factrans acounts for variable Earth-Sun distance
  
	 clt = cltscale*100.0*channel/(factrans*fm)

	 if(rtimeold.lt.0.0) rtimeold=0.0

	 if(rtimeold.gt.0.0) then 
           doseday=
     $     doseday + (drate+drateold)*(rtime-rtimeold)*0.5*3600.0
         end if

         drateold = drate
	
         rtimeold=rtime
        

c stray light correction channel1, here used only for ozone calc
         r300 = a1*f340 + b1*f380 + c1*parn
         f300 = f300 - r300

c stray light correction channel2, here used only for ozone calc
         r305 = a2*f340 + b2*f380 + c2*parn
         f305 = f305 - r305

        
         
         if(f305.gt.0.0 .and. f320.gt.0.0 .and. irato3 .eq.1 .and.
     $   iauto3.eq.1) then 
             ratio =(1.0 - r305/rawcal2)*f320/f305
             call deriveozone(RATIO,z,x,Na)
         end if


         if(f300.gt.0.0 .and. f320.gt.0.0 .and. irato3 .eq.2 .and.
     $   iauto3.eq.1) then 
             ratio =(1.0 - r300/rawcal1)*f320/f300
             call deriveozone(RATIO,z,x,Nb)
          end if

c automaticallys switch between ch3/ch1 and ch3/ch2 
         if(f300.gt.0.0 .and. f320.gt.0.0 .and.
     $   iauto3.eq.2 .and. z.lt.zswitcho3pair .and. 
     $   irato3.lt.3) then 
             ratio =(1.0 - r300/rawcal1)*f320/f300
             call deriveozone(RATIO,z,x,Nb)
         end if

         if(f305.gt.0.0 .and. f320.gt.0.0 .and. 
     $   iauto3.eq.2 .and. z.ge.zswitcho3pair .and.
     $   irato3.lt.3) then 
             ratio =(1.0 - r305/rawcal2)*f320/f305
             call deriveozone(RATIO,z,x,Na)
         end if


c ch4/ch1
         if(f300.gt.0.0 .and. f340.gt.0.0 .and. irato3 .eq.3) then 
             ratio =(1.0 - r300/rawcal1)*f340/f300
             call deriveozone(RATIO,z,x,Nc)
          end if

c ch5/ch1
         if(f300.gt.0.0 .and. f380.gt.0.0 .and. irato3 .eq.4) then 
             ratio =(1.0 - r300/rawcal1)*f380/f300
             call deriveozone(RATIO,z,x,Nd)
          end if

c ch2/ch1
         if(f300.gt.0.0 .and. f305.gt.0.0 .and. irato3 .eq.5) then 
             ratio =(1.0 - r305/rawcal2)*f305/f300
             call deriveozone(RATIO,z,x,Ne)
          end if


     
         if(x.le.0.0 .or. x.gt.700.0) x = -99.9
      
         x = x*o3scale
         
         if(idetail.eq.2)
     $   write(3,200) iym,im,id,jul,ih,min,is,z,drate,clt,x

       end if


c sum is later used to calculate averages
       if(rtime.gt.(hnoon-tdiff) .and. rtime.lt.(hnoon + tdiff)) then
          nn = nn + 1
          fmsum = fmsum + fm
          sum340 = sum340 + channel 
          dratesum = dratesum + drate

       end if

c sumo3 is later used to calculate ozone average
       if(rtime.gt.(hnoon-tdiff) .and. rtime.lt.(hnoon + tdiff)
     $ .and. clt .gt. clto3  .and. x.gt.-99.0 .and. z.lt.szao3) then
          no3 = no3 + 1
          sumo3 = sumo3 + x
          o3store(no3) = x
       end if

          



c       end if
                  

      go to 1
 900  continue
      close(3)


      o3n = -99.9
      draten= -99.9
      cltn = -99.9
      sigo3 = -99.9

      if(nn.gt.0) then
        if(no3.gt.0) o3n = sumo3/no3
        draten = dratesum/nn
        cltn = 100.0*cltscale*sum340/(factrans*fmsum)
       
        if(no3.gt.10) then
	   do 22 j = 1,no3
              sig = sig + (o3store(j) - sumo3/no3)**2.0
22         continue
           sigo3 = sqrt(sig/(no3-1))
        end if
      end if


      if(jino.gt.0) then
        write(2,300) iym,im,id,jul,o3n,no3,sigo3,draten,cltn,
     $  dratem,doseday      
      end if


 1000 continue


      close(1)
      close(2)


      if(numfile .eq.0) then

        call skipline(20)
        write(6,500) serial

        if(idateformat.eq.1) then
          write(6,501)  julsrt, julend,iym
        else
          write(6,503) char(id1+48)//char(id2+48),
     $    char(im1+48)//char(im2+48),char(iy1+48)//char(iy2+48)//
     $    char(iy3+48)//char(iy4+48),
     $    char(id3+48)//char(id4+48),
     $    char(im3+48)//char(im4+48),char(iy1+48)//char(iy2+48)//
     $    char(iy3+48)//char(iy4+48)
        end if

        write(6,502)

      end if    
         
      if(numfile.gt.0) then
         call skipline(20)
         write(6,*) 'Analysis completed'
      end if
      write(6,*)
      write(6,*) 'Hit ENTER-key to end program'
      read(5,*)
      write(6,*)
      write(6,*) 'Stop' 

      go to 3000
 2000 continue
      do i = 1,3
         write(6,*)
      end do
         write(6,*) '******* USER input error'
         write(6,*)
         write(6,*) 'Hit ENTER-key to end program'
         read(5,*)
 3000  continue

  3   format(a3)

  9   format(A9)
 10   format(a10)
 12   format(a12)
 13   format(a14)
 19   format(A9,' : ',i2)
 20   format
     $('Choose action spectrum',
     $' according to list above (1 -',i2,')')
 30   format(a30)
 70   format(' Cannot find calibration file:  ',a14)
 100  format(i2,i2,i2,1x,i2,i2,i2,6e10.3,f6.2)
 110  format(28x,'NILUUV ',a3)
 111  format('Uvproex Version 2.0')
 120  format('yyyy mm dd   jul   hh mm ss',
     $ '      SZA     dose rate       clt     Ozone')
 130  format('                              [degrees]',
     $ '  ',A10,'      [%]     [DU]')
 200  format(i4,1x,i2,1x,i2,3x,i3,3x,i2,1x,i2,1x,i2,3x,f7.2,
     $ 3x,e11.4,3x,f7.1,3x,f6.1)
 300  format(i4,1x,i2,1x,i2,2x,i3,1x,f6.1,1x,i4,1x,f6.1,2x,E11.4,
     $ 1x,f6.2,1x,E11.4,1x,E11.4)
c 310  format(i4,1x,i2,1x,i2,2x,i3,1x,f6.1,1x,
c     $ f6.1,2x,E11.4,2x,f7.2)
 400  format(28x,'Summary for NILU-UV ',a3)
 410  format(
     $'yyyy mm dd  jul   o3    no3   o3sig',
     $'  dose rate    clt   dose rate   daily dose')  
 420  format('Latitude : ',f7.2,' ',a7)
 421  format('Longitude: ',f7.2,' ',a7)
 422  format('Calibration file: ',a14)
 423  format('Ozone derived from channel pair: Uses ch3/ch1 ',
     $'for SZA < ',f4.1,' else uses ch3/ch2')
 424  format('Height above sea level: ',f7.1,'  m')
 425  format('Time interval averages are based on: ',
     $        f6.3,' UTC Hrs - ',f6.3,' UTC Hrs')
 426  format('CLT derived from channel ',i1) 
 427  format('Ozone derived from channel pair ',a7)
 428  format('Ozone scale factor used: ',f5.3)
 429  format('CLT scale factor used: ',f5.3)
 430  format('                 <------------ averages ------------->',
     $'   Maximum    Integrated')
 434  format('Ozone profile climatology: ',A13)
 438  format('Dose type: ',a9,' ',a30)
 442  format('Drift factors ch1-ch6: ',6f7.3)
 439  format('Drift factors used are found in the pr-files')
 440  format('                 [DU]         [DU]   ',a12,
     $'   [%]   ',a10,'  ',a10)
 500  format('Raw data for NILUUV ',a3,'  for the requested days')
 501  format('day no',i4,' - day no',i4,' year',i5)
 502  format('could not been found in data directory')
 503  format(a2,'.',a2,'.',a4,' - ',a2,'.',a2,'.',a4)
 800  format('Now processing data from: ',
     $ a2,'.',a2,'.',a4,' - ',a2,'.',a2,'.',a4)

      end

C****************** SUBROUTINE ZENANG ***************************

	subroutine zenang(year,rmonth,day,rhour,z,
     $                    rlat,rlong)

C     THIS SUBROUTINE CALCULATES THE SOLAR ZENITH ANGLE
C     AND THE CORRESPONDING MU-VALUE AT A GIVEN SITE. SEE OPERATIONS
C     HANDBOOK - OZONE OBSERVATIONS WITH A DOBSON
C     SPECTROPHOTOMETER BY W.D KOMHYR, WMO GLOBAL OZONE
C     RESEARCH AND MONITORING PROJECT, REPORT NO. 6, PAGE 104-105.

C     INPUT VARIABLES:
C        YEAR:       YEAR (UTC)
C        RMONTH:      MONTH (UTC)
C        DAY   :      DAY (UTC)
C        RHOUR :      FRACTIONAL HOUR (UTC)
C        RLAT  :      LATITUDE
C        RLONG :      LONGITUDE (POSITIVE FOR WEST)

C      OUTPUT VARIABLES:
C        Z     :      SOLAR ZENITH ANGLE


C      INTERNAL VARIABLES:

C        R1    :      EARTH-SUN DISTANCE IN METERS.
C        R2    :      RADIUS OF THE EARTH IN METERS




      
      UT = rhour
      rad = asin(1.0)*2/180.0
      r1 = 1.5E11
      r2 = 6371229.0
      A = (rmonth - 14 )/12
      B = INT(A)
      C = 1461*(year  + 4800 + B)/4
      U = INT(C)
      E = 367*(rmonth - 2 - 12*B)/12
      V = INT(E)
      G = (year  + 4900 + B)/100
      H = 3.0*INT(G)/4
      W = -INT(h)
      D = day - 2447095.5 + U + V + W + UT/24

      T = D/36525
      rLm = 279.697 + 36000.769*T
      m = INT(rLm/360.0)
      rL = rLm - m*360.0
      E = -(93.0 + 14.23*T - 0.0144*T**2.0)*sin(rL*rad) -
     $    (432.5 - 3.71*T - 0.2063*T**2.0)*cos(rL*rad)
     $   + (596.9 - 0.81*T - 0.0096*T**2.0)*sin(2.0*rL*rad)
     $   - (1.4 + 0.28*T)*cos(2.0*rL*rad)
     $   + (3.8 + 0.60*T)*sin(3.0*rL*rad) +
     $   (19.5 - 0.21*T - 0.0103*T**2.0)*cos(3.0*rL*rad) -
     $   (12.8 - 0.03*T)*sin(4.0*rL*rad)

      taneps = 0.43382 - 0.00027*T
      alfa = rL - E/240
      tandel = taneps*sin(alfa*rad)
      GHA = UT*15.0 + E/240 + 180
      tetha = (GHA -rlong)*rad
      delta = atan(tandel)/rad

      coszm = cos(tetha)*cos(delta*rad)*cos(rlat*rad) +
     $         sin(delta*rad)*sin(rlat*rad)

      cosz = (r1*coszm - r2)/
     $       sqrt(r1**2.0 + r2**2.0 - 2.0*r1*r2*coszm)
      z = acos(cosz)/rad




      return
      end

C****************** SUBROUTINE CLRSKY *************************


	SUBROUTINE CLRSKY(Z, F, fm)
     $                  

	REAL F(0:95)


C FIND FM,I.E. MODEL CLEAR-SKY IRRADIANCE (340 OR 380NM) AT SZA=Z
C BY LINEAR INTERPOLATION

	b = f(int(z)+1) - f(int(z))
	a = f(int(z)) - int(z)*b
	fm = a+b*z



	RETURN
	END

C********************* END OF CLRSKY *****************************

C******************** SUBROUTINE DERIVEOZONE  ******************** 

      SUBROUTINE DERIVEOZONE( RATIO, Z, X, N )
C
      PARAMETER( NOZONE=36, MINZ=0,MAXZ=95,MINOZ=0,IOZSTP=20)
      REAL RATIO
      REAL N(NOZONE,MINZ:MAXZ),N1(MINZ:MAXZ),N2(MINZ:MAXZ),OZ(NOZONE)

C   THIS SUBROUTINE COMPUTES TOTAL OZONE BY COMPARING MEASURED
C   IRRADIANCE RATIO  WITH MODEL CALCULATED IRRADIANCE RATIOS, N.
C   INPUT VARIABLES  :

C       RATIO        :     MEASURED IRRADIANCE RATIO 
C       Z            :     SOLAR ZENITH ANGLE
C       N(I,J)       :     MODEL CALCULATED IRRADIANCE RATIO AT SOLAR
C                          ZENITH ANGLE J, OZONE AMOUNT I.

C   OUTPUT VARIABLES :     

C       X            :     TOTAL OZONE IN D.U.


C   INTERNAL VARIABLES :

C       N1           :     THE N-VALUES AT Z=MINZ,...,MAXZ DEGREES, X1 D.U.
C       N2           :     THE N-VALUES AT Z=MINZ,...,MAXZ DEGREES, X2 D.U.


	DO 10 I=1, NOZONE
	    OZ(I)=(MINOZ + (I-1)*IOZSTP)*1.0
10      CONTINUE
		



      K = INT(Z)
      IF( RATIO.LE.N(2,K) ) I = 1

	DO  15 J = 2,NOZONE-2
      IF( (RATIO.GT.N(J,K)) .AND. (RATIO.LE.N(J+1,K)) ) I = J
15      continue

      IF( RATIO.GT. N(NOZONE-1,K) ) I = NOZONE-1

      X2 = OZ(I)
      X1 = OZ(I+1)
    
      DO 20 J = MINZ,MAXZ
	N1(J) = N(I+1,J)
	N2(J) = N(I,J)
20    CONTINUE

       I = INT(Z)
       B = (X1-X2) / ( N1(I) - N2(I) )
       A = X1 - B*N1(I)
       XI = A + B*RATIO
       J = I + 1
       B = (X1 - X2) / ( N1(J) - N2(J) )
       A = X1 - B*N1(J)
       XJ = A + B*RATIO
       X = XI + (XJ - XI) * ( Z - FLOAT(I) )
       RETURN
       END

C ************** END OF DERIVEOZONE ****************************

C ************** SUBROUINE DATE2JUL ****************************

        subroutine date2jul(iy,jul,mon,id)

c************************************************
c       input  :  	      			*
c                        iy  : year (yyyy)	*
c                        mon : month		*
c                        id  : day		*
c-----------------------------------------------*
c						*
c       output :         			*
c                        jul : day number	*
c						*
c************************************************
        integer mj(12), jj(12)

        data mj/1,32,60,91,121,152,182,213,244,274,305,335/



        n = 0

        if( float(iy/4) .eq. (iy/4.0) ) n = 1



        
        jj(1) = 1
        jj(2) = 32

        do i = 3,12
           jj(i) = mj(i) + n
        end do

        jul = jj(mon) + id -1

        return
        end

C ********* END OF DATE2JUL ****************************


C ********* SUBROUTINE JUL2DATE *************************

        subroutine jul2date(iy,jul,mon,id)
							
c-------------------------------------------------------	
c       input  :        				
c                        iy  : year (yyyy)		
c                        jul : day number		
c-------------------------------------------------------
c       output :         mon : month			
c                        id  : day			
c-------------------------------------------------------
        integer md(12), mj(12), mm(12), jj(12)

        data md/31,28,31,30,31,30,31,31,30,31,30,31/
        data mj/1,32,60,91,121,152,182,213,244,274,305,335/

        n = 0

        if( float(iy/4) .eq. (iy/4.0) ) n = 1


        do i = 1, 12
           mm(i) = md(i)
        end do

        mm(2) = md(2) + n

        jj(1) = 1
        jj(2) = 32

        do i = 3, 12
           jj(i) = mj(i) + n
        end do

        do i = 1, 12
           if( jul.ge.jj(i) .and. jul.le.(jj(i)+mm(i)-1) ) mon=i
        end do

        id = jul - jj(mon) + 1

        return
        end
C****************** END OF JUL2DATE *********************************

C****************** SUBROUTINE BLANK *********************************
        SUBROUTINE skipline(N)

C       WRITES N BLANK LINES.

        INTEGER N
        DO 10 I = 1, N
           WRITE(6,*)
10      CONTINUE
        RETURN
        END

C****************** END OF BLANK *************************************

C****************** SUBROUTINE DAYMONTHCHECK *************************

      subroutine daymonthcheck(iday,month,iy)

c     check if iday, month is valid, if not make them valid

      integer md(12)

      data md/31,28,31,30,31,30,31,31,30,31,30,31/

      
        n = 0

        if( float(iy/4) .eq. (iy/4.0) ) n = 1

        if(n.eq.1) md(2) = 29

c check if iday and month are valid. If not make them valid
        if(month.gt.12) month = 12
        if(month.lt.1) month = 1
        if(iday.gt.md(month)) iday=md(month)
        if(iday.lt.1) iday = 1

        return
        end

C****************** END OF DAYMONTHCHECK *****************************

C****************** SUBROUTINE JULCHECK *****************************

      subroutine julcheck(iy,jul)

        n = 0

        if( float(iy/4) .eq. (iy/4.0) ) n = 1

c check if jul is valid. If not make jul valid
        if(n.eq.0 .and. jul.gt.365) jul=365
        if(n.eq.1 .and. jul.gt.366) jul=366
        if(jul.lt.1) jul = 1


        return
        end

C********************* END OF JULCHECK *******************************
      subroutine drift(cr,iy,jul,serial,ilin,iid,iim,iiy)

c ---------------------------------------------------------
c 
c Variables
c
c   input:
c
c       iy      :  year (yyyy). integer
c       j       :  day number of year iy. integer
c       serial  :  serial number if instrument (character*3)
c       ilin    :  if ilin=1 linear drift between calibrations
c                  is assumed, else assumes no linear drift.
c                  integer.
c       iid, iim, iiy: initial calibration ddate and year.
c
c   output:
c
c       cr(i)   :  drift for channel i, relative to calibration.
c                  cr(i)>1.0 means increased sensitivity.
c                  NOTE: cr(i) is always relative to calibration
c                  number 1.
c                  To account for drift in channel i then multiply
c                  (rawdata - dark) with 1/cr(i) in main program
c 
c        
c
c   internal:
c
c       time    :  time for which the drift will be calculated
c                  (fractional year, real)
c       dr(k,i) :  drift for channel i at calibration number k
c                  stored in drift-file.
c       t(k)    :  calibration time for calibration number k
c                  fractional year. Found in drift-file
c
c
c ----------------------------------------------------------

      real t(1000), dr(1000,6), cr(6)
      character serial*3

      do i=1,6
         cr(i) = 1.0
         do k = 1,1000
            dr(k,i) = 0.0
            t(k) = 0.0
         end do
      end do

      time=iy*1.0+jul/365.0
      open(1,file='drift.'//serial, status='old',err=996)
      read(1,*,err=999)
      k=1
 1    read(1,*,end=99,err=999) iyy,j,
     $dr(k,1),dr(k,2),dr(k,3),dr(k,4),dr(k,5),dr(k,6)
      t(k) = iyy*1.0+j/365.0
      cr(1) = dr(k,1)
      k = k+1
      go to 1
 99   continue
      close(1)

      ierr=0
      do i = 1,6
         if(abs(dr(1,i) -1.0) .gt.0.001) irr=1
      end do




      do jj = 1,6
      do n=1,k-2

c check if calibration date-times are in chronological order
         if(t(n+1) .le. t(n)) ierr=1

         if(time.ge.t(n) .and. time .lt. t(n+1) ) then
            b = ( dr(n+1,jj) - dr(n,jj) ) / ( t(n+1) - t(n) )
            if(ilin.ne.1) b= 0.0
            a = dr(n,jj) - b*t(n)
         end if
         cr(jj) = a + b*time

c check if drift factors (cr) are acceptable   
c         if(abs(cr(jj)-1.0).lt.1.0e-8) ierr = 1

      end do
      end do

      if(time.lt.t(1) ) then
        do jj = 1,6
           cr(jj) = dr(1,jj)
        end do
      end if

      if(time.ge.t(k-1)) then
         do jj = 1,6
            cr(jj) = dr(k-1,jj)
         end do
      end if

     

       go to 997
 996  continue
      do i = 1,6
         cr(i) = 1.0
      end do
      call date2jul(iiy,ijul,iim,iid)
      open(1,file='drift.'//serial,status='new')
      write(1,100)
      write(1,110) iiy,ijul
 100  format('year  day     ch1     ch2     ch3'
     $,'     ch4     ch5     ch6')
 110  format(i4,2x,i3,
     $ '     1.000   1.000   1.000   1.000   1.000   1.000')
 997  continue
     

      go to 998
 999  continue
      do i = 1,6
         cr(i) = 1.0
      end do
      close(1)
 998  continue



      if(ierr.eq.1) then

         do i = 1,4
            write(6,*)
         end do
         write(6,200) serial
 200     format('######### Error in drift-file: drift.',a3)
         do i = 1,3
            write(6,*)
         end do
         write(6,*) 'Hit Enter-key to abort program'
         read(5,*)
         stop
      end if
      
      return
      end



 
