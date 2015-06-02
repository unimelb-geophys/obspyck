C+
	SUBROUTINE FOCINP
C
C	Input routine for FOCMEC
C
C	Arthur Snoke  Virginia Tech  May 1984
C	Last perturbed 12 October 1990
C	20 August 1991:  sun version.  call assign replaced by open
C	31 August:  expanded to include SV polarities and SV/SH ratios
C	15 May 1992:  Changed name of listing file from lp.lst to 
C		focmec.lst
C	19 June 1993: Separate P and S errors should now work
C	20 Jun 1993:  Belts and braces:  If someone uses a ratio with
C	  no input S take-off angle, now sets it equal to P angle
C	2 August 1993:  Changed rules for ratios near nodal surfaces
C	6 January 1997:  format change
C	7 April 2000: sense='R' no longer allowed for ratios.  First TOANG
C	  is not necessarily for P (if an SH or SV polarity is to be used)
C	1 October 2001: When porting to a PC, found some compilers could
C		not handle reading in an integer for a floating-pint
C		variable (number of polarity errors), so "fixed" it.
C	26 March 2002: If emergent polarity data are included, now it
C		will ask if you want to include it.  Previously it 
C		ignored such data.
C	5 July 2002: Some compilers do not like the way I had used
C	  CVALUE -- CVALUE(1:NFILE) with NFILE defined on the right-hand side.
c	19 November 2008: If INFO was left out for a ratio, did not read line.
C		Now it does.
C	June 2009: formatting changes
C-
	INCLUDE 'FOCMEC.INC'
	CHARACTER*1 SENSE,SNUMPOL
	CHARACTER*80 COMMNT,FILENA,CVALUE,DUMMY
	LOGICAL TRUTH, relweight, pguess
	CHARACTER*40 INFO
	RD = 45.0/ATAN(1.0)		
	open(2,file='focmec.lst',status='unknown')
	FILENA = 
     1    CVALUE('Output file name (for plotting) [focmec.out]',
     2    'focmec.out',NFILE)
	open(3,file=filena(1:nfile),status='unknown')
	CALL TIMDAT(2,'FOCMEC')
	CALL TIMDAT(3,'FOCMEC')
	COMMNT =
     1    CVALUE('Comment - up to 80 characters',DUMMY,NCOM)
	WRITE(2,'(1X,A)') COMMNT(1:NCOM)
	WRITE(3,'(1X,A)') COMMNT(1:NCOM)
100	FILENA = CVALUE('Input filespec',DUMMY,NFILE)
	OPEN(UNIT=1,FILE=FILENA(1:NFILE),STATUS='OLD',ERR=100)
	READ(1,'(A)') COMMNT
	ncom = lenc(commnt)
	WRITE(*,'(1X,A)') COMMNT(1:NCOM)
	IF (.NOT.TRUTH('Correct file?...[Y]')) GO TO 100
	WRITE(2,3) FILENA(1:NFILE)
	WRITE(3,3) FILENA(1:NFILE)
	WRITE(2,'(1X,A)') COMMNT(1:NCOM)
	WRITE(3,'(1X,A)') COMMNT(1:NCOM)
	WRITE(2,4)
	NRAT = 0
	NPOL = 0
	NPPOL = 0
	NSVPOL = 0
	NSHPOL = 0
	J = 0
	NGUESS = 0
200	read(1,'(a)',end=300) commnt
	ncom = lenc(commnt)
	if (ncom .lt. 22) then
	  read(commnt(1:21),'(A4,2F8.2,A1)') STA,AZIN,TOANG1,SENSE
	else
	  READ(commnt,5) STA,AZIN,TOANG1,SENSE,
     .	    RATLOG,SNUMPOL,TOANG2
	  if (ncom .gt. 39) then
	    read(commnt(40:ncom),'(a)') info
	  else
	    info = ' '
	  endif
	end if
	  IF (SENSE.EQ.'V' .OR. SENSE.EQ.'S' .OR. SENSE.EQ.'H') THEN
	    J = J + 1
	    IF (J .GT. MAX2) GO TO 300
	    stoang = toang1
	    ptoang = toang2
	    NRAT = NRAT + 1
	    LOGRAT(NRAT) = RATLOG
	    SVSH(2,NRAT) = SNUMPOL
	    RSTATN(NRAT) = STA
	    NADD = 0
	    SVSH(1,NRAT) = SENSE
	    IF (SENSE .EQ. 'H') NADD = 1000
	    IF (SENSE .EQ. 'S') NADD = 2000
	    WRITE(2,6) RSTATN(NRAT),AZIN,TOANG1,SENSE,LOGRAT(NRAT),
     .	      SVSH(1,NRAT),SVSH(2,NRAT),TOANG2,INFO(1:lenc(info))
	    KEYRAT(NRAT) = J + NADD
	  ELSE
	    WRITE(2,6) STA,AZIN,TOANG1,SENSE
	    IF (SENSE .EQ. 'U') SENSE = 'C'
            if (sense .eq. 'R') sense = '>'
            if (sense .eq. 'L') sense = '<'
	    if (sense.eq.'+' .or. sense.eq.'-' .or. sense.eq.'l'
     1	        .or. sense.eq.'r') then
	      if (nguess .eq. 0) then
	        nguess = 1
		pguess = truth('Include emergent polarity picks?..[Y]')
	      end if
	      if (pguess) then
	        IF (SENSE .EQ. '+') SENSE = 'C'
		IF (SENSE .EQ. '-') SENSE = 'D'
		IF (SENSE .EQ. 'l') SENSE = '<'
		IF (SENSE .EQ. 'r') SENSE = '>'
	      end if
	    end if
	    IF(.NOT.(SENSE.EQ.'C' .OR. SENSE.EQ.'D'
     .        .OR. SENSE .EQ. 'F' .OR. SENSE .EQ. 'B'
     .	      .OR. SENSE .EQ. '<' .OR. SENSE .EQ. '>')) GO TO 200
	      J = J + 1
	      IF (J .GT. MAX2) GO TO 300
	      stoang = toang1
	      ptoang = toang1
	      IF (SENSE .EQ. '<' .OR. SENSE .EQ. '>') THEN
	        NSHPOL = NSHPOL + 1
		if (sense .eq.'R') sense = '>'
	        if (sense .eq. 'L') sense = '<'
	        NADD = 2000
	      ELSE IF (SENSE .EQ. 'F' .OR. SENSE .EQ. 'B') THEN
	        NSVPOL = NSVPOL + 1
	        NADD = 1000
	      ELSE
	        NPPOL = NPPOL + 1
	        NADD = 0
	      END IF
	      NPOL = NPOL + 1
	      KEYPOL(NPOL) = J + NADD
	      PSTATN(NPOL) = STA
	      IF (SENSE .EQ. 'C' .OR. SENSE .EQ. '<'
     .          .OR. SENSE .EQ. 'F') THEN
	             POLRTY(NPOL) = 1
	      ELSE
	             POLRTY(NPOL) = -1
	      END IF
	  ENDIF
	  TREND = AZIN/RD
	  PLUNGE = (90.0 - PTOANG)/RD 
	  COST = COS(TREND)
	  SINT = SIN(TREND)
	  COSP = COS(PLUNGE)
	  SINP = SIN(PLUNGE)
	  XYZ(1,J) = COST*COSP
	  XYZ(2,J) = SINT*COSP
	  XYZ(3,J) = SINP
	  SPLUNG = (90.0 - STOANG)/RD
	  SINP = SIN(SPLUNG)
	  COSP = COS(SPLUNG)
C  Next two vectors reversed in sign from normal convention because
C   of my convention for SV and SH (down and left, facing the station)
	  XYZ(4,J) = -COST*SINP
	  XYZ(5,J) = -SINT*SINP
	  XYZ(6,J) = +COSP
	  XYZ(7,J) = SINT
	  XYZ(8,J) = -COST
	  XYZ(9,J) = 0.0
	  GO TO 200
300	CLOSE(UNIT=1)
	WRITE(*,7) NPPOL,NSVPOL,NSHPOL,NRAT
	IF (NPOL .LE. 0) THEN
	  WRITE(2,8)
	  WRITE(3,8)
	  GO TO 400
	ELSE
	  write(*,*) ' Can have relative weighting for polarity errors'
	  write(*,*) '  In this case, weight = theor. rad. factor'
	  write(*,*) '  above a chosen threshold'
	  IF (TRUTH('Want relative weighting?..[N]')) THEN
	    THRESH = RVALUE('Lower threshold for weighting [0.1]',0.1)
	    relweight = .true.
	  ELSE
	    THRESH = 1.0
	    relweight = .false.
	  END IF
	END IF
	IF (NPPOL .GT. 0 .AND. (NSHPOL .GT. 0
     .      .OR. NSVPOL .GT. 0)) THEN
	  WRITE(*,*) 'Options:    Total polarity errors'
	  WRITE(*,*) '         or Separate P and S error limits'
	  IF (TRUTH('Total polarity error option?..[Y]')) THEN
	    if (relweight) then
	      ERR = VALUE('Total number of errors (floating point)')
	      WRITE(3,17) NPPOL,NSVPOL,NSHPOL,ERR,THRESH
	      WRITE(2,17) NPPOL,NSVPOL,NSHPOL,ERR,THRESH
	    else
	      NERR = VALUE('Total number of errors (integer)')
	      err = nerr
	      WRITE(3,18) NPPOL,NSVPOL,NSHPOL,NERR
	      WRITE(2,18) NPPOL,NSVPOL,NSHPOL,NERR 
	    end if
	    ERRP = ERR
            ERRSV = ERR
	    ERRSH = ERR
	    GO TO 400
	  END IF
	END IF
	IF (NPPOL .GT. 0) THEN
	  if (relweight) then
	    ERRP = RVALUE('Allowed P polarity errors..[0.0]',0.0)
	  else
	    NERRP = IVALUE('Allowed P polarity errors..[0]',0)
	    errp = nerrp
	  end if
	  IF (ERRP .GT. float(NPPOL)) ERRP = NPPOL
	else
	  errp = 0.0
	END IF
	IF (NSVPOL .GT. 0) THEN
	  if (relweight) then
	    ERRSV = RVALUE('Allowed SV polarity errors..[0.0]',0.0)
	  else
	    NERRSV = IVALUE('Allowed SV polarity errors..[0]',0)
	    errSV = nerrsv
	  end if
	  IF (ERRSV .GT. float(NSVPOL)) ERRSV = NSVPOL
	else
	  errsv = 0.0
	END IF
	IF (NSHPOL .GT. 0) THEN
	  if (relweight) then
	    ERRSH = RVALUE('Allowed SH polarity errors..[0.0]',0.0)
	  else
	    NERRSH = IVALUE('Allowed SH polarity errors..[0]',0)
	    errsh = nerrsh
	  end if
	  IF (ERRSH .GT. float(NSHPOL)) ERRSH = NSHPOL
	else
	  errsh = 0.0
	END IF
	err = errp + errsv + errsh
	If (nguess .eq. 1) then
		if (pguess) then
		  write(2,*) 'Including emergent polarity picks'
		  write(3,*) 'Including emergent polarity picks'
		else
		  write(2,*) 'Not including emergent polarity picks'
		  write(3,*) 'Not including emergent polarity picks'
		end if
	end if
	if (thresh .lt. 1.0) then
	  write(2,19) nppol,errp,nsvpol,errsv,nshpol,errsh,thresh
	  write(3,19) nppol,errp,nsvpol,errsv,nshpol,errsh,thresh
	else
	  nerrp = errp
	  nerrsv = errsv
	  nerrsh = errsh
	  write(2,9) NPPOL,NERRP,NSVPOL,NERRSV,NSHPOL,NERRSH
	  write(3,9) NPPOL,NERRP,NSVPOL,NERRSV,NSHPOL,NERRSH
	end if
400	IF (NRAT .LE. 0) THEN
	  WRITE(2,12)
	  WRITE(3,12)
	ELSE
	  VPVS = RVALUE('VP/VS ratio at source ...[1.732]',1.732)
	  ERRRAT = RVALUE('Maximum allowed log10 of ratio..[0.6]',0.6)
	  NERRR = IVALUE('Number of allowed amp. ratio errors..[0]',0)
	  write(*,*) 'Next two entries are for near nodal amplitudes'
	  write(*,*) 'CUTP is the lower bound for P radiation factor'
	  write(*,*) 'CUTS is the lower bound for S radiation factor'
	  write(*,*) 'Ratio is indeterminate if both calculated'
	  write(*,*) '    values less than the chosen CUT values'
	  CUTP = RVALUE('CUTP: lower-limit P cutoff...[0.05]',0.05)
	  CUTS = RVALUE('CUTS: lower-limit S cutoff...[0.15]',0.15)
	  IF (NERRR .GT. NRAT) NERRR = NRAT
	  WRITE(2,13) NRAT,NERRR,ERRRAT,VPVS
	  WRITE(2,'(a,2(f5.3,a))') 'For ratios,  ', CUTP,
     1     ' = P radiation cutoff  ',CUTS,' = S radiation cutoff'
	  WRITE(3,13) NRAT,NERRR,ERRRAT,VPVS
	  WRITE(3,'(a,2(f5.3,a))') 'For ratios,  ', CUTP,
     1     ' = P radiation cutoff  ',CUTS,' = S radiation cutoff'
	  VPVS3 = VPVS**3
	ENDIF
	MAXSOL = IVALUE('Exit after this many acceptable sols...[100]',
     .	  100)
	BTMIN = RVALUE('Minimum search value B trend..[0]',0.0)
	BTDEL = ABS(RVALUE('Increment for B trend..[5 degrees]',5.0))
	BTMAX = RVALUE('Maximum B trend..[355 degrees]',355.0)
	  BTMAX = AMAX1(BTMIN,AMIN1(BTMAX,359.0))
	BPMIN = RVALUE('Minimum search value B plunge..[0]',0.0)
	BPDEL = ABS(RVALUE('Increment for B plunge..[5 degrees]',5.0))
	BPMAX = RVALUE('Maximum B plunge..[90 degrees]',90.0)
	  BPMAX = AMAX1(BPMIN,AMIN1(BPMAX,90.0))
	WRITE(*,*) '"A" angle = zero in vertical plane of B trend'
	AAMIN = RVALUE('Minimum search value A angle..[0]',0.0)
	AADEL = ABS(RVALUE('Increment for A angle..[5 degrees]',5.0))
	AAMAX = RVALUE('Maximum A angle..[85 degrees]',85.0)
	  AAMAX = AMAX1(AAMIN,AMIN1(AAMAX,90.0-AADEL))
	WRITE(2,14) BTMIN,BTDEL,BTMAX,BPMIN,BPDEL,BPMAX,AAMIN,AADEL,
     1	  AAMAX
	WRITE(3,14) BTMIN,BTDEL,BTMAX,BPMIN,BPDEL,BPMAX,AAMIN,AADEL,
     1	  AAMAX
	if (NRAT .gt. 0) then
	  write(3,*) 'R Ac/Al is number of acceptable ratios / allowed'
	  write(3,*) 'RMS Err is the RMS of acceptable obs-calc ratios'
	  write(3,*) 'RMS Err (all) is the RMS of all obs-calc ratios'
	  write(2,*) 'R Ac/Al is number of acceptable ratios / allowed'
	  write(2,*) 'RMS Err is the RMS of acceptable obs-calc ratios'
	  write(2,*) 'RMS Err (all) is the RMS of all obs-calc ratios'
	  write(*,*) 'R Ac/Al is number of acceptable ratios / allowed'
	  write(*,*) 'RMS Err is the RMS of acceptable obs-calc ratios'
	  write(*,*) 'RMS Err (all) is the RMS of all obs-calc ratios'
	  WRITE(3,15)
	  write(*,'('' '')')
	  WRITE(*,15)
	else
	  WRITE(3,20)
	  write(*,'('' '')')
	  WRITE(*,20)
	endif
	write(2,16)
	RETURN
C
3	FORMAT(1X,'Input from a file ',A)
4	FORMAT(/' Statn',T9,'Azimuth',T18,'TOAng',T26,
     1    'Key',T31,'Log10 (S/P)',T44,
     2    'NumPol',T52,'DenTOAng',T62,'Comment')
5	FORMAT(A4,2F8.2,A1,F8.4,1X,A1,1X,F6.2)
6	FORMAT(2X,A4,T10,F5.1,T19,F5.1,T27,A1,T32,F8.4,T41,'S',A1,T47,
     .    A1,T53,F6.2,T60,A)
7	FORMAT(' Input:',I4,' P ',I4,' SV and ',I4,' SH polarities and,',
     .    I4,' ratios')
8	FORMAT(' There are no polarity data')
9	FORMAT(' Polarities/Errors:  P ',I3.3,'/',I2.2,'  SV ',
     .    I3.3,'/',I2.2,'  SH ',I3.3,'/',I2.2)
12	  FORMAT(' There are no amplitude ratio data')
13	  FORMAT(' Input',I3,' ratios',I3,
     1      ' allowed errors, maximum error of',
     2      F5.2,'    VP/VS =',F6.3)
14	FORMAT(' The minimum, increment and maximum B axis trend are ',
     1    3F8.2/' The limits for the B axis',
     2    ' plunge are ',3F8.2/' The limits',
     3    ' for the angle of the A axis are ',3F8.2)
15	FORMAT(T5,'Dip',T11,'Strike',T20,'Rake',T28,'Pol:',
     .    T33,'P',T39,'SV',T45,'SH',
     .    T50,'R Ac/Al',T59,'RMS RErr',T70,'RErr (All)')
20	FORMAT(T5,'Dip',T11,'Strike',T20,'Rake',T28,'Pol:',
     .    T33,'P',T39,'SV',T45,'SH')
16       FORMAT(' ',76('+')/)
17	FORMAT(I4,' P Pol.',I3,' SV Pol.',I3,' SH Pol.',F5.1,' allowed',
     .    ' (weighted) errors'/  'Threshold =',F5.2)
18	FORMAT(I4,' P Pol.',I3,' SV Pol.',I3,' SH Pol.',I4,' allowed',
     .    '  errors')
19	FORMAT(' Polarities/Errors:  P ',I3.3,'/',F4.1,'  SV ',
     .    I3.3,'/',F4.1,'  SH ',I3.3,'/',F4.1,'  Threshh. =',F5.2)
	END
