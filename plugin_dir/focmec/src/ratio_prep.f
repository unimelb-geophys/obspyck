C+
	program ratio_prep
C
C	Reads in a file with a latitude, longitude, and depth for an event
C	  plus a list of statin names.  Calculates epicentral distances,
C	  station-based azimuth, P and S travel times and take-off angles 
C	  for each station, and the S-to-P surface velocity ratio.
C	This version uses the iasp91 or ak135 traveltime tables.  
C	  (Others in same format could be used.)  
C	  Because the most likely S arrival to be
C	  used for teleseismic stations is SH, S is used 
C	  even if SKS comes in earlier.
C	Input file format:
C		Line 1: latitude, longitude, depth (free format). Angles in
C			degrees and decimal degrees with negative W and S.
C			Depth is in km.
C		Lines 2 through N+1: Station names (A4) for N stations
C	Also required:
C		stations.loc one line per station
C			Station name (A4), latitude and longitude (free format)
C		IASP91.hed, IASP91.tbl (prepared using iasp91 software)
C	Output file format:
C		one comment line
C		one line which starts with the S/P velocity ratio
C		one line with column headers describing the station output
C		one line per station. Name, dist, az, Ptoang, Stoang, Pemrga,
C		Semrga,tp, ts.  Format is A4,F9.4,7F9.3 for each line.
C	The output file is used as an input file for focmec_prep.
C	A second output file, fileprep.lst, has a summary of the run.
C	1 April 2000 jas/vt
C	3 August 2001: Previous version needed a flat model file in addition
C	  to the .TBL and .HED files to get the take-off angles.  Learned
C	  recently that the slownesses are returned by subroutine depset so
C	  do not need the extra file.
C	July 2008:  Changed name from fileprep.
C-
	parameter (nsta=500)
	character*4 sta(nsta),char4
	dimension Ptoang(nsta),Stoang(nsta),dist(nsta),Pemrga(nsta)
	dimension Semrga(nsta),tp(nsta),ts(nsta),az(nsta)
	CHARACTER*80 FILENA,CVALUE,commnt
c
c	Open file with hypocenter and up to 500 station names
C
	rd = 45.0/atan(1.0)
	open(unit=2,file='ratio_prep.lst',status='unknown')
	call timdat(2,'ratio_prep')
	FILENA =
     1    CVALUE('Station locations filespec [./stations.loc]',
     2    '../stations.loc',NFILE)
	open(3,file=filena(1:nfile),status='old')
	write(2,*) 'Station locations from ',
     1		filena(1:nfile)
	FILENA = 
     1    CVALUE('Input filespec with hypocenter and stations',
     2    'dummy',NFILE)
	open(1,file=filena(1:nfile),status='old')
	read(1,*) elat, elong, focdep
	write(2,*) 'Hypocenter and Station names from ',
     1		filena(1:nfile)
	write(2,*) 'Latitude:',elat,' Longitude:',elong,' Depth:',focdep
	elatr = elat/rd
	elongr = elong/rd
	j = 1
	ierr = 0
	do while (ierr.eq.0 .and. j.le.nsta)
	  read(1,'(a)',iostat=ierr) char4
	  if (ierr .eq. 0) then
	    sta(j) = char4
	    call stations(3,sta(j),slat,slong)
	    slatr = slat/rd
	    slongr = slong/rd
	    call disaz(elatr,elongr,slatr,slongr,azr,baz,distr)
	    az(j) = rd*azr
	    dist(j) = rd*distr
	    N = j
	    j = j + 1
	  end if
	end do
	close(1)
	close(3)
	call getrays(N,dist,focdep,Ptoang,Stoang,Pemrga,Semrga,tp,ts,
     1		vrat)
	FILENA = 
     1    CVALUE('Filespec of fmecprep input file',
     2    'dummy',NFILE)
	open(1,file=filena(1:nfile),status='unknown')
	write(2,*) 'fmecprep input filespec: ',filena(1:nfile)
	commnt = CVALUE('Comment line','dummy',ncom)
	write(1,*) commnt(1:ncom)
	write(2,*) commnt(1:ncom)
	write(1,*) vrat,' is the S/P velocity ratio at the surface'
        write(2,*) vrat,' is the S/P velocity ratio at the surface'
	write(1,'("Stn",t8,"Dist",t17,"Az",T26,"Ptoa",T35,"Stoa",T44,
     1    "Pemrg",T53,"Semrg",T62,"TT(P)",T71,"TT(S)")')
	do j=1,N
	  write(1,'(A4,F9.4,7F9.3)') sta(j),dist(j),az(j),Ptoang(j),
     1	      Stoang(j),Pemrga(j),Semrga(j),tp(j),ts(j)
	end do
	stop
	end
	subroutine getrays(N,dist,focdep,Ptoang,Stoang,Pemrga,Semrga,
     1	      tp,ts,vrat)
	parameter (max=60)
	logical prnt(3)/3*.false./
	character*8 phcd(max),phlst(10)
	character*50 modnam,cvalue
	dimension dist(*),Ptoang(*),Stoang(*),Pemrga(*),Semrga(*)
	dimension tp(*),ts(*)
	dimension tt(max),dtdd(max),dtdh(max),dddp(max)
	dimension usrc(2)
	data in/1/,phlst(1)/'P'/,phlst(2)/'S'/,rzero/6371./
c
	RD = 45.0/ATAN(1.0)
C
	modnam =
     1    CVALUE('Model path and name for .tbl and .hed [IASP91]',
     2    '../../iasp91stuff/IASP91',Nmodnam)
	call tabin(in,modnam)
	write(2,*) 'Model Name: ',modnam(1:nmodnam)
	call brnset(2,phlst,prnt)
	fdep = 0.0
	call depset(fdep,usrc)
	vpsurf = 1.0/usrc(1)
	vssurf = 1.0/usrc(2)
	call depset(focdep,usrc)
C	
C	NOte that the slownesses returned are flat-earth corrected
C
	vpfoc = ((rzero-focdep)/rzero)/usrc(1)
	vsfoc = ((rzero-focdep)/rzero)/usrc(2)
	vrat = vssurf/vpsurf
	etasurfp = rzero/(vpsurf*rd)
	etasurfs = rzero/(vssurf*rd)
	etafocp = (rzero-focdep)/(vpfoc*rd)
	etafocs = (rzero-focdep)/(vsfoc*rd)
	do j=1,N
	  call trtm(dist(j),max,nph,tt,dtdd,dtdh,dddp,phcd)
	  Pemrga(j) = rd*asin(dtdd(1)/etasurfp)
	  Ptoang(j) = rd*asin(dtdd(1)/etafocp)
	  if (dtdh(1) .gt. 0.0) Ptoang(j) = 180. - Ptoang(j)
	  tp(j) = tt(1)
	  k = 1
	  do while(phcd(k)(1:1).ne.'S' .or. phcd(k)(1:2).eq.'SK')
	    k = k + 1
	  end do
	  Semrga(j) = rd*asin(dtdd(k)/etasurfs)
	  Stoang(j) = rd*asin(dtdd(k)/etafocs)
	  if (dtdh(k) .gt. 0.0) Stoang(j) = 180. - Stoang(j)
	  ts(j) = tt(k)
	end do
	close(1)
	return
	end
C+
      SUBROUTINE STATIONS(NUN,STA,SLAT,SLONG)
C
C  FINDS STATION WITH NAME STA FROM FILE NUN
C  IF STA NOT ON LIST, exits
C-
      LOGICAL MORE,ANOTHER
      CHARACTER*4 STA,TSTA
      CHARACTER*100 RECORD
	nsta = lenc(sta)
	tsta = sta
c	call uppercase(tsta)
      MORE = .TRUE.
      DO WHILE (MORE)
        REWIND NUN
        ANOTHER = .TRUE.
        DO WHILE (ANOTHER)
          READ(NUN,'(A)',IOSTAT=IERR) RECORD
          IF (IERR .EQ. 0) THEN
c	    call uppercase(record(1:nsta))
	    if (tsta .eq. record(1:nsta)) then
		I = nsta + 1
                READ(RECORD(I:lenc(record)),*) SLAT,SLONG
                ANOTHER = .FALSE.
                MORE = .FALSE.
            END IF
          ELSE if (IERR .eq. -1) then
            WRITE(*,'('' No station called '',A4,'' listed'')') STA
	    stop
	  else
	    write(*,*) 'Error in station list.  Aborting'
	    stop
          END IF
        END DO
      END DO
      RETURN
      END
C+
C     SUBROUTINE DISAZ(ELAT,ELONG,SLAT,SLONG,AZE,AZS,DIST)
C
C  CALCULATES DISTANCES AND AZIMUTHS FOR A SPHERICAL EARTH
C    USING GEOCENTRIC LATITUDES INSTEAD OF GEOGRAPHIC ONES
C    BETWEEN AN (E)VENT AND A (S)EISMOGRAPH STATION.
C  ALL ANGLES - INCLUDING DIST - ARE IN RADIANS.
C  ASSUMES LATITUDES RUN FROM -PI/2 TO +PI/2 FROM S TO N, AND
C	    LONGITUDES FROM -PI TO +PI FROM W TO E.
C  TO DERIVE THE EQUATIONS, DEFINE A SPHERICAL TRAINGLE WITH ONE VERTEX
C    AT THE NORTH POLE, ONE AT S AND THE OTHER AT E.
C-
	SUBROUTINE DISAZ(ELAT,ELONG,SLAT,SLONG,AZE,AZS,DIST)
	DOUBLE PRECISION B,PI,ELA,SLA,ESLON,PI2,TWOPI,ELON,SLON,
     .    COSDEL,COSELA,SINELA,COSSLA,SINSLA
	DATA B,PI/0.9932773D0,3.1415926D0/
	DATA PI2,TWOPI/1.5707963D0,6.2831853D0/
	if ((elat.eq.slat) .and. (elong.eq.slong)) then
	  aze = 0.0
	  azs = pi
	  dist = 0.0
	  return
	end if
	ELA = PI2 - DATAN2(B*DSIN(DBLE(ELAT)),DCOS(DBLE(ELAT)))
	SLA = PI2 - DATAN2(B*DSIN(DBLE(SLAT)),DCOS(DBLE(SLAT)))
	ELON = ELONG
	IF (ELON .LT. 0.0) ELON = ELON + TWOPI
	SLON = SLONG
	IF (SLON .LT. 0.0) SLON = SLON + TWOPI
	ESLON = ELON - SLON
	COSELA = DCOS(ELA)
	SINELA = DSIN(ELA)
	COSSLA = DCOS(SLA)
	SINSLA = DSIN(SLA)
	COSDEL = COSELA*COSSLA+SINELA*SINSLA*DCOS(ESLON)
	DIST = DACOS(COSDEL)
	SINDEL = SIN(DIST)
	FACTOR = -DSIN(ESLON)/SINDEL
	SINAZE = FACTOR*SINSLA
	COSAZE = (COSSLA-COSELA*COSDEL)/(SINELA*SINDEL)
	AZE = ATAN2(SINAZE,COSAZE)
	IF (AZE .LT. 0.0) AZE = AZE + TWOPI
	SINAZS = -FACTOR*SINELA
	COSAZS = (COSELA-COSSLA*COSDEL)/(SINSLA*SINDEL)
	AZS = ATAN2(SINAZS,COSAZS)
	IF (AZS .LT. 0.0) AZS = AZS + TWOPI
	RETURN
	END
