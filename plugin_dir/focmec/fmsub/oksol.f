C+
	LOGICAL FUNCTION OKSOL(A,N,ANGLE,JSLIP)
C
C	Called by SRCHFM, which is called by FOCMEC
C	Checks for valid focal mechanisms based on ratios and polarities
C		designated by A and N
C	ANGLE is passed in and written out for a valid solution:
C		It is an internally caluclated quantity - the
C		angle an axis makes with the plane defined by
C		the B axis and vertical.  If JSLIP -1, it is the A axis,
C		If JSLIP = 2, it is the N axis.
C
C	Arthur Snoke  Virginia Tech  July 1984
C	24 July 1985:  Added SH polarity check
C	7 July 1990:  Commented out (C90) call to perturb solution
C	  if there are ratio dats based on Kisslinger's least-squares
C	  approach.  Decided it is too misleading, as polarities are
C	  not taken into account.  (Room for improvement!)
C	Latest perturbation (VAX):  12 October 1990
C	31 August 1991:  sun   Allows for SV polarities and SV/SH ratios
C	15 May 1992:  Corrected an error in printing if near a double
C		nodal plane in ratio printout
C	5 August 1993:  Changed rules for ratios when numerator and/or
C	  denominator near nodal surfaces.  Also, if both numerator and
C	  denominator near a nodal surface for all solutions, writes out
C	  rmsall as 99.99 instead of 0.0.  FLAG has been added to output
C	  for ratio data: NUM and DEN for numerator or denominator near
C	  nodal surface respectively; N&D for both.  Summary printout for
C	  total ratio error now same as in short output file -- RMS errors.
C	  Now radiation factors (normalized to unity) are printed out even
C	  if weighted option is not chosen.
C	22 March 2002: Small changes to include fact that fmreps can allow
C	  moment tensor input -- not used in this program.
C	18 November 2008: If all ratios are N&D, unacceptable solution.
C	  For each N&D, the number of ratios is decreased by one.
C	  Solution is unacceptable if fewer remaining ratios than bad ratios
C	June 2009: formatting changes for ratios.  Fixed ANGLE reference
C-
	INCLUDE 'FOCMEC.INC'
	CHARACTER*1 POL,MARK(3),AorN(2)
	character*3 flag(MAX)
	LOGICAL OKRAT,OKPOL,PT,AN,ANGS,mt
	REAL*4 DSR(3), DSRD(3),PTTP(4),ANBTP(6),DSR2(3),DIFF(MAX)
	REAL*4 SPOL(MAX),A(3),N(3),MOMTEN(6)
	DATA PT,AN,mt,ANGS/3*.FALSE.,.TRUE./
	DATA MARK/'*',' ','#'/, AorN/'A','N'/
	save
	OKSOL = .FALSE.
	IF (NPOL .GT. 0) THEN
	  IF (.NOT.OKPOL(A,N,NBADP,NBADSV,NBADSH)) RETURN
C
	ELSE
	  NBADP = 0
	  NBADSV = 0
	  NBADSH = 0
	  BADP = 0.0
	  BADSV = 0.0
	  BADSH = 0.0
	END IF
	do j=1,3
	  if (abs(A(j)) .lt. 0.0001) A(j) = 0.0
	  if (abs(abs(a(j))-1.0) .lt. 0.0001) a(j) = a(j)/abs(a(j))
	  if (abs(N(j)) .lt. 0.0001) N(j) = 0.0
	  if (abs(abs(N(j))-1.0) .lt. 0.0001) N(j) = N(j)/abs(N(j))
	end do
	pi = 4.0*atan(1.0)
	CALL AN2DSR(A,N,DSR,PI)
	IF (NRAT .GT. 0) THEN
	  IF (.NOT.OKRAT(DSR,NBADR,ESUM,ESUMAL,DIFF,SPOL,FLAG)) 
     1        RETURN
C
C	DON'T WANT TO COUNT SOLUTIONS NEAR DOUBLE NODAL SURFACE
C
	  NRAT_ALLOWED = NRAT
	  DO K=1,NRAT
	    IF (FLAG(K) .eq. 'N&D') NRAT_ALLOWED = NRAT_allowed - 1
	  END DO
C
	  if (nrat_allowed .gt. 0) then
	    RMSALL = SQRT(ESUMAL/NRAT_ALLOWED)
	  else
	    return
	  endif
	  if (nrat_allowed .le. nbadr) return
	ENDIF
	OKSOL = .TRUE.
	DO K=1,3
	  DSRD(K) = DSR(K)*RD
	END DO
	CALL FMREPS(ANBTP,DSRD,PTTP,DSR2,AN,PT,ANGS,MT,MOMTEN,2,0)
	ANGDEG = ANGLE*57.29578
	WRITE(2,1) AorN(JSLIP),ANGDEG
	IF (NRAT .GT. 0) THEN
	  nok = nrat_allowed - NBADR
	  RMS = SQRT(ESUM/(NRAT_allowed-NBADR))
	  WRITE(*,3) DSRD,BADP,BADSV,BADSH,nok,nrat_allowed,RMS,RMSALL
	  WRITE(3,3) DSRD,BADP,BADSV,BADSH,nok,nrat_allowed,RMS,RMSALL
	ELSE
	  WRITE(*,3) DSRD,BADP,BADSV,BADSH
	  WRITE(3,3) DSRD,BADP,BADSV,BADSH
	END IF
	IF (NBADP .GT. 0) THEN
	  IF (NBADP .LE. 11) THEN
	    WRITE(2,5) (BADPP(KK),KK=1,NBADP)
	    WRITE(2,11) (WBADP(KK),KK=1,NBADP)
	  ELSE
	    WRITE(2,5) (BADPP(KK),KK=1,11)
	    WRITE(2,11) (WBADP(KK),KK=1,11)
	    if(nbadp .le. 22) then
	      WRITE(2,'(T23,11(A4,1X))') (BADPP(KK),KK=12,NBADP)
	      WRITE(2,'(T23,11(F4.2,1X))') (WBADP(KK),KK=12,NBADP)
	    else 
	      WRITE(2,'(T23,11(A4,1X))') (BADPP(KK),KK=12,22)
	      WRITE(2,'(T23,11(F4.2,1X))') (WBADP(KK),KK=12,22)
	      WRITE(2,'(20(T23,11(A4,1X)/))') (BADPP(KK),KK=23,nbadp)
	      WRITE(2,'(20(T23,11(F4.2,1X)/))') (WBADP(KK),KK=23,nbadp)
	    endif
	  END IF
	  WRITE(2,12) BADP
	END IF
	IF (NBADSV .GT. 0) THEN
	  IF (NBADSV .LE. 11) THEN
	    WRITE(2,10) (BADSVP(KK),KK=1,NBADSV)
	    WRITE(2,13) (WBADSV(KK),KK=1,NBADSV)
	  ELSE
	    WRITE(2,10) (BADSVP(KK),KK=1,11)
	    WRITE(2,13) (WBADSV(KK),KK=1,11)
	    if(nbadsv .le. 22) then
	      WRITE(2,'(T23,11(A4,1X))') (BADsvp(KK),KK=12,NBADsv)
	      WRITE(2,13) (WBADsv(KK),KK=12,NBADsv)
	    else 
	      WRITE(2,'(T23,11(A4,1X))') (BADsvp(KK),KK=12,22)
	      WRITE(2,13) (WBAdsv(KK),KK=12,22)
	      WRITE(2,'(20(T23,11(A4,1X)/))') (BADsvP(KK),KK=23,nbadsv)
	      WRITE(2,'(20(T23,11(F4.2,1X)/))') (WBADsv(KK),KK=23,nbadsv)
	    endif
	  END IF
	  WRITE(2,14) BADSV
	END IF
	IF (NBADSH .GT. 0) THEN
	  IF (NBADSH .LE. 11) THEN
	    WRITE(2,15) (BADSHP(KK),KK=1,NBADSH)
	    WRITE(2,16) (WBADSH(KK),KK=1,NBADSH)
	  ELSE
	    WRITE(2,15) (BADSHP(KK),KK=1,11)
	    WRITE(2,16) (WBADSH(KK),KK=1,11)
	    if(nbadsh .le. 22) then
	      WRITE(2,'(T23,11(A4,1X))') (BADshp(KK),KK=12,NBADsh)
	      WRITE(2,'(T23,11(F4.2,1X))') (WBADsh(KK),KK=12,NBADsh)
	    else 
	      WRITE(2,'(T23,11(A4,1X))') (BADshp(KK),KK=12,22)
	      WRITE(2,'(T23,11(F4.2,1X))') (WBAdsh(KK),KK=12,22)
	      WRITE(2,'(20(T23,11(A4,1X)/))') (BADshP(KK),KK=23,nbadsh)
	      WRITE(2,'(20(T23,11(F4.2,1X)/))') (WBADsh(KK),KK=23,nbadsh)
	    endif
	  END IF
	  WRITE(2,17) BADSH
	END IF
	IF (NRAT .LE. 0) THEN
	  WRITE(2,6)
	  RETURN
	END IF
	WRITE(2,7)
	DO K=1,NRAT
	  IF (SVSH(1,K) .EQ. 'V' .AND. SPOL(K) .GT. 0.0) POL = 'F'
	  IF (SVSH(1,K) .EQ. 'V' .AND. SPOL(K) .LT. 0.0) POL = 'B'
	  IF (SVSH(1,K) .EQ. 'S' .AND. SPOL(K) .GT. 0.0) POL = 'F'
	  IF (SVSH(1,K) .EQ. 'S' .AND. SPOL(K) .LT. 0.0) POL = 'B'
	  IF (SVSH(1,K) .EQ. 'H' .AND. SPOL(K) .GT. 0.0) POL = 'L'
	  IF (SVSH(1,K) .EQ. 'H' .AND. SPOL(K) .LT. 0.0) POL = 'R'
	  if (FLAG(k) .ne. 'N&D') then
	    WRITE(2,8) LOGRAT(K),CALRAT(K),DIFF(K),MARK(WTRAT(K)+1),
     .	      RSTATN(K),SVSH(1,K),SVSH(2,K),POL,FLAG(K)
	  else
	    WRITE(2,21) LOGRAT(K),MARK(3),
     .	      RSTATN(K),SVSH(1,K),SVSH(2,K),POL,FLAG(K)
	  endif
	END DO
	WRITE(2,9) nrat_allowed, RMSALL, nok, RMS
	WRITE(2,6)
	RETURN
C
1	FORMAT(a1,' angle with plane defined by vertical & B trend:',
     1         F5.1)
3	FORMAT(3F8.2,T30,3F6.1,4x,I2.2,'/',I2.2,F10.2,F10.2)
5	FORMAT(/' P Polarity error at',T23,11(A4,1X))
6	FORMAT(' ',77('+')/)
7	FORMAT(/T11,'Log10(Ratio)',T53,'Ratio',
     .    T63,'S Polarity'/T6,'Observed',
     1    T16,'Calculated',T30,
     2    'Difference',T42,'Station',T53,' Type',T63,'Obs.',
     3    T69,'Calc.',T75,'Flag')
8	FORMAT(4X,F8.4,T17,F8.4,T30,F8.4,T43,A1,A4,T55,'S',A1,T64,A1,
     .    T71,A1,T76,A3)
21	FORMAT(4X,F8.4,T43,A1,A4,T55,'S',A1,T64,A1,
     .    T71,A1,T76,A3)
9	FORMAT(/' The RMS log amplitude ratio error for all',i3,
     1    ' allowed solutions is', F7.3/
     2    ' The error for',i3,' acceptable solutions is',F6.3/)
10	FORMAT(/' SV Polarity error at',T24,11(A4,1X))
11	FORMAT(' P Polarity weights:',T23,11(F4.2,1X))
12	FORMAT('  Total P polarity weight is',F7.3)
13	FORMAT(' SV Polarity weights:',T24,11(F4.2,1X))
14	FORMAT('  Total SV polarity weight is',F7.3)
15	FORMAT(/' SH Polarity error at',T24,11(A4,1X))
16	FORMAT(' SH Polarity weights:',T24,11(F4.2,1X))
17	FORMAT('  Total SH polarity weight is',F7.3)
	END
