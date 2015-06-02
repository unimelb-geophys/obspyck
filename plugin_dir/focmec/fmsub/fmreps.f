C+
	SUBROUTINE FMREPS(ANBTP,ANGS,PTTP,ANGS2,AN,PT,DSR,mt,MOMTEN,
     .	  LUNIT1,LUNIT2)
C
C	Input: A and N (trned and plunge), or P and T or dip, strike
C	  and rake (A&R convention) or moment tensor (A&R)
C	Output: the other representations plus the auxiliary plane.
C	PTTP:  4 parameters, trend and plunge for P and T
C	  P trend and plunge, T trend and plunge
C	ANBTP  6 parameters, t and p for A, N and B respectively.
C	ANGS   3 parameters,  dip, strike and rake for first plane
C	ANGS2  3 parameters, dip strike and rake for auxiliary plane
C	AN, PT, DSR are LOGICAL variables which are true if
C	  A and N, P and T or dip-strike-rake are input
C	MOMTEN  6 parameters:  the moment tensor (unit scalar
C	  magnitude) D & W convention
C	Angles come in and go out in degrees.
C	If LUNIT1 and/or LUNIT2 are positive, the representations
C	  are written on those logical unit numbers.
C	22 July 1985:  Added moment tensor output
C	30 October 1992:  Fixed up problem if lunit2=5.
C	15 March 2002:  Added moment-tensor input, changed MFF to MPP, etc.
C-
	LOGICAL AN,PT,DSR,MT
	REAL*4 MOMTEN(6)
	INTEGER LUNIT1, LUNIT2
	DIMENSION PTTP(4),ANGS(3),ANGS2(3),ANBTP(6)
	RDEG = 45.0/ATAN(1.0)
	PI = 4.0*ATAN(1.0)
	IF (MT) then
	  call mt_in(PTTP,PI)
	  CALL PTTPIN(PTTP,ANGS,ANGS2,ANBTP,MOMTEN,PI)
	else IF (PT) THEN
	  DO 100 J=1,4
100	  PTTP(J) = PTTP(J)/RDEG
	  CALL PTTPIN(PTTP,ANGS,ANGS2,ANBTP,MOMTEN,PI)
	ELSE IF (AN) THEN
	  DO 200 J=1,4
200	  ANBTP(J) = ANBTP(J)/RDEG
	  CALL ANTPIN(ANBTP,ANGS,ANGS2,PTTP,MOMTEN,PI)
	ELSE if (DSR) then
	  DO 300 J=1,3
300	  ANGS(J) = ANGS(J)/RDEG
	  CALL DSRIN(ANGS,ANBTP,ANGS2,PTTP,MOMTEN,PI) 
	END IF
	DO 400 I=1,3
	  ANGS(I) = ANGS(I)*RDEG
	  ANGS2(I) = ANGS2(I)*RDEG
	  PTTP(I) = PTTP(I)*RDEG
  	  ANBTP(I) = ANBTP(I)*RDEG
400	CONTINUE
	ANBTP(4) = ANBTP(4)*RDEG
	ANBTP(5) = ANBTP(5)*RDEG
	ANBTP(6) = ANBTP(6)*RDEG
	PTTP(4) = PTTP(4)*RDEG
	IF (LUNIT1 .GT. 0) THEN
	  WRITE (LUNIT1,1) (ANGS(I),I=1,3)
	  WRITE(LUNIT1,2)(ANGS2(I),I=1,3),'   Auxiliary Plane'
	  WRITE (LUNIT1,3) (ANBTP(J),J=1,4)
	  WRITE(LUNIT1,4) (ANBTP(J),J=5,6)
	  WRITE(LUNIT1,5) PTTP
	  WRITE(LUNIT1,6) MOMTEN
	END IF
	IF (LUNIT2 .GT. 0) THEN
	  if (lunit2 .eq. 5) then
	    WRITE (*,1) (ANGS(I),I=1,3)
	    WRITE(*,2)(ANGS2(I),I=1,3),'   Auxiliary Plane'
	    WRITE (*,3) (ANBTP(J),J=1,4)
	    WRITE(*,4) (ANBTP(J),J=5,6)
	    WRITE(*,5) PTTP
	    WRITE(*,6) MOMTEN
	  else
	    WRITE (LUNIT2,1) (ANGS(I),I=1,3)
	    WRITE(LUNIT2,2)(ANGS2(I),I=1,3),'   Auxiliary Plane'  
	    WRITE (LUNIT2,3) (ANBTP(J),J=1,4)
	    WRITE(LUNIT2,4) (ANBTP(J),J=5,6)
	    WRITE(LUNIT2,5) PTTP
	    WRITE(LUNIT2,6) MOMTEN
	  end if
	END IF
	RETURN
C
1	FORMAT(5X,'Dip,Strike,Rake ',3F9.2)
2	FORMAT(5X,'Dip,Strike,Rake ',3F9.2,A)
3	FORMAT(5X,'Lower Hem. Trend, Plunge of A,N ',4F9.2)
4	FORMAT(5X,'Lower Hem. Trend & Plunge of B ',2F9.2)
5	FORMAT(5X,'Lower Hem. Trend, Plunge of P,T ',4F9.2)
6	FORMAT(5X,'MRR =',F5.2,'  MTT =',F5.2,'  MPP =',F5.2,
     .    '  MRT =',F5.2,'  MRP =',F5.2,'  MTP =',F5.2)
	END
