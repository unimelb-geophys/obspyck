C+
	SUBROUTINE DSRIN (ANGS,ANBTP,ANGS2,PTTP,MOMTEN,PI)
C
C	Calculates other representations of fault planes with
C		dip, strike and rake (A&R convinention) input.  All
C		angles are in radians.
C	22 July 1985:  Added moment tensor output (D&W convention)
C	               normalized to unit scalar moment
C	1 October 2001: When ported to the PC, on one compiler there
C		was a roundoff problem for limiting cases.  Included
C		a fix.
C-
	REAL N(3), MOMTEN(6)
	DIMENSION PTTP(4),ANGS(3),ANGS2(3),ANBTP(6),P(3),T(3),A(3),B(3)
	DATA SR2/0.707107/
	RAKE=ANGS(3)
	STR = ANGS(2)
	DIP = ANGS(1)
	A(1) = COS(RAKE)*COS(STR) + SIN(RAKE)*COS(DIP)*SIN(STR)
	A(2) = COS(RAKE)*SIN(STR) - SIN(RAKE)*COS(DIP)*COS(STR)
	A(3) = -SIN(RAKE)*SIN(DIP)
	N(1) = -SIN(STR)*SIN(DIP)
	N(2) = COS(STR)*SIN(DIP)
	N(3) = -COS(DIP)
	do j=1,3
          if (abs(A(j)) .le. 0.0001) A(j) = 0.0
          IF ((ABS(A(j))-1.0) .gt. 0.0) A(j)=A(j)/abs(A(j))
          if (abs(N(j)) .le. 0.0001) N(j) = 0.0
          IF ((ABS(N(j))-1.0) .gt. 0.0) N(j)=N(j)/abs(N(j))
        end do
	CALL V2TRPL(A,ANBTP(1),PI)
	CALL V2TRPL(N,ANBTP(3),PI)
	DO 100 J=1,3
	T(J) = SR2*(A(J) + N(J))
100	P(J) = SR2*(A(J) - N(J))
	B(1) = P(2)*T(3) - P(3)*T(2)
	B(2) = P(3)*T(1) - P(1)*T(3)
	B(3) = P(1)*T(2) - P(2)*T(1)
	CALL V2TRPL(P,PTTP(1),PI)
	CALL V2TRPL(T,PTTP(3),PI)
	CALL V2TRPL(B,ANBTP(5),PI)
	CALL AN2DSR(N,A,ANGS2,PI)
	CALL AN2MOM(A,N,MOMTEN)
	RETURN
	END
