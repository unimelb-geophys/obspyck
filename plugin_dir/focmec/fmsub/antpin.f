C+
	SUBROUTINE ANTPIN (ANBTP,ANGS,ANGS2,PTTP,MOMTEN,PI)
C
C	Calculates other representations of fault planes with
C		trend and plunge of A and N as input.  All
C		angles are in radians.
C	22 July 1985:  Added moment tensor output.
C-
	REAL N(3), MOMTEN(6)
	DIMENSION PTTP(4),ANGS(3),ANGS2(3),ANBTP(6),P(3),T(3),A(3),B(3)
	DATA SR2/0.707107/
	CALL TRPL2V(ANBTP(1),A)
	CALL TRPL2V(ANBTP(3),N)
	DO 100 J=1,3
	  T(J) = SR2*(A(J) + N(J))
	  P(J) = SR2*(A(J) - N(J))
100	CONTINUE
	B(1) = P(2)*T(3) - P(3)*T(2)
	B(2) = P(3)*T(1) - P(1)*T(3)
	B(3) = P(1)*T(2) - P(2)*T(1)
	CALL V2TRPL(P,PTTP(1),PI)
	CALL V2TRPL(T,PTTP(3),PI)
	CALL V2TRPL(B,ANBTP(5),PI)
	CALL AN2DSR(A,N,ANGS,PI)
	CALL AN2DSR(N,A,ANGS2,PI)
	CALL AN2MOM(A,N,MOMTEN)
	RETURN
	END
