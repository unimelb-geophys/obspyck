C+
C	SUBROUTINE FLTSOL(A,N,BMATRX,PLUNGE,TREND,ANGLE,JA)
C
C	Called by SRCHFM, a subroutine of FOCMEC
C	Calculates the A and N vectors (Herrmann's X and Y)
C		for a given trend and plunge for the B axis
C		and an ANGLE relative to a plane defined by the vertical
C		direction and the trend of B.  That is, if ANGLE=0,
C		A and B have the same trend.
C	Rotations obey the right-hand rule.
C
C	Arthur Snoke  Virginia Tech  May 1984
C	June 2009: Added comments to explain more fully the operations
C-
	SUBROUTINE FLTSOL(A,N,BMATRX,PLUNGE,TREND,ANGLE,JA)
	INCLUDE 'FOCMEC.INC'
	REAL*4 A(3), N(3), BMATRX(3,3),ZROT(3,3),YROT(3,3),ANB(3,3)
	IF (JA .EQ. 1) THEN
		DO 100 J=1,3
		DO 100 K=1,3
		ZROT(J,K) = 0.0
100		YROT(J,K) = 0.0
C
C	Construct a rotation matrix to map X (North), Y (East),
C		Z (Down) into A, N, B.  Input is B and an angle ANGLE
C
C	First rotate about Z (= Down) through an angle TREND.
C		X now has the trend of B
	ZROT(1,1) = COS(TREND)
	ZROT(2,2) = ZROT(1,1)
	ZROT(1,2) = SIN(TREND)
	ZROT(2,1) = -ZROT(1,2)
	ZROT(3,3) = 1.0
C
C	Now rotate about Y through an angle 90-PLUNGE.
C		This rotates the Z axis into B.
C		Becasue the rotation is about a horizontal axis
C		perpendicular to the plane defined by the vertical
C		direction and the trend of B, the rotated X axis will
C		still be in that plane but its trend will differ from
C		that of B by 180 degrees.
C
	YROT(1,1) = SIN(PLUNGE)
	YROT(3,3) = YROT(1,1)
	YROT(3,1) = COS(PLUNGE)
	YROT(1,3) = -YROT(3,1)
	YROT(2,2) = 1.0
C
C	BMATRX is the product of YROT and ZROT
C
	CALL GMPRD(YROT,ZROT,BMATRX,3,3,3)
C
C	BMATRX does not change as ANGLE changes
C
	ENDIF
C
C	Rotate about Z (= B) through an angle ANGLE.
C		This rotates X into A and Y into N.
C	Inverse of matrix rotating by ANGLE about B
C
C
	ZROT(1,1) = COS(ANGLE)
	ZROT(2,2) = ZROT(1,1)
	ZROT(1,2) = SIN(ANGLE)
	ZROT(2,1) = -ZROT(1,2)
	ZROT(3,3) = 1.0
C
C	ANB is the product of ZROT and BMATRX
C
	CALL GMPRD(ZROT,BMATRX,ANB,3,3,3)
	DO 200 J=1,3
	A(J) = ANB(1,J)		!  A is 1st row of ANB
200	N(J) = ANB(2,J)		!  N is 2nd row of ANB
	RETURN
	END
