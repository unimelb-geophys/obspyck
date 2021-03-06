C+
	SUBROUTINE SHNSRF(PI,MT,R,ALINE,DASHL,SIZE,UPPER)
C
C	Calculates and plots the projections of the SH nodal surfaces.
C	Based on Bruce Julian's code.
C-
	character*4 aline
	LOGICAL XRTMAX,YTPMAX
	REAL X(181), Y(181)
	INTEGER SEGST(10), NTOT(10)
C  moment tensor
	REAL MT(6)
	LOGICAL SHNODE, SHNNXT
	LOGICAL RCLIP
	LOGICAL PENUP
C  loop index
	INTEGER I
C  focal-sphere coordinates along contour
	REAL CI1, CI2, CZ1, CZ2
C k = data, j = node (but j not used)
	INTEGER K
	DATA XRTMAX,YTPMAX/2*.TRUE./
	DO 100 I=1,10
	  NTOT(I) = 0
	  SEGST(I) = 0
100	CONTINUE
	IF ((MT(4) .EQ. 0.0) .AND. (MT(5) .EQ. 0)) THEN
	  NCURVE = 2
	  AZ = 0.5*ATAN2(-MT(6),0.5*(MT(2)-MT(3)))
	  SAZ = SIN(AZ)
	  CAZ = COS(AZ)
	  SEGST(1) = 1
	  SEGST(2) = 3
	  NCURVE = 2
	  NTOT(1) = 2
	  NTOT(2) = 2
	  X(1) = R*SAZ
	  Y(1) = R*CAZ
	  X(2) = -X(1)
	  Y(2) = -Y(1)
	  X(3) = Y(1)
	  Y(3) = X(2)
	  X(4) = -X(3)
	  Y(4) = -Y(3)
	ELSEIF ((MT(6) .EQ. 0.0) .AND. (0.5*(MT(2)-MT(3)) .EQ. 0.0)) THEN
	  AZ = ATAN2(-MT(5),MT(4))
	  CAZ = COS(AZ)
	  SAZ = SIN(AZ)
	  NCURVE = 1
	  SEGST(1) = 1
	  NTOT(1) = 2
	  X(1) = R*SIN(AZ)
	  Y(1) = R*COS(AZ)
	  X(2) = -X(1)
	  Y(2) = -Y(1)
	ELSE
	  NCURVE = 0
	  K = 0
	  DO 300 I = 1, 2
	    IF (.NOT.(SHNODE(MT, 0.05*(0.5*PI), CI1, CZ1))) GOTO 300
	    PENUP = .TRUE.
	    IF (I .EQ. 2) THEN
	      CI1 = PI - CI1
	      CZ1 = CZ1 + PI
	    END IF
200	    IF (.NOT.SHNNXT(CI2, CZ2)) GO TO 300
	    IF (I .EQ. 2) THEN
	      CI2 = PI - CI2
	      CZ2 = CZ2 + PI
	    END IF
	    IF (RCLIP(CI1, CZ1, CI2, CZ2, (0.5*PI))) THEN
	      IF (PENUP) THEN
	        K = K + 1
	        NCURVE = NCURVE + 1
	        SEGST(NCURVE) = K
	        NTOT(NCURVE) = 1
	        CALL TA2XY(CI1, CZ1, X(K), Y(K))
	        PENUP = .FALSE.
	      END IF
	      K = K + 1
	      NTOT(NCURVE) = NTOT(NCURVE) + 1
	      CALL TA2XY(CI2, CZ2, X(K), Y(K))
	    ELSE IF (.NOT.PENUP) THEN
	      PENUP = .TRUE.
	    END IF
	    CI1 = CI2
	    CZ1 = CZ2
	    GO TO 200
300	  CONTINUE
	END IF
	IF (NCURVE .EQ. 0) RETURN
	DO 400 I=1,NCURVE
	  NN = NTOT(I)
	  NSTART = SEGST(I)
	  CALL PLTDAT(X(NSTART),0.0,-R,1.0,-R,R,
     .	    Y(NSTART),-R,-R,R,1.0,NN,ALINE,DASHL,SIZE,
     .	    NSYMBL,1,'XLIN','YLIN',XRTMAX,YTPMAX)
400	CONTINUE
	CALL TSEND
	RETURN
	END
