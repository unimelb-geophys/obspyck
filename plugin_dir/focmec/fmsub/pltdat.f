C+
	SUBROUTINE PLTDAT(X,DELX,XORG,XSCALE,XMININ,XMAXIN,
     .		Y,YORG,YMININ,YMAXIN,YSCALE,N,PTLINE,DASHL,SIZE,
     .		NSYMBL,NPREC,XLOG,YLOG,XRTMAX,YTPMAX)
C
C     IF PTLINE='LINE', THIS IS A CONTINUOUS LINE PLOT OF Y VS X
C	   IF 'DASH' IT IS A DASHED-LINE PLOT OF Y VS X
C		  'DASHL' IS THEN THE LENGTH OF THE DASH
C		  'SIZE' IS THEN THE DISTANCE BETWEEN START OF DASHES
C	   OTHERWISE IT IS POINT PLOT OF Y VS X WITH SYMBOLS
C		  'SIZE' IS THEN THE HEIGHT OF THE SYMBOL IN INCHES
C		  'NYSMBL' IS THEN THE NUMBER FOR THE SYMBOL
C     IF DELX IS ZERO, IT PLOTS THE N-POINT Y ARRAY VS THE X ARRAY
C     IF DELX .NE. 0, IT TAKES X(1) TO BE THE FIRST X AND DELX AN
C	   INCREMENT IN X - USED WHEN X HAS EQUAL INCREMENTS
C     IF XLOG IS 'XLOG' OR YLOG IS 'YLOG', IT PLOTS THE APPROP. LOG
C     IF XRTMAX IS TRUE, X INCREASES TO THE RIGHT
C     IF YTPMAX IS TRUE Y INCREASES UPWARDS
C     IF NPREC=1, SINGLE PRECISION, IF NPREC=2, DOUBLE PRECISION
C     LAST MODIFIED FEBRUARY 1985 BY J. A. SNOKE
C	2/85:  No longer draws lines on boundaries if outside
C	  region.  Also, does a linear extrapolation to boundary
C	  if one point is outside region.
C	28 November 1988: If logx or logy, do not need to input limits
C	  as powers of 10.  It figures it out.
C	22 January 1989:  Took out statement numbers.  Hopefully still works
C	21 August 1991 sun version.  Changed nprec logic
C-
	LOGICAL XOK,YOK,IPLOT,LINE,XARRAY,LOGX,LOGY,XRTMAX,YTPMAX
	LOGICAL NOTNOW,NOMORE,OLDXOK,OLDYOK
	DIMENSION X(*),RMAX(2),Y(*),JPLOT(2)
	character*4 xlog,ylog,ptline
	DATA JPLOT/2,3/
	XPLOT(A) = XORG + XSIGN*A
	YPLOT(A) = YORG + YSIGN*A
C
	IF (XRTMAX) XSIGN = 1.0
	  iF (.NOT.XRTMAX) XSIGN = -1.0
	IF (YTPMAX) YSIGN = 1.0
	  iF (.NOT.YTPMAX) YSIGN = -1.0
	XMIN = XMININ
	XMAX = XMAXIN
	IF (XLOG .NE. 'XLOG') THEN 
	  LOGX = .FALSE.
	  LLX = 1
	  XSPAN = (XMAX - XMIN)*XSCALE
	ELSE
	  LOGX = .TRUE.
	  LLX = 2
	  XSPAN = ALOG10(XMAX/XMIN)*XSCALE
	END IF
	YMIN = YMININ
	YMAX = YMAXIN
	IF (YLOG .NE. 'YLOG') THEN
	  LOGY = .FALSE.
	  LLY = 1
	  YSPAN = (YMAX - YMIN)*YSCALE
	ELSE
	  LOGY = .TRUE.
	  LLY = 2
	  YSPAN = ALOG10(YMAX/YMIN)*YSCALE
	END IF
	XARRAY = .FALSE.
	  IF (DELX .EQ. 0.0) XARRAY = .TRUE.
	IF (.not.(PTLINE .eq. 'DASH' .or. ptline .eq. 'LINE')) THEN
C
C	   SYMBOLS AT EACH POINT
C
	  DO J=1,N
	    XNOW = XX(X,J,DELX,NPREC,XARRAY)
	    XNOW = XY(XMIN,XSCALE,XNOW,LLX)
	    IF (XNOW .GT. XSPAN .OR. XNOW .LT. 0.0) THEN
	      IF ((.NOT.IPLOT) .AND.(.NOT.XARRAY)) N = J
	    ELSE
	      ynOW = XX(Y,J,0.0,NPREC,.TRUE.)
	      YNOW = XY(YMIN,YSCALE,YNOW,LLY)
	      IF (.NOT.(YNOW .GT. YSPAN .OR. YNOW .LT. 0.0))
     *	        CALL SYMBOL(XPLOT(XNOW),YPLOT(YNOW),SIZE,NSYMBL,0.0,-1)
	    END IF
	  END DO
	  call tsend
	  return
	end if
	XN = XX(X,1,DELX,NPREC,XARRAY)
	XNOW = XY(XMIN,XSCALE,XN,LLX)
	YNOW = XX(Y,1,0.0,NPREC,.TRUE.)
	YNOW = XY(YMIN,YSCALE,YNOW,LLY)
	XOK = (XNOW .GE. 0.0 .AND. XNOW .LE. XSPAN)
	YOK = (YNOW .GE. 0.0 .AND. YNOW .LE. YSPAN)
	if (XOK .and. YOK) then
	  NSTART = 1
	  OLDX = XNOW
	  OLDY = YNOW
	  OLDXOK = XOK
	  OLDYOK = YOK
	else
	  XOK = .FALSE.
	  YOK = .FALSE.
	  NSTART = 0
	  DO WHILE (.NOT.(XOK .AND. YOK))
	    NSTART = NSTART + 1
	    IF (NSTART .GE. N) RETURN
	    OLDX = XNOW
	    OLDY = YNOW
	    OLDXOK = XOK
	    OLDYOK = YOK
	    XN = XX(X,NSTART,DELX,NPREC,XARRAY)
	    XNOW = XY(XMIN,XSCALE,XN,LLX)
	    ynOW = XX(Y,NSTART,0.0,NPREC,.TRUE.)
	    YNOW = XY(YMIN,YSCALE,YNOW,LLY)
	    XOK = (XNOW .GE. 0.0 .AND. XNOW .LE. XSPAN)
	    YOK = (YNOW .GE. 0.0 .AND. YNOW .LE. YSPAN)
	  END DO
	end if
	IF (NSTART .EQ. 1) THEN
	  CALL PLOT(XPLOT(XNOW),YPLOT(YNOW),3)
	ELSE
	  CALL PROJKT(OLDX,OLDY,XNOW,YNOW,XON,YON,XSPAN,YSPAN,
     .	    OLDXOK,OLDYOK)
	  CALL PLOT(XPLOT(XON),YPLOT(YON),3)
	  OLDX = XON
	  OLDY = YON
	END IF
	IPLOT = .TRUE.
	LINE = .TRUE.
	NOMORE = .FALSE.
	IF (PTLINE .EQ. 'LINE') THEN
C
C	   PLOTS A CONTINUOUS LINE
C
	  DO J=NSTART,N
	    IF (J .GT. NSTART) THEN
	      OLDX = XNOW
	      OLDY = YNOW
	      OLDXOK = XOK
	      OLDYOK = YOK
	    END IF
	    XNOW = XX(X,J,DELX,NPREC,XARRAY)
	    XNOW = XY(XMIN,XSCALE,XNOW,LLX)
	    IF (XNOW .GE. 0.0 .AND. XNOW .LE. XSPAN) THEN 
	      XOK = .TRUE.
	    ELSE
	      IF ((.NOT.IPLOT) .AND.(.NOT.XARRAY)) THEN
	        NOMORE = .TRUE.
	        N = J
	      END IF
	      XOK = .FALSE.
	    END IF
	    IF (.NOT.NOMORE) THEN
	     ynOW = XX(Y,J,0.0,NPREC,.TRUE.)
	     YNOW = XY(YMIN,YSCALE,YNOW,LLY)
	     IF (YNOW .GE. 0.0 .AND. YNOW .LE. YSPAN) THEN 
	       YOK = .TRUE.
	     ELSE
	       YOK = .FALSE.
	     END IF
	     IF (XOK .AND. YOK) THEN
	       IF (.NOT.IPLOT) THEN
	         IPLOT = .TRUE.
	         CALL PROJKT(OLDX,OLDY,XNOW,YNOW,XON,YON,XSPAN,YSPAN,
     .	           OLDXOK,OLDYOK)
	         CALL PLOT(XPLOT(XON),YPLOT(YON),3)
	       END IF
	       CALL PLOT(XPLOT(XNOW),YPLOT(YNOW),2)
	     ELSE IF (IPLOT) THEN
	       IPLOT = .FALSE.
	       CALL PROJKT(XNOW,YNOW,OLDX,OLDY,XON,YON,XSPAN,YSPAN,
     .	         XOK,YOK)
	       CALL PLOT(XPLOT(XON),YPLOT(YON),2)
	     END IF
	    END IF
	  END DO
	ELSE
C
C	DASHED LINE
C
	  RMAX(1) = DASHL
	  RMAX(2) = SIZE
	  DIST = 0.0
	  DO J=NSTART,N
	    IF (J .GT. NSTART) THEN
	      OLDX = XNOW
	      OLDY = YNOW
	      OLDXOK = XOK
	      OLDYOK = YOK
	    END IF
	    XNOW = XX(X,J,DELX,NPREC,XARRAY)
	    XNOW = XY(XMIN,XSCALE,XNOW,LLX)
	    IF (XNOW .GE. 0.0 .AND. XNOW .LE. XSPAN) THEN 
	      XOK = .TRUE.
	    ELSE
	      IF ((.NOT.IPLOT) .AND.(.NOT.XARRAY)) THEN
	        NOMORE = .TRUE.
	        N = J
	      END IF
	      XOK = .FALSE.
	    END IF
	    IF (.NOT.NOMORE) THEN
	     ynOW = XX(Y,J,0.0,NPREC,.TRUE.)
	     YNOW = XY(YMIN,YSCALE,YNOW,LLY)
	     IF (YNOW .GE. 0.0 .AND. YNOW .LE. YSPAN) THEN 
	       YOK = .TRUE.
	     ELSE
	       YOK = .FALSE.
	     END IF
	     NOTNOW = .FALSE.
	     IF (XOK .AND. YOK) THEN
	       IF (.NOT.IPLOT) THEN
	         IPLOT = .TRUE.
	         CALL PROJKT(OLDX,OLDY,XNOW,YNOW,XON,YON,XSPAN,YSPAN,
     .	 	  OLDXOK,OLDYOK)
	         CALL PLOT(XPLOT(XON),YPLOT(YON),3)
	         OLDX = XON
	         OLDY = YON
	       END IF
	       XTEMP = XNOW
	       YTEMP = YNOW
	     ELSE
	       IF (IPLOT) THEN
	         IPLOT = .FALSE.
	         CALL PROJKT(XNOW,YNOW,OLDX,OLDY,XON,YON,XSPAN,YSPAN,
     .	           XOK,YOK)
	         XTEMP = XON
	         YTEMP = YON
	       ELSE
	         NOTNOW = .TRUE.
	       END IF
	     END IF
	     IF (.NOT.NOTNOW) THEN
	      DISTDR = RMAX(2) + 1.0
	      DO WHILE (DISTDR .GE. RMAX(2))
	        DY = YTEMP - OLDY
	        DX = XTEMP - OLDX
	        DR = SQRT(DX**2 + DY**2)
	        DIST = DIST + DR
	        JDASH = 1
	        IF (.NOT.LINE) JDASH = 2
	        IF (DIST .LT. RMAX(JDASH)) THEN
	          CALL PLOT(XPLOT(XTEMP),YPLOT(YTEMP),JPLOT(JDASH))
	          DISTDR = RMAX(2) - 1.0
	        ELSE
	          DIST = DIST - DR
	          CANG = DX/DR
	          SANG = DY/DR
	          OLDX = OLDX + (RMAX(JDASH) - DIST)*CANG
	          OLDY = OLDY + (RMAX(JDASH) - DIST)*SANG
	          CALL PLOT(XPLOT(OLDX),YPLOT(OLDY),JPLOT(JDASH))
	          LINE = .NOT.LINE
	          DISTDR = DIST + DR
	          IF (LINE) DIST = 0.0	
	          IF (.NOT.LINE) DIST = RMAX(1)
	        END IF
	      END DO
	     END IF
	    END IF
	  END DO
	END IF
	CALL TSEND
	RETURN
	END
