C+
	PROGRAM RADIAT
C
C	Radiation factors and amplitude ratios for P, SV and SH
C	21 July 2001:  Corrected the statement of sign convention for SH
C-
	DIMENSION APS(2)
	LOGICAL TRUTH
	RD = 45.0/ATAN(1.0)
	open(2,file='radiat.lst',status='unknown')
	write(*,*) 'Sign conventions: P (up), SV (toward station),',
     1    ' SH (left, back to event) are +'
	write(2,*) 'Sign conventions: P (up), SV (toward station),',
     1    ' SH (left, back to event) are +'
	WRITE(2,1)
1	FORMAT(/,T5,'Dip',T13,'Strike',T23,'Rake',T30,'Azimuth',
     .    T40,'P TOA',T49,'S TOA',T57,'P factor',T69,'SV',
     .    T78,'SH')
100	DIP = VALUE ('Dip')
	STRIKE = VALUE('Strike')
	RAKE = VALUE('Rake')
	DIPR = DIP/RD
	STRR = STRIKE/RD
	RAKER = RAKE/RD
200	AZIM = VALUE('Azimuth')
	PTOA = VALUE('P Take off angle - relative to down')
	APS(1) = 180.0 -PTOA
	STOA = VALUE('S Take off angle - relative to down')
	APS(2) = 180.0 - STOA
	CALL RADPAT(DIP,STRIKE,RAKE,AZIM,APS,PR,SVR,SHR)
	WRITE(*,2) DIP,STRIKE,RAKE,AZIM,PTOA,STOA,PR,SVR,SHR
	WRITE(2,2) DIP,STRIKE,RAKE,AZIM,PTOA,STOA,PR,SVR,SHR
2	FORMAT(8F9.3,F9.2)
	IF (PR .NE. 0.0) THEN
	  SHRAT = SHR/PR
	  SVRAT = SVR/PR
	  SVSH = SVR/SHR
	  WRITE(2,*) '     SH/P =',SHRAT,'   SV/P =',SVRAT,'   SV/SH =',
     1		SVSH
	END IF
	IF (TRUTH('More?..[Y]')) THEN
	  IF (TRUTH('Same earthquake?..[Y]')) go to 200
	  go to 100
	end if
	stop
	end
C+
	SUBROUTINE RADPAT(DIP,STRIKE,SLIP,AZD,APS,PR,SVR,SHR)
C
C     Input the dip, strike and slip - in the Aki & Richards convention
C         plus the azimuth and the P & S takeoff angles from the focus.
C     Output is the radiation-pattern terms for P, SV and SH.
C-
	REAL N(3)
	DIMENSION A(3),APS(2),R(2,3),TH(3),PH(3)
	DR=1./57.29578
	PH(3)=0.0
	D=DIP*DR
	ST=STRIKE*DR
	SL=SLIP*DR
	A(1)=COS(SL)*COS(ST)+SIN(SL)*COS(D)*SIN(ST)
	A(2)=COS(SL)*SIN(ST)-SIN(SL)*COS(D)*COS(ST)
	A(3)=-SIN(SL)*SIN(D)
	N(1)=-SIN(ST)*SIN(D)
	N(2)=COS(ST)*SIN(D)
	N(3)=-COS(D)
	AZ=AZD*DR
	DO I=1,2
	  AT=APS(I)*DR
	  R(I,1)=SIN(AT)*COS(AZ)
	  R(I,2)=SIN(AT)*SIN(AZ)
	  R(I,3)=-COS(AT)
	END DO
C  THETA AND PHI ARE REVERSED FROM THE USUAL CONVENTION
	TH(1)=COS(AT)*COS(AZ)
	TH(2)=COS(AT)*SIN(AZ)
	TH(3)=SIN(AT)
	PH(1)=SIN(AZ)
	PH(2)=-COS(AZ)
	RPA=0.0
	RPN=0.0
	RSA=0.0
	RSN=0.0
	PHA=0.0
	PHN=0.0
	THA=0.0
	THN=0.0
	DO J=1,3
	  RPA=RPA+R(1,J)*A(J)
	  RPN=RPN+R(1,J)*N(J)
	  RSA=RSA+R(2,J)*A(J)
	  RSN=RSN+R(2,J)*N(J)
	  PHA=PHA+PH(J)*A(J)
	  PHN=PHN+PH(J)*N(J)
	  THA=THA+TH(J)*A(J)
	  THN=THN+TH(J)*N(J)
	END DO
C  P IS + ALONG RAY, SV + DOWN AND SH TO THE LEFT LOOKING FROM EQ
	PR = 2.*RPA*RPN
	SVR = RSN*THA+RSA*THN
	SHR = RSN*PHA+RSA*PHN
	RETURN
	END
