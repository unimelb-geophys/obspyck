C+
	program dsretc
C
C	Based on the Aki & Richards convention, gives all 
C	  representations of a focal mechanism for input
C	  Dip, Strike, Rake    or
C	  A and N trend and plunge    or
C	  P and T trend and plunge.
C
C	The code is Fortran 77 for a VAX/VMS system
C	Subroutine specific to this program are
C	  FMREPS,PTTPIN,ANTPIN,DSRIN,AN2MOM,V2TRPL,TRPL2V,AN2DSR, MT_IN
C	21 August 1991:  sun version
C	30 December 1993 Incuded Stuart Sipkin's beachball
C	7 March 2002: Added moment tensor input
C	June 2009: Added angle of A with plane formed by vertical
C	  and the trend of B.  Also added angle of N.
C	  Also added "save" to two subroutines to fix problem with
C	  printer plots on the screen.
C-
	LOGICAL AN,PT,DSR,TRUTH,first,MT
	character*80 getstring,commnt
	REAL*4 MOMTEN(6)
	DIMENSION PTTP(4),ANGS(3),ANGS2(3),ANBTP(6)
C
C	If your compiler complains about form='print', leave it out.
C
	open(unit=2,file='dsretc.lst',status='unknown')

100	COMMNT = getstring('Comment')
	CALL TIMDAT(2,'dsretc')
200	write(2,'(/,5x,a)') commnt(1:lenc(commnt))
	WRITE(*,*) 'Can enter P & T or D, S & R, moment tensor, or A & N'
	DSR = .FALSE.
	AN = .FALSE.
	PT = .FALSE.
	MT = .false.
	IF (TRUTH('Dip, Strike and Rake?..[Y]')) THEN
	  DSR = .TRUE.
	  CALL PRINTX('Enter Dip, Strike and Rake (degrees)')
	  READ(*,*) (ANGS(J),J=1,3)
	ELSE IF (TRUTH('P and T axes trend and plunge?..[Y]')) THEN
	  PT = .TRUE.
	  CALL PRINTX
     .	  ('Enter trend and plunge for P and T (t,p,t,p)')
	  READ(*,*) (PTTP(J),J=1,4)
	else if (truth('Moment-tensor input?...[Y]')) then
	  MT = .true.
	ELSE
	  AN = .TRUE.
	  CALL PRINTX
     .	  ('Enter trend and plunge for A and N (t,p,t,p)')
	  READ(*,*) (ANBTP(J),J=1,4)
	END IF
	CALL FMREPS(ANBTP,ANGS,PTTP,ANGS2,AN,PT,DSR,mt,MOMTEN,2,6)
	call ang(anbtp)
	first = .true.
	call bball(momten,pttp(1),pttp(2),pttp(3),pttp(4),2,first)
	call bball(momten,pttp(1),pttp(2),pttp(3),pttp(4),6,first)
	IF (.NOT.TRUTH('Run some more?...[Y]')) STOP
        first = .true.
	IF (TRUTH('Same comment?..[Y]')) GO TO 200
	GO TO 100
	END
C+
      subroutine bball(g,pazim,pplng,tazim,tplng,unit,first)

c ...... generate printer plot rendition of lower hemisphere 
c        equal area projection
C	g has the six elements of the moment tensor, the rest are the
C	  plunge and trends of the P and T axes in degrees. unit is the output
C	  unit.
C	From Stuart Sipkin and Bob Uhrhammer 1993
C	1 October 2001: Replaced his sind, etc, with sin as not all compilers
C		know about degree versions of sin, cos, etc.
C-
      dimension g(6)
      integer unit
      character*1 ach(39,72),aplus,aminus,apaxis,ataxis,ablank
      logical first
c
      data aplus,aminus,apaxis,ataxis,ablank /'#','-','P','T',' '/
      data radius /1.41/
c
c ...... construct lower hemisphere fps 
c
      save
      rd = 45.0/atan(1.0)
      r0=radius
      x0=r0+0.250
      y0=r0+0.500
      ix0=12.*x0
      iy0=6.5*y0
      do 3 i=1,2*ix0
      do 2 j=1,2*iy0
      dx=real(i-ix0)/12.
      dy=-real(j-iy0)/6.5
      dd=dx*dx+dy*dy
      if(dd.gt.0.) then
        del=sqrt(dd)
      else
        del=0.
      endif
      if((dx.eq.0.).and.(dy.eq.0.)) then
        theta=0.
      else
        theta=rd*atan2(dx,dy)
      endif
      if(del.gt.r0) then
        ach(j,i)=ablank
        go to 1
      endif
      if(del.ge.r0) then
        aoi=90.0
      else
        aoi=90.*del/r0
      endif
      if(polar(g,aoi,theta,first).gt.0.) then
        ach(j,i)=aplus
      else
        ach(j,i)=aminus
      endif
    1 continue
    2 continue
    3 continue
c
c ...... add P & T axis
c
      ixp=nint(r0*12.*(90.-pplng)*sin(pazim/rd)/90.+real(ix0))
      iyp=nint(-r0*6.5*(90.-pplng)*cos(pazim/rd)/90.+real(iy0))
      do 5 i=ixp-1,ixp+1
      do 4 j=iyp-1,iyp+1
      ach(j,i)=ablank
    4 continue
    5 continue
      ach(iyp,ixp)=apaxis
      ixt=nint(r0*12.*(90.-tplng)*sin(tazim/rd)/90.+real(ix0))
      iyt=nint(-r0*6.5*(90.-tplng)*cos(tazim/rd)/90.+real(iy0))
      do 7 i=ixt-1,ixt+1
      do 6 j=iyt-1,iyt+1
      ach(j,i)=ablank
    6 continue
    7 continue
      ach(iyt,ixt)=ataxis
c
c ...... add fps plot
c
      do 8 i=1,2*iy0-2
      write(unit,'(5x,72a1)') (ach(i,j),j=1,2*ix0)
    8 continue
c
      return
      end

      real*4 function polar(g,aoi,theta,first)
c
c ...... compute first motion podsretc_CMT.lstlarity as a function of aoi & theta
c        for a moment tensor for a double-couple solution.
C	Conventions differ slightly from Sipkin.  My moment tensor is derived
C	  from the outer product of two vectors and is hence normalized.  The
C	  order is also different from his, apparently.  I also did not know
C	  cosd and sind existed.
c
      dimension g(6)
      real mxx,mxy,mxz,myy,myz,mzz
      logical first
c
      save
      rd = 45.0/atan(1.0)
      if(first) then
        mxx= g(2)
        mxy=-g(6)
        mxz= g(4)
        myy= g(3)
        myz=-g(5)
        mzz= g(1)
        first = .false.
      endif
	x = cos(theta/rd)*sin(aoi/rd)
	y = sin(theta/rd)*sin(aoi/rd)
	z = cos(aoi/rd)
c
      polar = x*mxx*x + 2*x*mxy*y + 2*x*mxz*z + 2*y*myz*z +y*myy*y
     1        +z*mzz*z
c
      return
      end
C+
	subroutine ang(anbtp)
C
C	Given A and B trend and plunge, find and print angle A makes
C	with the plane formed by the vertical and the trend of B.
C	See subroutines srchfm.f and fltsol.f for explanation
C-
	dimension anbtp(6), btrpl(2), xtrpl(2), atrpl(2), a(3),x(3)
	rddeg = 45.0/atan(1.0)
	atrpl(1) = anbtp(1)
	atrpl(2) = anbtp(2)
	btrpl(1) = anbtp(5)
	btrpl(2) = anbtp(6)
	xtrpl(1) = btrpl(1) + 180.0
	xtrpl(2) = 90.0 - btrpl(2)
	do j=1,2
	  atrpl(j) = atrpl(j)/rddeg
	  xtrpl(j) = xtrpl(j)/rddeg
	enddo
	call trpl2v(atrpl,a)
	call trpl2v(xtrpl,x)
	cosangle = 0.0
	do j=1,3
	  cosangle = cosangle + a(j)*x(j)
	enddo
	angle = rddeg *acos(cosangle)
	write(*,1) angle,90-angle
	write(2,1) angle,90-angle
1       FORMAT(5x,'"A" angle with plane defined by vertical & B trend:',
     2         F5.1,/,5x,
     3       '"N" angle with plane defined by vertical & B trend:',F5.1)
	return
	end
