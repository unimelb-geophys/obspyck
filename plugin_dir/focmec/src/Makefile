#  Makefile  for creating executables in FOCMEC package.  FCMP is set on entry

LIB = ../lib/libiasp.a ../lib/libfm.a

all:	dsretc focmec_prep focmec focplt freesurf \
hypo2foc radiat ratio_prep sgftops sgfswap symdemo vwbeetle

clean:
	cd ../bin; \rm -f dsretc focmec_prep focmec focplt freesurf \
hypo2foc radiat ratio_prep sgftops sgfswap symdemo ttimes vwbeetle; \
cd ../lib; \rm -f ak135.hed ak135.tbl iasp91.hed iasp91.tbl libfm.a libiasp.a

dsretc:	dsretc.f
	$(FCMP) dsretc.f $(LIB) -C -o ../bin/dsretc

focmec_prep: focmec_prep.f
	$(FCMP) focmec_prep.f $(LIB) -C -o ../bin/focmec_prep

focmec:	focmec.f
	$(FCMP) focmec.f $(LIB) -C -o ../bin/focmec

focplt: focplt.f
	$(FCMP) focplt.f $(LIB) -C -o ../bin/focplt

freesurf:	freesurf.f
	$(FCMP) freesurf.f $(LIB) -C -o ../bin/freesurf

hypo2foc:	hypo2foc.f
	$(FCMP) hypo2foc.f $(LIB) -C -o ../bin/hypo2foc

radiat:	radiat.f
	$(FCMP) radiat.f $(LIB) -C -o ../bin/radiat

sgftops: sgftops.c 
	gcc -o ../bin/sgftops sgftops.c

sgfswap: sgfswap.c 
	gcc -o ../bin/sgfswap sgfswap.c

ratio_prep: ratio_prep.f
	$(FCMP) ratio_prep.f $(LIB) -C -o ../bin/ratio_prep
	
symdemo: symdemo.f
	$(FCMP) symdemo.f $(LIB) -C -o ../bin/symdemo

vwbeetle: vwbeetle.f
	$(FCMP) vwbeetle.f $(LIB) -C -o ../bin/vwbeetle
