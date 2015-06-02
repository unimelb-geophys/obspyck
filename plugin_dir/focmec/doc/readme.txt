July 2008 by Arthur Snoke (snoke@vt.edu) Updated in June 2009

This package contains software for determining and displaying double-couple
earthquake focal mechanisms. Input are polarities (P, SV , SH) and amplitude
ratios (SV/P, SH/P, SV/SH). The main program, Focmec (coded in Fortran 77),
performs an efficient, systematic search of the focal sphere and reports
acceptable solutions based on selection criteria for the number of polarity
errors and errors in amplitude ratios. The search of the focal sphere is
uniform in angle, with selectable step size and bounds. The selection
criteria for both polarities and angles allow adjustment through weightings
for near-nodal solutions. Published applications include determinations of
best-constrained fault-plane solutions for suites of earthquakes recorded at
local to regional distances, analysis of large earthquakes observed at
teleseismic distances, and the use of recorded polarities and relative
amplitudes to produce waveform synthetics.

The package as distributed is a gzipped tar file, focmec.tgz (accompanied by
this README file and a copy of the FOCMEC manual). On Unix, one can both
uncompress and expand this file with a single command:    

      tar -xzf focmec.tgz

or, if you are using an older version of tar that does not recognize -z,

      gzip -d -c focmec.tgz | tar -xf - 

This operation produces a directory ./focmec  Instructions for building and
testing the package are in HTML format.  One starts by opening
./focmec/doc/focmec.html.  Included among the links is one to the manual: 
./focmec/doc/focmec_manual.pdf.

Changes made since July 2008:

June 2009: Program sgftops has been updated, but in ways that should be 
  transparent for FOCMEC package plot files.  Program sgfswap has been
  added; it swaps the byte order in the binary .sgf plot files.  The 
  write-up in the manual on graphics has been moved to Appendix D.
  
  Although the use of the FOCMEC search parameter worked, the description of
  it was incomplete and the printed output regarding it in Focmec runs
  was imprecise.  That has been corrected, and ANGLE has been added to the
  output of runs of Dsretc.  This can be useful when doing runs of Focmec
  with a single solution provided by an external source.  In the application
  within this package, it is for the CMT solution for the Sakhalin Island
  event.
  
  The discussion of the Sakhalin Island event example has bee expanded.  Now
  it shows explicitly how well (in this case) one can do with a sparse
  broadband network using S polarities and amplitude ratios.  Previously the
  showed only that the solution could be improved by adding the BB data to
  P-wave polarity data.
  
  The printer plots produced by program Dsretc did not work on some compilers.
  Adding a "save" to two subroutines fixed that.
  
  In Focplt, if one is adding solutions from a file after adding other
  solutions, there was no prompt to verify it was the correct file.  Now it
  does (changes in solplt.f).
  
  The command "make clean" in ./src now deletes all executables and libraries.
  
  Calls to symbol for actual symbols got warnings in f77.  No longer.
  
April 2009: The wording in prompts in program focplt have been modified
  to remove ambiguities about adding solutions to plots.  One can add
  several sets of solutions for the same set of focal mechanisms or different
  focal mechanisms to the same plot.  One answers yes to the prompt
  "Add more solutions to same plot?"
  
November 2008: If INFO was left out for a ratio, did not read line.
  Now it does.  In Oksol.f:  If both numerator and denominator are near the
  nodal surfaces (called N&D), the number of ratios is decreased by one.
  If the number of remaining ratios is not more than the number
  of bad ratios, the solution is classified as unacceptable.

August 2008: A code change in subroutine finrng.f of program remodl solved an 
  instability that occurred when using the Sun f95 compiler.
