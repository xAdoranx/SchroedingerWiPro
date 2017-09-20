Solves eigenfunctions of the schroedinger equation for specific eigenenergies.

Installation:

1. verify the file: "config.mk"
   - edit the local pathes of the lapack library depending on the local device
2. "make"
3. "make clean"
4. check installation with "make test"

The program is used by the console command ./schroedinger
It needs the input file 

Input in file schrodinger.inp
formatted like:

2.0		% mass
-2.0 2.0 1999	% xMin, xMax, nPoints
1 15 		% first and last eigenvalue to calculate
linear		% interpolation type
2		% nr. of interpolation points
-2.0 0.0	% xy-declarations
 2.0 0.0	%

Interpolation type options:

	 linear
	 polynomial
