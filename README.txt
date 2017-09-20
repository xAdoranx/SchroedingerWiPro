Solves eigenfunctions of the schroedinger equation for specific eigenenergies.

Installation:

1. make
2. make realclean

use by ./schroedinger

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
	 polynom
