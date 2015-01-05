function rho = rho_bulge(R, MBULGE, BB)

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Compute the mass density associated with a Plummer sphere.
% 
% Coded by L. J. Rossi (2014).
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

rho = 3*BB^2*MBULGE./(R.^2+BB^2).^(5/2);

return