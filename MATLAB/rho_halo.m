function rho = rho_halo(R, HCHOICE, VHL, AHL, MHALO, AH, MSERH, REH, NSERH)

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Compute the mass density associated with a dark matter halo, defined by
% the parameter HCHOICE.
% 
% Coded by L. J. Rossi (2014).
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Gravitational constant in kpc^3/Mo/s^2
G = 4.302e-6;

if HCHOICE == 1 % Logarithmic halo
    
    rho = VHL^2*(AHL^2 - R.^2)./(AHL^2 + R.^2)^2 /(4*pi*G);    
    
elseif HCHOICE == 2 % NFW halo
    
    rho = MHALO/(4*pi)./((R+AH).^2.*R);
    
elseif HCHOICE == 3 % Sersic halo
    
    b = 2*NSERH -1/3 + 0.009876/NSERH;
    p = 1 - 0.6097/NSERH + 0.05563/NSERH^2;
    rho0 = MSERH/(4*pi*REH^3*NSERH*b^(NSERH*(p-3))*gamma(NSERH*(3-p)));
    rho = rho0*(R./REH).^(-p).*exp(-b*(R./REH).^(1/NSERH));

end

return
