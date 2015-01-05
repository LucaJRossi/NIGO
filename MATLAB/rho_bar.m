function rho = rho_bar(X, Y, PHI0_B, NBAR, ABAR, BBAR, CBAR, MBAR)

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Compute the mass density associated with a Ferres triaxial ellipsoid.
% 
% Coded by L. J. Rossi (2014).
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

rho_c=MBAR*gamma(2*NBAR+4)/(gamma(NBAR+1)*gamma(NBAR+2))/(2^(2*NBAR+3)*pi*ABAR*BBAR*CBAR);

Xb=X.*cos(PHI0_B)-Y.*sin(PHI0_B);
Yb=+X.*sin(PHI0_B)+Y.*cos(PHI0_B);

M=sqrt(Xb.^2/ABAR^2+Yb.^2/BBAR^2);

rho = ones(length(X), length(Y));

for i=1:length(X)    
    for j=1:length(Y)
        if M(i,j)<1
            rho(i,j)=rho_c*(1-M(i,j).^2).^NBAR;       
        else
            rho(i,j)=0;            
        end        
    end    
end

return