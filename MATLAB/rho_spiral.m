function rho = rho_spiral(X, Y, DELTA_SP, NSP, ZSP, ASP, RS, LSP, ISP, PHI0_SP, MDISC, AD , BD)

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Compute the mass density associated with a spirally-perturbed 
% Miyamoto-Nagai disc.
% 
% Coded by L. J. Rossi (2014).
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

R = sqrt(X.^2 + Y.^2);

Z=0.0;

% 
g = 1/(LSP*tan(ISP))*log(1+(R/RS).^LSP)-PHI0_SP;

dg_dx=(X.*R.^(LSP-2))./(RS^LSP*tan(ISP)*(1+(R./RS).^LSP));
dg_dy=(Y.*R.^(LSP-2))./(RS^LSP*tan(ISP)*(1+(R./RS).^LSP));

d2g_dx2=((R).^(LSP-2)./(1+(R/RS).^LSP)+X.^2./R.*(((LSP-2)*R.^(LSP-3).*(1+(R/RS).^LSP)-R.^(LSP-2).*(LSP*R.^(LSP-1)/RS^LSP))./(1+(R/RS).^LSP).^2))/(RS^LSP*tan(ISP));
d2g_dy2=((R).^(LSP-2)./(1+(R/RS).^LSP)+Y.^2./R.*(((LSP-2)*R.^(LSP-3).*(1+(R/RS).^LSP)-R.^(LSP-2).*(LSP*R.^(LSP-1)/RS^LSP))./(1+(R/RS).^LSP).^2))/(RS^LSP*tan(ISP));

%
phi = atan2(Y,X);

dphi_dx=-Y./(X.^2+Y.^2);
dphi_dy= X./(X.^2+Y.^2);

d2phi_dx2=2*X.*Y./(X.^2+Y.^2).^2;
d2phi_dy2=-2*X.*Y./(X.^2+Y.^2).^2;

%
q = cos(NSP*(phi - g));

dq_dx=-sin(NSP*(phi-g)).*(NSP*(dphi_dx-dg_dx));
dq_dy=-sin(NSP*(phi-g)).*(NSP*(dphi_dy-dg_dy));

d2q_dx2=-cos(NSP*(phi-g)).*(NSP*(dphi_dx-dg_dx)).^2-sin(NSP*(phi-g)).*(NSP*(d2phi_dx2-d2g_dx2));
d2q_dy2=-cos(NSP*(phi-g)).*(NSP*(dphi_dy-dg_dy)).^2-sin(NSP*(phi-g)).*(NSP*(d2phi_dy2-d2g_dy2));

%
s_in = exp(-ASP*(R-RS).^2);

ds_in_dx = s_in.*(-2*ASP*(R-RS).*X./R);
ds_in_dy = s_in.*(-2*ASP*(R-RS).*Y./R);
ds_in_dz = 0.0;

d2s_in_dx2 = ds_in_dx.*(-2*ASP*(R-RS).*X./R) + s_in.*(-2*ASP.*(RS*X.^2./R^3 + 1 - RS./R));
d2s_in_dy2 = ds_in_dy.*(-2*ASP*(R-RS).*Y./R) + s_in.*(-2*ASP.*(RS*Y.^2./R^3 + 1 - RS./R));
d2s_in_dz2 =0.0;

%
s_out = 1.0;

ds_out_dx = s_out.*(2*ZSP*Z^2*X./R.^4);
ds_out_dy = s_out.*(2*ZSP*Z^2*Y./R.^4);
ds_out_dz = -s_out*(2*ZSP*Z./R.^2);

d2s_out_dx2 = ds_out_dx.*(2*ZSP*Z^2*X./R.^4) + s_out.*(2*ZSP*Z^2.*(1./R.^4-4*X.^2./R.^6));
d2s_out_dy2 = ds_out_dy.*(2*ZSP*Z^2*Y./R.^4) + s_out.*(2*ZSP*Z^2.*(1./R.^4-4*Y.^2./R.^6));
d2s_out_dz2 = -ds_out_dz.*(2*ZSP*Z./R.^2)-s_out.*(2*ZSP./R.^2);

%
Phi = - MDISC./(R.^2+(AD+(BD^2+Z^2)^0.5)^2).^0.5;

dPhi_dx = MDISC*X./(R.^2+(AD+(BD^2+Z^2)^0.5)^2).^1.5;
d2Phi_dx2 = MDISC./(R.^2+(AD+BD)^2).^(1.5)-3*X.^2*MDISC./(R.^2+(AD+BD)^2).^(2.5);

dPhi_dy = MDISC*Y./(R.^2+(AD+(BD^2+Z^2)^0.5)^2).^1.5;
d2Phi_dy2 = MDISC./(R.^2+(AD+BD)^2).^(1.5)-3*Y.^2*MDISC./(R.^2+(AD+BD)^2).^(2.5);

dPhi_dz = 0;
d2Phi_dz2 = MDISC*(AD+BD)./(R.^2+(AD+BD)^2).^(1.5)/BD;

d2Phi = d2Phi_dx2 + d2Phi_dy2 + d2Phi_dz2;

%
f_in = 1 + DELTA_SP.*q.*s_in;

df_in_dx = DELTA_SP*(dq_dx.*s_in + q.*ds_in_dx);
df_in_dy = DELTA_SP*(dq_dy.*s_in + q.*ds_in_dy);
df_in_dz = DELTA_SP*q.*ds_in_dz;

d2f_in_dx2 = DELTA_SP*(d2q_dx2.*s_in + 2*dq_dx.*ds_in_dx + q.*d2s_in_dx2);
d2f_in_dy2 = DELTA_SP*(d2q_dy2.*s_in + 2*dq_dy.*ds_in_dy + q.*d2s_in_dy2);
d2f_in_dz2 = DELTA_SP*q.*d2s_in_dz2;

d2Phi_in_dx2  = d2Phi_dx2.*f_in + 2*dPhi_dx.*df_in_dx + Phi.*d2f_in_dx2;
d2Phi_in_dy2  = d2Phi_dy2.*f_in + 2*dPhi_dy.*df_in_dy + Phi.*d2f_in_dy2;
d2Phi_in_dz2  = d2Phi_dz2.*f_in + 2*dPhi_dz.*df_in_dz + Phi.*d2f_in_dz2;

d2Phis_in = d2Phi_in_dx2 + d2Phi_in_dy2 + d2Phi_in_dz2;

%
f_out= 1 + DELTA_SP.*q.*s_out;

df_out_dx = DELTA_SP*(dq_dx.*s_out + q.*ds_out_dx);
df_out_dy = DELTA_SP*(dq_dy.*s_out + q.*ds_out_dy);
df_out_dz = DELTA_SP*q.*ds_out_dz;

d2f_out_dx2 = DELTA_SP*(d2q_dx2.*s_out + 2*dq_dx.*ds_out_dx + q.*d2s_out_dx2);
d2f_out_dy2 = DELTA_SP*(d2q_dy2.*s_out + 2*dq_dy.*ds_out_dy + q.*d2s_out_dy2);
d2f_out_dz2 = DELTA_SP*q.*d2s_out_dz2;


d2Phi_out_dx2  = d2Phi_dx2.*f_out + 2*dPhi_dx.*df_out_dx + Phi.*d2f_out_dx2;
d2Phi_out_dy2  = d2Phi_dy2.*f_out + 2*dPhi_dy.*df_out_dy + Phi.*d2f_out_dy2;
d2Phi_out_dz2  = d2Phi_dz2.*f_out + 2*dPhi_dz.*df_out_dz + Phi.*d2f_out_dz2;

d2Phis_out = d2Phi_out_dx2 + d2Phi_out_dy2 + d2Phi_out_dz2;
%

rho = ones(length(X),length(Y));

for i = 1 : length(X);    
    for j = 1 : length(Y);        
        if R(i,j) > RS    
             rho(i,j) = d2Phis_out(i,j)/(4*pi);             
        elseif  R(i,j) <= RS             
             rho(i,j) = d2Phis_in(i,j)/(4*pi);              
       end        
    end    
end

return