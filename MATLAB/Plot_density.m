%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 
%
% Produce the logarithmic colormap of the density distribution 
% projected on the galactic plane as it is at the start of the simulation. 
%
% Coded by L.J. Rossi (2014).
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

clear all
close all

% Read the inputN.dat file, defining the mass model
Read_inputN

% Set up the grid on which the density distribution will be evaluated
x = linspace(-20,20,1500);
y = x;
[X,Y] = meshgrid(x,y);
R = sqrt(X.^2 + Y.^2);

% Compute the mass density associated to each mass component 
RHO_B1 = rho_bulge(R, MBULGE1, BB1);
RHO_B2 = rho_bulge(R, MBULGE2, BB2);


RH0_BAR1 = rho_bar(X, Y, PHI0_B1, NBAR1, ABAR1, BBAR1, CBAR1, MBAR1);
RH0_BAR2 = rho_bar(X, Y, PHI0_B2, NBAR2, ABAR2, BBAR2, CBAR2, MBAR2);

RHO_D1 = rho_spiral(X, Y, DELTA_SP, NSP, ZSP, ASP, RS, LSP, ISP, PHI0_SP, MDISC1, AD1 , BD1);
RHO_D2 = rho_spiral(X, Y, DELTA_SP, NSP, ZSP, ASP, RS, LSP, ISP, PHI0_SP, MDISC2, AD2 , BD2);
RHO_D3 = rho_spiral(X, Y, DELTA_SP, NSP, ZSP, ASP, RS, LSP, ISP, PHI0_SP, MDISC3, AD3 , BD3);

RHO_H = rho_halo(R,HCHOICE,VHL,AHL,MHALO,AH,MSERH,REH,NSERH);

RHO_TOT = RHO_B1 + RHO_B2 + RH0_BAR1 + RH0_BAR2 + RHO_D1 + RHO_D2 + RHO_D3 + RHO_H;

% Plot the results as a mesh plot. The units are expressed in Mo/pc^3.
%cmap = load('redblue_cmap.dat')/255;
%cmap = load('kripto_cmap.dat')/255;
%cmap = load('Custom2_cmap.dat')/255;
colormap(bone(255))

mesh(X,Y,log10(RHO_TOT/1e9))
axis([-20 20 -20 20]);
axis square
xlabel('x (kpc)','fontsize',12)
ylabel('y (kpc)','fontsize',12)
set(gca,'fontsize',10,'xtick',(-25:5:25),'ytick',(-25:5:25))
grid off
colorbar('EastOutside','fontsize',10)
