%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Read the InputN.dat file, defining the parameters of the mass
% model. The input file has to be in the current working directory.
%
% Coded by L. J. Rossi (Melbourne, 2014).
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fid = fopen('InputN.dat');

fgets(fid); I = sscanf(fgets(fid),'%f%f%f%f%f'); 
[N_star,T_start,T_stop,T_step,Abserr] = deal(I(1),I(2),I(3),I(4),I(5));

fgets(fid); fgets(fid); I = sscanf(fgets(fid),'%f%f');
[R0,V0] = deal(I(1),I(2));

fgets(fid); fgets(fid); I = sscanf(fgets(fid),'%f%f'); 
[MBULGE1,BB1] = deal(I(1),I(2));

fgets(fid); fgets(fid); I = sscanf(fgets(fid),'%f%f');
[MBULGE2,BB2] = deal(I(1),I(2));

fgets(fid); fgets(fid); I = sscanf(fgets(fid),'%f%f%f');
[MSER,RE,NSER] = deal(I(1),I(2),I(3));

fgets(fid); fgets(fid); I = sscanf(fgets(fid),'%f%f%f');
[MDISC1,AD1,BD1] = deal(I(1),I(2),I(3));

fgets(fid); fgets(fid); I = sscanf(fgets(fid),'%f%f%f');
[MDISC2,AD2,BD2] = deal(I(1),I(2),I(3));

fgets(fid); fgets(fid); I = sscanf(fgets(fid),'%f%f%f');
[MDISC3,AD3,BD3] = deal(I(1),I(2),I(3));

fgets(fid); fgets(fid); I = sscanf(fgets(fid),'%f%f%f%f%f%f%f');
[MBAR1,ABAR1,BBAR1,CBAR1,NBAR1,OMEGA_B1,PHI0_B1] = deal(I(1),I(2),I(3),I(4),I(5),I(6),I(7));	

fgets(fid); fgets(fid); I = sscanf(fgets(fid),'%f%f%f%f%f%f%f');
[MBAR2,ABAR2,BBAR2,CBAR2,NBAR2,OMEGA_B2,PHI0_B2] = deal(I(1),I(2),I(3),I(4),I(5),I(6),I(7));

fgets(fid); fgets(fid); I = sscanf(fgets(fid),'%f%f%f%f%f%f%f%f');
[HCHOICE,VHL,AHL,MHALO,AH,MSERH,REH,NSERH] = deal(I(1),I(2),I(3),I(4),I(5),I(6),I(7),I(8));

fgets(fid); fgets(fid); I = sscanf(fgets(fid),'%f%f%f%f%f%f%f%f%f');
[NSP,RS,ISP,OMEGA_SP,PHI0_SP,LSP,ZSP,ASP,DELTA_SP] = deal(I(1),I(2),I(3),I(4),I(5),I(6),I(7),I(8),I(9));

PHI0_B1 = PHI0_B1*pi/180;
PHI0_B2 = PHI0_B2*pi/180;

PHI0_SP = PHI0_SP*pi/180;
ISP = ISP*pi/180;