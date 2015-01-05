MODULE PARAMETERS
!
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!
! Define the common parameters of the mass model and integration options.
! 
! Coded by L. J. Rossi (Melbourne, 2014).
!
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!
  IMPLICIT NONE
!
  INTEGER (4) N_STAR,N_SNAP
  REAL (8) T_START,T_STOP,DELTA_T,ABSERR,R0,V0,MBULGE1,MBULGE2,BB1,BB2,MDISC1,MDISC2,MDISC3,&
           AD1,AD2,AD3,BD1,BD2,BD3,MBAR1,ABAR1,BBAR1,CBAR1,NBAR1,PHI0_B1,OMEGA_B1,MBAR2,ABAR2,&
           BBAR2,CBAR2,NBAR2,PHI0_B2,OMEGA_B2,HCHOICE,MHALO,AH,VHL,AHL,NSP,RS,ISP,OMEGA_SP,&
           PHI0_SP,LSP,ZSP,ASP,DELTA_SP,G,PI,CONST,MSER,RE,NSER,MSERH,REH,NSERH,BSER,&
           AA,GG,BSERH,AAH,GGH
!
END MODULE PARAMETERS
