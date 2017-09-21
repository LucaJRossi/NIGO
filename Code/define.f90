SUBROUTINE DEFINE()
!
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!
! Definition of input parameters and options.
!    
!     N_STAR       Number of test particles
!     T_START      Value of the start time of the simulation (Myr)
!     T_STOP       Value of the end time of the simulation (Myr)
!     DELTA_T      Output time interval (Myr)
!     ABSERR       Absolute error determining the accuracy required to the integrator
!
!     R0           Galactocentric distance of the Sun (kpc)
!     V0           Velocity of the local standard of rest (km/sec)
!
!     MBULGE1      Mass of the Plummer sphere bulge 1 (solar masses) 
!     BB1          Softening length of the bulge 1 (kpc)  
!
!     MBULGE2      Mass of the Plummer sphere bulge 2 (solar masses) 
!     BB2          Softening length of the bulge 1 (kpc)
!
!     MSER         Mass of the Sèrsic component (solar masses)
!     RE           Projected half-light radius (kpc)
!     N            Sèrsic profile curvature
!
!     MDISC1       Mass of the Miyamoto-Nagai disc 1 (solar masses)  
!     AD1          Softening length of Miyamoto-Nagai potential 1 (kpc) 
!     BD1          Vertical softening lenght of the Miyamoto-Nagai disc 1 (kpc)
!
!     MDISC2       Mass of the Miyamoto-Nagai disc 2 (solar masses)  
!     AD2          Softening length of Miyamoto-Nagai potential 2 (kpc) 
!     BD2          Vertical softening lenght of the Miyamoto-Nagai disc 2 (kpc)     
!
!     MDISC3       Mass of the Miyamoto-Nagai disc 3 (solar masses)  
!     AD3          Softening length of Miyamoto-Nagai potential 3 (kpc) 
!     BD3          Vertical softening lenght of the Miyamoto-Nagai disc 3 (kpc)     
!
!     MBAR1        Mass of the bar 1 (solar masses)
!     ABAR1        First semi-axix of the bar 1 (kpc)
!     BBAR1        Second semi-axis of the bar 1 (kpc)
!     CBAR1        Third semi-axis of the bar 1 (kpc)
!     NBAR1        Density shape of the bar 1
!     PHI0_B1      Initial phase of the bar 1 (degrees)
!     OMEGA_B1     Pattern speed of the bar 1 (km/sec/kpc)
!
!     MBAR2        Mass of the bar 2 (solar masses)
!     ABAR2        First semi-axix of the bar 2 (kpc)
!     BBAR2        Second semi-axis of the bar 2 (kpc)
!     CBAR2        Third semi-axis of the bar 2 (kpc)
!     NBAR2        Density shape of the bar 2
!     OMEGA_B2     Pattern speed of the bar 2 (km/sec/kpc)
!     PHI0_B2      Initial phase of the bar 2 (degrees)
!
!     HCHIOCE      Halo chioce parameter = 0: no dark matter halo
!                                        = 1: logarithmic halo
!                                        = 2: NFW halo
!                                        = 3: Sersic halo
!     VHL          Asymptotic velocity of the logarithmic halo (km/s)
!     AHL          Scale length of the logarithmic halo (kpc)     
!     MHALO        Mass of the NFW halo (solar masses)
!     AH           Softening length of the NFW halo (kpc)
!     MSERH        Mass of the Sersic halo (solar masses)
!     REH          Scale radius of the Sersic halo (kpc)
!     NSERH        Curvature of the Sersic profile
!
!     NSP          Number of spiral arms
!     RS           Distance from the galactic centre of the arms' start (kpc)
!     ISP          Pitch angle of the spiral arms (degrees)
!     OMEGA_SP     Pattern speed of the spiral arms (km/sec/kpc)
!     PHI0_SP      Initial phase of the spiral pattern (degrees)
!     LSP          Structural parameters defining the extension of the spiral arms (standard value, LSP = 100)
!     ZSP          Damping lenght of the spiral perturbation alonf the vertical direction (kpc)
!     ASP          Damping factor of the spirl perturbation in the inner regions of the galaxy (standard value, ASP = 100) 
!     DELTA_SP     Ratio between arm and intra-arm value of the gravitational potential                        
!
! Coded by L. J. Rossi (Melbourne, 2014).
!
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!
  USE PARAMETERS
!
  IMPLICIT NONE
!
  REAL (8) VB1_2,VB2_2,VD1_2,VD2_2,VD3_2, VAXI_2,DGAMMA,PSER,PSERH
!
  OPEN (UNIT = 2, FILE = "InputN.dat")
    READ(2,*)
    READ(2,*) N_STAR,T_START,T_STOP,DELTA_T,ABSERR
    READ(2,*)
    READ(2,*)
    READ(2,*) R0,V0
    READ(2,*)
    READ(2,*)
    READ(2,*) MBULGE1,BB1
    READ(2,*)
    READ(2,*)
    READ(2,*) MBULGE2,BB2
    READ(2,*)
    READ(2,*)
    READ(2,*) MSER,RE,NSER
    READ(2,*)
    READ(2,*)
    READ(2,*) MDISC1,AD1,BD1
    READ(2,*)
    READ(2,*)
    READ(2,*) MDISC2,AD2,BD2
    READ(2,*)
    READ(2,*)
    READ(2,*) MDISC3,AD3,BD3
    READ(2,*)
    READ(2,*)
    READ(2,*) MBAR1,ABAR1,BBAR1,CBAR1,NBAR1,OMEGA_B1,PHI0_B1
    READ(2,*)
    READ(2,*)
    READ(2,*) MBAR2,ABAR2,BBAR2,CBAR2,NBAR2,OMEGA_B2,PHI0_B2
    READ(2,*)
    READ(2,*)
    READ(2,*) HCHOICE,VHL,AHL,MHALO,AH,MSERH,REH,NSERH
    READ(2,*)
    READ(2,*)
    READ(2,*) NSP,RS,ISP,OMEGA_SP,PHI0_SP,LSP,ZSP,ASP,DELTA_SP
  CLOSE (UNIT = 2)
! 
  N_SNAP = (T_STOP-T_START)/DELTA_T
! Gravitational constant expressed in kpc^3/Mo/Myr^2
  CONST = 1.023D-03
  G = 4.302D-06*CONST**2.0
  PI = 4.0D0*ATAN(1.0D0)
!
  VHL = VHL*CONST
!  
! Conversion from degrees to radiants
  PHI0_B1 = PHI0_B1*PI/180.0
  PHI0_B2 = PHI0_B2*PI/180.0
  PHI0_SP = PHI0_SP*PI/180.0
  ISP = ISP*PI/180.0
!
! Conversion of the pattern speed from km/sec/kpc to 1/Myr
  OMEGA_B1 = OMEGA_B1*CONST
  OMEGA_B2 = OMEGA_B2*CONST
  OMEGA_SP = OMEGA_SP*CONST
!     
! Value of the functions defining the potential generated by a Sersic model 
  PSER = 1.0 - 0.6097/NSER + 0.05563/NSER**2.0
  BSER = 2.0*NSER - 1.0/3.0 + 0.009876/NSER
  AA = NSER*(3.0 - PSER)
  GG = DGAMMA(AA)
!
! Value of the functions defining the potential generated by a halo Sersic mass model 
  PSERH = 1.0 - 0.6097/NSERH + 0.05563/NSERH**2.0
  BSERH = 2.0*NSERH - 1.0/3.0 + 0.009876/NSERH
  AAH = NSERH*(3.0 - PSERH)
  GGH = DGAMMA(AAH)
!
END SUBROUTINE DEFINE
