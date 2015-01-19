SUBROUTINE EOM (T,X,XP)

!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!
!  Implement the system of differential equations that describe the motion of a particle
!  moving within the galactic gravitational potential.
!  
!  Coded by L. J. Rossi (Melbourne, 2014).
!
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!
  USE PARAMETERS
!
  IMPLICIT NONE
!
  REAL (8) X(6),XP(6),T,AX,AY,AZ,XS,YS,XB,YB,AXR,AYR,SINTHETA_S,COSTHETA_S,&
           SINTHETA_B,COSTHETA_B
!
  EXTERNAL FBULGE,FBAR,FDISC,FSPIRAL,FHALO
! 
  XP(1) = X(4)
  XP(2) = X(5)
  XP(3) = X(6) 
  XP(4) = 0.0D0
  XP(5) = 0.0D0
  XP(6) = 0.0D0
!
! Contribution of the Plummer sphere bulge 1
  IF (MBULGE1 /= 0.0D0) THEN
      CALL FBULGE(X(1),X(2),X(3),AX,AY,AZ,G,MBULGE1,BB1)
      XP(4) = XP(4) + AX
      XP(5) = XP(5) + AY
      XP(6) = XP(6) + AZ
  END IF
!
! Contribution of the Plummer sphere bulge 2
  IF (MBULGE2 /= 0.0D0) THEN
      CALL FBULGE(X(1),X(2),X(3),AX,AY,AZ,G,MBULGE2,BB2)
      XP(4) = XP(4) + AX
      XP(5) = XP(5) + AY
      XP(6) = XP(6) + AZ
  END IF
!
! Contribution of the Ferrer's bar 1
  IF (MBAR1 /= 0.0D0) THEN
     SINTHETA_B = SIN(PHI0_B1 + OMEGA_B1*T)
     COSTHETA_B = COS(PHI0_B1 + OMEGA_B1*T)
     XB = COSTHETA_B*X(1) - SINTHETA_B*X(2)
     YB = SINTHETA_B*X(1) + COSTHETA_B*X(2)
     CALL FBAR (XB,YB,X(3),AXR,AYR,AZ,G,MBAR1,ABAR1,BBAR1,CBAR1,NBAR1,ABSERR)
     AX = COSTHETA_B*AXR + SINTHETA_B*AYR
     AY = -SINTHETA_B*AXR + COSTHETA_B*AYR
     XP(4) = XP(4) + AX
     XP(5) = XP(5) + AY
     XP(6) = XP(6) + AZ
  END IF
!
! Contribution of the Ferrer's bar 2
  IF (MBAR2 /= 0.0D0) THEN
     SINTHETA_B = SIN(PHI0_B2 + OMEGA_B2*T)
     COSTHETA_B = COS(PHI0_B2 + OMEGA_B2*T)
     XB = COSTHETA_B*X(1) - SINTHETA_B*X(2)
     YB = SINTHETA_B*X(1) + COSTHETA_B*X(2)
     CALL FBAR (XB,YB,X(3),AXR,AYR,AZ,G,MBAR2,ABAR2,BBAR2,CBAR2,NBAR2,ABSERR)
     AX = COSTHETA_B*AXR + SINTHETA_B*AYR
     AY = -SINTHETA_B*AXR + COSTHETA_B*AYR
     XP(4) = XP(4) + AX
     XP(5) = XP(5) + AY
     XP(6) = XP(6) + AZ
  END IF
!
! Contribution of the Sèrsic component
  IF (MSER /= 0.0D0) THEN
      CALL FSERSIC(X(1),X(2),X(3),AX,AY,AZ,G,MSER,RE,NSER,GG,BSER,AA)
      XP(4) = XP(4) + AX
      XP(5) = XP(5) + AY
      XP(6) = XP(6) + AZ
  END IF
!
! Transformation of the coordinates in the spiral arms corotating frame of reference
  SINTHETA_S = SIN(PHI0_SP + OMEGA_SP*T)
  COSTHETA_S = COS(PHI0_SP + OMEGA_SP*T)
  XS = COSTHETA_S*X(1) - SINTHETA_S*X(2)
  YS = SINTHETA_S*X(1) + COSTHETA_S*X(2)
!
! Contribution of the Miyamoto-Nagai disc 1
  IF (MDISC1 /= 0.0D0) THEN
     IF (DELTA_SP > 0.0D0) THEN
        CALL FSPIRAL(XS,YS,X(3),AXR,AYR,AZ,NSP,RS,ISP,LSP,ZSP,ASP,DELTA_SP,G,MDISC1,AD1,BD1)
        AX = COSTHETA_S*AXR + SINTHETA_S*AYR
        AY = -SINTHETA_S*AXR + COSTHETA_S*AYR
        XP(4) = XP(4) + AX
        XP(5) = XP(5) + AY
        XP(6) = XP(6) + AZ
     ELSE
        CALL FDISC(X(1),X(2),X(3),AX,AY,AZ,G,MDISC1,AD1,BD1)
        XP(4) = XP(4) + AX
        XP(5) = XP(5) + AY
        XP(6) = XP(6) + AZ
     END IF
  END IF
!
! Contribution of the Miyamoto-Nagai disc 2
  IF (MDISC2 /= 0.0D0) THEN
     IF (DELTA_SP > 0.0D0) THEN
        CALL FSPIRAL(XS,YS,X(3),AXR,AYR,AZ,NSP,RS,ISP,LSP,ZSP,ASP,DELTA_SP,G,MDISC2,AD2,BD2)
        AX = COSTHETA_S*AXR + SINTHETA_S*AYR
        AY = -SINTHETA_S*AXR + COSTHETA_S*AYR
        XP(4) = XP(4) + AX
        XP(5) = XP(5) + AY
        XP(6) = XP(6) + AZ
     ELSE
        CALL FDISC(X(1),X(2),X(3),AX,AY,AZ,G,MDISC2,AD2,BD2)
        XP(4) = XP(4) + AX
        XP(5) = XP(5) + AY
        XP(6) = XP(6) + AZ
     END IF
  END IF
!
! Contribution of the Miyamoto-Nagai disc 3
  IF (MDISC3 /= 0.0D0) THEN
     IF (DELTA_SP > 0.0D0) THEN
        CALL FSPIRAL(XS,YS,X(3),AXR,AYR,AZ,NSP,RS,ISP,LSP,ZSP,ASP,DELTA_SP,G,MDISC3,AD3,BD3)
        AX = COSTHETA_S*AXR + SINTHETA_S*AYR
        AY = -SINTHETA_S*AXR + COSTHETA_S*AYR
        XP(4) = XP(4) + AX
        XP(5) = XP(5) + AY
        XP(6) = XP(6) + AZ
     ELSE
        CALL FDISC(X(1),X(2),X(3),AX,AY,AZ,G,MDISC3,AD3,BD3)
        XP(4) = XP(4) + AX
        XP(5) = XP(5) + AY
        XP(6) = XP(6) + AZ
     END IF
  END IF
!
! Contribution of the dark matter halo
  IF (HCHOICE > 0.0D0) THEN
        CALL FHALO(X(1),X(2),X(3),AX,AY,AZ,G,MHALO,AH,VHL,AHL,MSERH,REH,NSERH,GGH,BSERH,AAH,HCHOICE)
        XP(4) = XP(4) + AX
        XP(5) = XP(5) + AY
        XP(6) = XP(6) + AZ  
  END IF
!
END SUBROUTINE EOM
