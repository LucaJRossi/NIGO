SUBROUTINE FSPIRAL (X,Y,Z,AX,AY,AZ,NSP,RS,ISP,LSP,ZSP,ASP,DELTA_SP,G,MDISC,AD,BD)
!
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!
! Compute the accelerations due to a spiral perturbation of the background axisymmetric 
! disc in the frame of reference corotating with the spiral pattern.
!
! Coded by L.J. Rossi (Melbourne, 2014).
!
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!
  IMPLICIT NONE
!
  REAL (8) X,Y,Z,AX,AY,AZ,NSP,RS,ISP,LSP,ZSP,ASP,DELTA_SP,G,MDISC,AD,BD
  REAL (8) R,CMD
  REAL (8) PHI_D,DPHID_DX,DPHID_DY,DPHID_DZ
  REAL (8) H,DH_DX,DH_DY
  REAL (8) PHI,DPHI_DX,DPHI_DY
  REAL (8) S,DS_DX,DS_DY,DS_DZ
  REAL (8) Q,DQ_DX,DQ_DY
  REAL (8) F,DF_DX,DF_DY,DF_DZ,SECH2
!
  R = SQRT(X**2.0 + Y**2.0)
!
  CMD = MDISC/(R**2.0 + (AD + SQRT(Z**2.0 + BD**2.0))**2.0)**(1.5)
!
  PHI_D = - MDISC/SQRT(R**2.0 + (AD + SQRT(Z**2.0 + BD**2.0))**2.0)
!
  DPHID_DX = CMD*X
  DPHID_DY = CMD*Y
  DPHID_DZ = CMD*Z*(AD + SQRT(Z**2.0 + BD**2.0))/SQRT(Z**2.0 + BD**2.0)
!
  H = LOG(1 + (R/RS)**LSP)/(LSP*TAN(ISP))
  DH_DX = (R**(LSP - 2.0)/(RS**LSP*TAN(ISP)*(1 + (R/RS)**LSP)))*X
  DH_DY = (R**(LSP - 2.0)/(RS**LSP*TAN(ISP)*(1 + (R/RS)**LSP)))*Y
!
  PHI = ATAN2(Y,X)
  DPHI_DX = -Y/R**2.0
  DPHI_DY = X/R**2.0
!
  Q = COS(NSP*(PHI - H))
  DQ_DX = -SIN(NSP*(PHI - H))*(NSP*(DPHI_DX - DH_DX))
  DQ_DY = -SIN(NSP*(PHI - H))*(NSP*(DPHI_DY - DH_DY))
 !
  SECH2 = 1 - (TANH(Z/ZSP))**2.0
!
  IF (R >= RS) THEN   
     S = SECH2
     DS_DX = 0.0D0
     DS_DY = 0.0D0
     DS_DZ = -2*S/ZSP*TANH(Z/ZSP)
  ELSE
     S = SECH2*EXP(-ASP*(R - RS)**2.0)
     DS_DX = S*(-2.0*ASP*X*(R - RS)/R)
     DS_DY = S*(-2.0*ASP*Y*(R - RS)/R)
     DS_DZ = -2*S/ZSP*TANH(Z/ZSP)    
  END IF
!
  F = 1.0 + DELTA_SP*Q*S
  DF_DX = DELTA_SP*(DQ_DX*S + Q*DS_DX)
  DF_DY = DELTA_SP*(DQ_DY*S + Q*DS_DY)
  DF_DZ = DELTA_SP*Q*DS_DZ
!
! Compute the accelerations  
  AX = -G*(DPHID_DX*F + PHI_D*DF_DX)
  AY = -G*(DPHID_DY*F + PHI_D*DF_DY)
  AZ = -G*(DPHID_DZ*F + PHI_D*DF_DZ)
!
END SUBROUTINE FSPIRAL
