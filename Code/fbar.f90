SUBROUTINE FBAR (X,Y,Z,AX,AY,AZ,G,MBAR,ABAR,BBAR,CBAR,NBAR,TOL)
!
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!
! Compute the acceleration of a particle moving within the bar potential 
! expressed in the bar corotating frame of reference.
!
! Coded by L. J. Rossi (Melbourne, 2014)
!
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!
  IMPLICIT NONE
!
  REAL (8) X,Y,Z,AX,AY,AZ,G,MBAR,ABAR,BBAR,CBAR,NBAR,CMBAR,X2,Y2,Z2,TOL
  REAL (8) W000,W100,W010,W001,W110,W011,W101,W200,W020,W002,&
           W111,W120,W012,W201,W210,W021,W102,W300,W030,W003
!
  EXTERNAL BAR_COEFFICIENTS
!
  CALL BAR_COEFFICIENTS (X,Y,Z,ABAR,BBAR,CBAR,W000,W100,W010,W001,W110,W011,W101,W200,W020,W002,&
                         W111,W120,W012,W201,W210,W021,W102,W300,W030,W003,TOL)
!  
  X2 = X**2.0
  Y2 = Y**2.0
  Z2 = Z**2.0
!
! Compute the accelerations
  IF (NBAR == 0.0D0) THEN
!    
     CMBAR = -G*MBAR*3.0/4.0
     AX = 2.0*CMBAR*X*W100
     AY = 2.0*CMBAR*Y*W010
     AZ = 2.0*CMBAR*Z*W001
!
  ELSE IF (NBAR == 1.0D0) THEN
!
     CMBAR = -G*MBAR*1.875
     AX = 2.0*CMBAR*X*(W100 - Y2*W110 - Z2*W101 - X2*W200)
     AY = 2.0*CMBAR*Y*(W010 - X2*W110 - Z2*W011 - Y2*W020)
     AZ = 2.0*CMBAR*Z*(W001 - Y2*W011 - X2*W101 - Z2*W002)
!
  ELSE IF (NBAR == 2.0D0) THEN
!
     CMBAR = -G*MBAR*3.2812
     AX = 2.0*CMBAR*X*(W100-2.0*Y2*W110-2.0*Z2*W101-2.0*X2*W200+2.0*Y2*Z2*W111+2.0*X2*Y2*W210+2.0*X2*Z2*W201& 
          +Y**4.0*W120+Z**4.0*W102+X**4.0*W300)
     AY = 2.0*CMBAR*Y*(W010-2.0*X2*W110-2.0*Z2*W011-2.0*Y2*W020+2.0*X2*Z2*W111+X**4.0*W210+2.0*Y2*Z2*W021& 
          +2.0*X2*Y2*W120+Z**4.0*W012+Y**4.0*W030)
     AZ = 2.0*CMBAR*Z*(W001-2.0*X2*W101-2.0*Y2*W011-2.0*Z2*W002+2.0*X2*Y2*W111+X**4.0*W201+Y**4.0*W021& 
          +2.0*Y2*Z2*W012+2.0*X2*Z2*W102+Z**4.0*W003)
!
  END IF  
!
END SUBROUTINE FBAR
