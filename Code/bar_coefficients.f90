SUBROUTINE BAR_COEFFICIENTS(X,Y,Z,A,B,C,W000,W100,W010,W001,W110,W011,W101,&
           W200,W020,W002,W111,W120,W012,W201,W210,W021,W102,W300,W030,W003,TOL)
!
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!
! Compute the value of the coefficients determining the value of the force experienced 
! by a particle orbiting withing the potential generated by a triaxial Ferrer's ellipsoid.
!
! Coded by L. J. Rossi (Melbourne, 2014).
!
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!
  IMPLICIT NONE
!
  REAL (8) X,Y,Z,A,B,C,W000,W100,W010,W001,W110,W011,W101,W200,W020,W002,&
           W111,W120,W012,W201,W210,W021,W102,W300,W030,W003
  REAL (8) LAMBDA,DELTA,M,F,E,SIN2PHI,CSC2PHI,SINPHI,COS2PHI,K2,RF,RD,A2,B2,C2,X2,Y2,Z2,X1,Y1,Z1,TOL
  INTEGER (4) IER1,IER2
  REAL (8) DRF,DRD
! 
  A2=A**2.0
  B2=B**2.0
  C2=C**2.0
!
  X2=X**2.0
  Y2=Y**2.0
  Z2=Z**2.0
!
  M = SQRT(X2/A2 + Y2/B2 + Z2/C2)
!
! Value of lambda
  IF (M >= 1) THEN 
     CALL FIND_LAMBDA (A,B,C,X,Y,Z,LAMBDA,TOL)
  ELSE
     LAMBDA = 0.0D0
  END IF
!
  DELTA = SQRT((A2 + LAMBDA)*(B2 + LAMBDA)*(C2 + LAMBDA))
!
! Numerical values of the incomplete ellittic integrals of the first and the second kind F(phi,k) and E(phi,k)
  SIN2PHI = ((A2 - C2)/(A2 + LAMBDA))
  SINPHI = SQRT(SIN2PHI)
  COS2PHI = 1.0 - SIN2PHI
  CSC2PHI = 1.0/SIN2PHI
  K2 = (A2 - B2)/(A2 - C2)
!
! Carlson's symmetric forms (DRF and DRD are functions from the SLATEC library) 
  X1 = CSC2PHI - 1
  Y1 = CSC2PHI - K2
  Z1 = CSC2PHI
!
  RF = DRF(X1,Y1,Z1,IER1)
  RD = DRD(X1,Y1,Z1,IER2)
!
! Elliptic integrals of the first and the second type written in the Carlson's symmetric form
  F= RF
  E= RF - K2*RD/3.0
!
! Value of the Wijk coefficients 
  W000 = 2.0*F/SQRT(A2 - C2)
  W100 = 2.0*(F - E)/((A2 - B2)*SQRT(A2 - C2))
  W001 = 2.0/(B2 - C2)*SQRT((B2 + LAMBDA)/((A2 + LAMBDA)*(C2 + LAMBDA))) - 2.0*E/((B2 - C2)*SQRT(A2 - C2))
  W010 = 2.0/DELTA - W100 - W001 
  W110 = (W010 - W100)/(A2 - B2)
  W011 = (W001 - W010)/(B2 - C2)
  W101 = (W100 - W001)/(C2 - A2)
  W200 = (2.0/(DELTA*(A2 + LAMBDA)) - W110 - W101)/3.0
  W020 = (2.0/(DELTA*(B2 + LAMBDA)) - W110 - W011)/3.0
  W002 = (2.0/(DELTA*(C2 + LAMBDA)) - W101 - W011)/3.0
  W111 = (W011 - W101)/(A2 - B2)
  W120 = (W020 - W110)/(A2 - B2)
  W012 = (W002 - W011)/(B2 - C2)
  W201 = (W200 - W101)/(C2 - A2)
  W210 = (W110 - W200)/(A2 - B2)
  W021 = (W011 - W020)/(B2 - C2)
  W102 = (W101 - W002)/(C2 - A2)
  W300 = (2.0/(DELTA*(A2 + LAMBDA)**2.0) - W210 - W201)/5.0
  W030 = (2.0/(DELTA*(B2 + LAMBDA)**2.0) - W120 - W021)/5.0
  W003 = (2.0/(DELTA*(C2 + LAMBDA)**2.0) - W102 - W012)/5.0
!
END SUBROUTINE BAR_COEFFICIENTS
