 SUBROUTINE FIND_LAMBDA(A,B,C,X,Y,Z,L,TOL)
!
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!
! Solution of m^2(lambda)=1 with the Newton-Raphson Algorithm.
! The solution is the positive zero of the function f(l)=A*l^3+Al^2+Bl+C.
!
! Coded by L.J. Rossi (Melbourne, 2014).
!
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!
  IMPLICIT NONE
!
  REAL (8) A,B,C,X,Y,Z
  REAL (8) L,A1,B1,C1,D1,F,DF,L0,L1,A2,B2,C2,X2,Y2,Z2
  INTEGER (4) I, N_MAX
  REAL (8) TOL 
!
  I = 0
  N_MAX = 1.0D+06
!
  A2 = A**2.0
  B2 = B**2.0
  C2 = C**2.0
!
  X2 = X**2.0
  Y2 = Y**2.0
  Z2 = Z**2.0
!
  A1 = 1.0
  B1 = A2 + B2 + C2 - X2 - Y2 - Z2
  C1 = A2*B2 + A2*C2 + B2*C2 - X2*(B2 + C2) - Y2*(A2 + C2) - Z2*(A2 + B2)
  D1 = A2*B2*C2 - X2*B2*C2 - A2*Y2*C2 - A2*B2*Z2
!
! Initial value of the problem
  L0 = 1.0D+03
  F = A1*L0**3.0 + B1*L0**2.0 + C1*L0 + D1
!
  DO WHILE (F <= 0.0) 
    L0 = L0 + 1D+03
    F = A1*L0**3.0 + B1*L0**2.0 + C1*L0 + D1
  END DO
!
! First derivative
  DF = 3*A1*L0**2.0 + 2*B1*L0 + C1
  L1 = L0 - F/DF
!
! Do while loop to find the positive root
  DO WHILE (I<=N_MAX .AND. ABS(L1-L0)>=TOL)
!
     L0 = L1
     F = A1*L0**3.0 + B1*L0**2.0 + C1*L0 + D1
     DF = 3*A1*L0**2.0 + 2*B1*L0 + C1
     L1 = L0 - F/DF
     I = I + 1
!
  END DO
!
  L = L1
!
END SUBROUTINE FIND_LAMBDA
