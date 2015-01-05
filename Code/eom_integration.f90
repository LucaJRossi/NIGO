SUBROUTINE EOM_INTEGRATION (T_SNAP,NEQN,Y_START,TOTAL_SOL_MATRIX,I_STAR)
!
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!
! Integration of the equations of motion using the Shampine-Gordon integrator scheme.
!
! Coded by L. J. Rossi (Melbourne, 2014)
!
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!
  USE PARAMETERS
!
  IMPLICIT NONE
!
  INTEGER (4) IFLAG,I_STEP,NEQN,I,I_STAR
  REAL (8) RELERR,T,T_OUT,T_STEP,T_RUN,T_STEP_RUN
  REAL (8) WORK(100+21*NEQN)
  INTEGER (4) IWORK(5)
  REAL (8) Y(NEQN)
  REAL (8) Y_START(NEQN)
  REAL (8) SOL_MATRIX((N_SNAP+1),(NEQN+1)) 
  REAL (8), DIMENSION((N_SNAP+1)*N_STAR, NEQN+1), INTENT(OUT) :: TOTAL_SOL_MATRIX
  REAL (8) T_SNAP(N_SNAP+1)
!
  EXTERNAL EOM
!
  RELERR = ABSERR
!
! Initialization of the integration variables
  Y = Y_START
  T = T_START
!
! First row of the solution matrix
  SOL_MATRIX(1,:) = (/T,Y/)
  SOL_MATRIX(1,5) = SOL_MATRIX(1,5)/CONST
  SOL_MATRIX(1,6) = SOL_MATRIX(1,6)/CONST
  SOL_MATRIX(1,7) = SOL_MATRIX(1,7)/CONST
!  
  I = 2
!
  DO WHILE (T < T_STOP) 
     T_STEP = SQRT((Y(1)**2.0 + Y(2)**2.0 + Y(3)**2.0)/(Y(4)**2.0 + Y(5)**2.0 + Y(6)**2.0))/10.0
     IFLAG = 1
     T_RUN = T
     IF (T_SNAP(I) < T + T_STEP) THEN
        T_STEP = T_SNAP(I) - T
     END IF
!    
     T_STEP_RUN = T_STEP
     T_OUT = T + T_STEP
!
     CALL ODE (EOM,NEQN,Y,T,T_OUT,RELERR,ABSERR,IFLAG,WORK,IWORK)
!
     DO WHILE (IFLAG /= 2)
        IFLAG = 1
        T = T_RUN 
        T_STEP_RUN = T_STEP_RUN/2.0D0
        T_OUT = T + T_STEP_RUN
        CALL ODE (EOM,NEQN,Y,T,T_OUT,RELERR,ABSERR,IFLAG,WORK,IWORK)
     END DO
!
     IF (T == T_SNAP(I)) THEN
        SOL_MATRIX(I,:) = (/T,Y/)
        SOL_MATRIX(I,5) = SOL_MATRIX(I,5)/CONST
        SOL_MATRIX(I,6) = SOL_MATRIX(I,6)/CONST
        SOL_MATRIX(I,7) = SOL_MATRIX(I,7)/CONST
        I = I + 1
     END IF
!
  END DO
!
  DO I = 1,N_SNAP+1
        TOTAL_SOL_MATRIX (I_STAR + N_STAR*(I-1),:) = SOL_MATRIX(I,:)
  END DO
!    
  WRITE(*,*) " N_star: ", I_STAR
!
END SUBROUTINE EOM_INTEGRATION
