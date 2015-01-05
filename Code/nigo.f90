SUBROUTINE NIGO ()
!
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!
! Read the initial conditions, set up the total solution matrix, call the orbit integrator
! and create the output files.
!
! Coded by L. J. Rossi (Melbourne, 2014). 
!
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!
  USE PARAMETERS
!
  IMPLICIT NONE
!  
  INTEGER (4), PARAMETER :: NEQN = 6 
  INTEGER (4) IERROR,I_STEP,I_STAR,I,IFLAG,OMP_GET_MAX_THREADS
  REAL (8) T_SNAP(N_SNAP+1)
  REAL (8), DIMENSION((N_SNAP+1)*N_STAR, NEQN+1):: TOTAL_SOL_MATRIX
  REAL (8) INITIAL_STATE_MATRIX(N_STAR, NEQN)
  REAL (8) PERCENT, SECONDS, WTIME
  LOGICAL :: ORBITS_EXISTS, LOG_EXISTS
!
  EXTERNAL EOM_INTEGRATION
!
  SECONDS = WTIME( )
  T_SNAP(1) = 0.0
!
  N_SNAP = T_STOP/DELTA_T
  DO I= 2, N_SNAP+1  
     T_SNAP(I) = DELTA_T*(I-1)
  END DO
!  
  IFLAG = 1
!
! Read the initial conditions from the input file. The positions are expressed in kpc, while the velocities 
! are transformed from km/sec to kpc/Myr
  OPEN (UNIT = 2, FILE = "InputS.dat")
!
  DO I_STAR = 1, N_STAR
     READ (2, *)  INITIAL_STATE_MATRIX(I_STAR,:)
     INITIAL_STATE_MATRIX(I_STAR,4) = INITIAL_STATE_MATRIX(I_STAR,4)*CONST
     INITIAL_STATE_MATRIX(I_STAR,5) = INITIAL_STATE_MATRIX(I_STAR,5)*CONST
     INITIAL_STATE_MATRIX(I_STAR,6) = INITIAL_STATE_MATRIX(I_STAR,6)*CONST
  END DO
! 
  CLOSE (UNIT = 2)
 !
  WRITE(6,"(A, I3)") " THREADS NUMBER: ", OMP_GET_MAX_THREADS()
  WRITE(*,*)"N_star :", N_STAR
!
  WRITE ( *, '(a)' ) ' '
!
! Parallel integration of the orbits of the stars
 !$OMP PARALLEL DO
  DO I_STAR = 1, N_STAR
!
     CALL EOM_INTEGRATION(T_SNAP, NEQN, INITIAL_STATE_MATRIX(I_STAR,:), &
         TOTAL_SOL_MATRIX, I_STAR)
!
  END DO 
!
  WRITE ( *, '(a)' ) ' '
    SECONDS = WTIME ( ) - SECONDS;
  WRITE(*,*)'COMPUTATION TIME :', SECONDS, 'sec'
!
! Write the solution matrix to a file fter checking the existence of the file
  INQUIRE(FILE="Orbits.dat", EXIST=ORBITS_EXISTS)
  INQUIRE(FILE="logfile", EXIST=LOG_EXISTS)
!
  IF (ORBITS_EXISTS) THEN
     OPEN(UNIT=3, FILE="Orbits.dat", STATUS="REPLACE", ACTION="WRITE", IOSTAT=IERROR)
  ELSE
     OPEN(UNIT=3, FILE="Orbits.dat", STATUS="NEW", ACTION="WRITE", IOSTAT=IERROR)
  END IF

  DO I_STEP = 1, (N_SNAP+1)*N_STAR
     WRITE(UNIT=3, FMT=*) TOTAL_SOL_MATRIX(I_STEP,:)
  END DO
!
  CLOSE(UNIT=3)
!
  IF (LOG_EXISTS) THEN
     OPEN(UNIT=3, FILE="logfile", STATUS="REPLACE", ACTION="WRITE", IOSTAT=IERROR)
  ELSE
     OPEN(UNIT=3, FILE="logfile", STATUS="NEW", ACTION="WRITE", IOSTAT=IERROR)
  END IF
     
  WRITE(UNIT=3, FMT=*) '     N_star', '     N_threads', '     t_comp'
  WRITE(UNIT=3, FMT=*) N_STAR,  OMP_GET_MAX_THREADS(), SECONDS
!
  CLOSE(UNIT=3)
!
END SUBROUTINE NIGO
