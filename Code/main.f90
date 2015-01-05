PROGRAM MAIN
!
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!
!  Main program of NIGO (Numerical Intergration of Galactic Orbits).
!
!  Coded by L. J. Rossi (Melbourne, 2014). 
!
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!
  USE PARAMETERS
!
  IMPLICIT NONE
!
  EXTERNAL TIMESTAMP
  EXTERNAL NIGO
  EXTERNAL DEFINE
! 
  CALL TIMESTAMP ( )
  CALL DEFINE() 
  CALL NIGO ()
!
  WRITE ( *, '(a)' ) ' '
  WRITE ( *, '(a)' ) '  GALACTIC GRAVITATIONAL POTENTIAL: NIGO INTEGRATION'
  WRITE ( *, '(a)' ) '  Normal end of execution. Output written on "Orbits.dat"'
  WRITE ( *, '(a)' ) ' ' 
!
  STOP
!
END PROGRAM MAIN
