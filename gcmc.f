       PROGRAM GCMC
        IMPLICIT NONE

        INCLUDE 'include/para.f'
        INCLUDE 'include/comm.f'

        DOUBLE PRECISION Ranf    ! for the Random
        INTEGER stim, etim, TIME ! time
        DOUBLE PRECISION pres, ivit

        stim = TIME()
        OPEN(22, FILE='log.gcmc')
        OPEN(21, FILE='traj.xyz')
        WRITE(22,*) 'i epot pressideal ivir press atnb'

        CALL Rinp()

        epot = 0.d0
        ivir = 0.d0

        IF (rest) THEN
         CALL Rdat()
         CALL EVTo(epot, ivir)
        ELSE 
         CALL Cdat()
        ENDIF

        DO i = 1, cycl
            DO j = 1, step
              IF (Ranf() .LE. 0.5d0) THEN ! Insertion or suppression
                CALL Inse()
              ELSEIF (atnb .GE. 1) THEN
                CALL Supp()
              ENDIF
            ENDDO
            pres = dfloat(atnb)*bolt*temp/volu*1d2
            ivit = ivir/3.d0/volu*1.602d6
            WRITE(22,*) i, epot, pres, ivit, pres+ivit
     .                  , atnb
            IF (MOD(i, 50) .EQ. 0.d0) THEN
             CALL Wxyz()
            ENDIF
        ENDDO

        etim = TIME()-stim
        WRITE(22,*) 'Time (s): ', etim
        WRITE(22,*) 'Time per step (s): ', dfloat(etim)/(cycl*step)
        CLOSE(22)
        CLOSE(21)
       END