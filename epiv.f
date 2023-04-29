       SUBROUTINE EVTo(epo2, iva2) ! Epot and V total
        IMPLICIT NONE

        INCLUDE 'include/para.f'
        INCLUDE 'include/comm.f'

        DOUBLE PRECISION epo2, iva2, dpo2, div2
              
        DO i = 1, (atnb-1)
         DO j = (i+1), atnb
          CALL EpIv(i, j, dpo2, div2)
          epo2 = epo2+dpo2
          iva2 = iva2+div2
         ENDDO
        ENDDO
        epo2 = 4.d0*epsi*epo2
        iva2 = 24.d0*epsi*iva2
       END

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

       SUBROUTINE EpIv(i, j, dpot, divi) ! Epot and IV for two atoms
        IMPLICIT NONE

        INCLUDE 'include/para.f'
        INCLUDE 'include/comm.f'

        DOUBLE PRECISION tdis(3), dist, dpot, divi, tca1, tca2, tca3

        dist = 0.d0
        DO k = 1, 3
         tdis(k) = atpo(i,k)-atpo(j,k)
         tdis(k) = tdis(k)-boxl*ANINT(tdis(k)/boxl)
         dist = dist+tdis(k)**2
        ENDDO

        dpot = 0.d0
        IF (dist .LE. cuto) THEN
         tca1 = sigm/dist
         tca2 = tca1**6
         tca3 = -tca1**3
         dpot = tca2+tca3
         divi = 0.d0
         DO k = 1, 3
          divi = divi+(2.d0*tca2+tca3)/dist*tdis(k)**2
         ENDDO
        ELSE
         dpot = 0.d0
         divi = 0.d0
        ENDIF
       END

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

       SUBROUTINE EVDi(atom, dpot, divi) ! Epot and iv diff for 1 atom
        IMPLICIT NONE
      
        INCLUDE 'include/para.f'
        INCLUDE 'include/comm.f'
      
        INTEGER atom
        DOUBLE PRECISION dpot, divi, dpo2, div2
                    
        DO i = 1, atnb
         IF (i .NE. atom) THEN
          CALL EpIv(atom, i, dpo2, div2)
          dpot = dpot+dpo2
          divi = divi+div2
         ENDIF
        ENDDO
        dpot = 4.d0*epsi*dpot
        divi = 24.d0*epsi*divi
       END
      