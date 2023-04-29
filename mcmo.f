       SUBROUTINE Inse()
        IMPLICIT NONE

        INCLUDE 'include/para.f'
        INCLUDE 'include/comm.f'

        DOUBLE PRECISION dpot, divi ! potential energy and ivir
        DOUBLE PRECISION prob       ! probability of suppression
        DOUBLE PRECISION Ranf       ! for the Random

        atnb = atnb+1
        DO k = 1, 3
            atpo(atnb,k) = Ranf()*boxl ! create an atom
        ENDDO

        dpot = 0.d0
        divi = 0.d0
        CALL EVDi(atnb, dpot, divi)

        prob = (volu*fuga)/(bolt*temp*dfloat(atnb))*fac1
     .         *EXP(-dpot/(bolt*temp)*fac2)

        IF (Ranf() .GE. prob) THEN ! reject
         DO k = 1, 3
          atpo(atnb,k) = bign
         ENDDO
         atnb = atnb-1
        ELSE ! accept
         epot = epot+dpot
         ivir = ivir+divi
        ENDIF
       END

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

       SUBROUTINE Supp()
        IMPLICIT NONE

        INCLUDE 'include/para.f'
        INCLUDE 'include/comm.f'

        INTEGER atom                ! ID of the atom that will be deleted
        DOUBLE PRECISION dpot, divi ! potential energy and ivir
        DOUBLE PRECISION prob       ! probability of suppression
        DOUBLE PRECISION Ranf       ! for the Random

        atom = int(Ranf()*dfloat(atnb))+1
        dpot = 0.d0
        divi = 0.d0
        CALL EVDi(atom, dpot, divi)

        prob = bolt*temp*dfloat(atnb)/fac1/(volu*fuga)
     .         *EXP(dpot/(bolt*temp)*fac2)

        IF (Ranf() .LT. prob) THEN ! accept
         DO k = 1, 3
          atpo(atom,k) = atpo(atnb,k) ! move last atom to this atom
          atpo(atnb,k) = bign         ! delete last atom
         ENDDO
         atnb = atnb-1
         epot = epot-dpot
         ivir = ivir-divi
        ENDIF
       END
