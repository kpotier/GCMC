       SUBROUTINE Rinp() ! Read input
        IMPLICIT NONE

        INCLUDE 'include/para.f'
        INCLUDE 'include/comm.f'

        OPEN(11,FILE='input.in')
        READ(11,*) rest
        READ(11,*) temp
        READ(11,*) fuga

        IF (.NOT. rest) THEN
         READ(11,*) boxl
         volu = boxl**3
        ELSE
         READ(11,*)
        ENDIF

        READ(11,*) cuto
        cuto = cuto**2

        READ(11,*) epsi
        READ(11,*) sigm
        sigm = sigm**2
        READ(11,*) cycl
        READ(11,*) step
        READ(11,*) seed
       END

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

       SUBROUTINE Rdat() ! Read data
        IMPLICIT NONE
       END

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

       SUBROUTINE Cdat() ! Create data
        IMPLICIT NONE

        INCLUDE 'include/para.f'
        INCLUDE 'include/comm.f'

        DO i = 1, atma
         DO k = 1, 3
          atpo(i,k) = bign
         ENDDO
        ENDDO

        atnb = 0
       END

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

       SUBROUTINE Wxyz() ! Write xyz
        IMPLICIT NONE
      
        INCLUDE 'include/para.f'
        INCLUDE 'include/comm.f'

        WRITE(21,*) atma
        WRITE(21,*) boxl, boxl, boxl

        DO i = 1, atma
         WRITE(21,*) "1 ", (atpo(i,k), k = 1, 3)
        ENDDO
       END