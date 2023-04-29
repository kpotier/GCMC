C      variables input.in
       LOGICAL rest          ! restart
       DOUBLE PRECISION temp ! temperature
       DOUBLE PRECISION fuga ! fugacity
       DOUBLE PRECISION boxl ! box length
       DOUBLE PRECISION volu ! volume
       DOUBLE PRECISION cuto ! cutoff SQUARED
       DOUBLE PRECISION epsi ! epsilon
       DOUBLE PRECISION sigm ! sigma
       INTEGER cycl          ! number of cycles
       INTEGER step          ! number of steps
       INTEGER seed         ! random number seed
       COMMON/login/ rest
       COMMON/realin/ temp,fuga,boxl,volu,cuto,epsi,sigm
       COMMON/intin/ cycl,step,seed

C      variables data
       INTEGER atnb                  ! number of atoms
       DOUBLE PRECISION epot         ! potential energy
       DOUBLE PRECISION ivir         ! internal virial
       DOUBLE PRECISION atpo(atma,3) ! position of atoms
       COMMON/intda/ atnb
       COMMON/realda/ epot, atpo, ivir