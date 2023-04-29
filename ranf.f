       DOUBLE PRECISION FUNCTION Ranf()
        IMPLICIT NONE

        INCLUDE 'include/para.f'
        INCLUDE 'include/comm.f'

        INTEGER iia, ib, i1, i2, i3
        DOUBLE PRECISION scale
        PARAMETER (scale=0.99999999999999d0)

        iia = seed/32768
        ib = mod(seed,32768)
        i1 = IIA*30485
        i2 = IB*30485
        i3 = IB*48603
        i1 = MOD(I1,65536)
        i3 = MOD(I3,65536)
        i1 = I1+I3+13849+I2/32768+MOD(IIA,2)*32768
        i2 = MOD(I2,32768)+12659
        i1 = I1+I2/32768
        seed = MOD(i1,65536)*32768+MOD(i2,32768)
        Ranf = seed*4.65661287308E-10
        Ranf = Ranf*scale

        RETURN
      END