      program PairingModel
c
      integer i,j,a,b,N
      real*8 Phi(8),Ecor,g
      STRUCTURE /states/ integer*4 p,s
      COMMON /states/ sy(8)
c
      sy(1).p = 1
      sy(1).s = 1
      sy(2).p = 1
      sy(2).s = 0
      sy(3).p = 2
      sy(3).s = 1
      sy(4).p = 2
      sy(4).s = 0
      sy(5).p = 3
      sy(5).s = 1
      sy(6).p = 3
      sy(6).s = 0
      sy(7).p = 4
      sy(7).s = 1
      sy(8).p = 4
      sy(8).s = 0
c
      g=0.5
      Ecor=0
      do i=1,4
         do j=1,4
            do a=5,8
               do b=5,8
                  Ecor = Ecor + 0.25*g**2*corr(a,b,i,j)*corr(i,j,a,b)
     +                     /Espec(a,b,i,j)
               end do
            end do
      end do
      end program PairingModel
c
      real*8 function corr(a,b,i,j)
      corr = 0
      if (sy(a).p .ne. sy(b).p || sy(i).p .ne. sy(j).p) then corr=0
      if (sy(i).s .eq. sy(j).s || sy(a).s .eq. sy(b).s) then corr=0
      if (sy(a).s .eq. sy(i).s) then corr=-1/2.
      if (sy(a).s .ne. sy(j).s) then corr=1/2.
      end function

      real*8 function Espec(a,b,i,j)
      Espec = Espec - sy(a).p+1 -sy(b).p+1 + sy(i).p-1 + sy(j).p-1
      end function
      
