*******************************************************************
*
*                            Pairing Model
*
* This progam creates the ground state ocupation given the number
* of particles in the system, N, and the number of available 
* states, Nst. The system is sets up to populate the available 
* states with particles from the bottom up. The particle hole
* formalism is used where N is the number of hole states and 
* Nst-N are the number of particle states using standard second 
* quantization...blah..blah...blaaahhhhhh...
*
* (7,July 2015)
*******************************************************************
c
      program PairingModel
c
      integer N,nelvl,ns,Nst
      data N,nelvl,ns,Nst /4,4,2,8/
      integer Pstates,spst(4,2),elvl(4,2)
      real*8 g
      common /state/ N,ns,Nst,spst,elvl
c 
      Pstates = Nst-N    ! to be used in the generalized form
c
      do i=1,N
         do j=1,ns
            spst(i,j)=(-1)**(j+1)
            elvl(i,j)=i
c            print *, spst(i,j),elvl(i,j)
         end do
      end do
*************************************************
c This part is not necessary
c 
c      do i=Pstates,Nst
c         do j=1,ns
c            spst(i,j)=j-1
c            elvl(i,j)=i-Pstates+1
c            print *,spst(i,j),elvl(i,j)
c         end do
c      end do
*************************************************
c
      g=-0.5
      Ecor=0.
      do k=1,4
         do l=1,4
            do i=1,4
               do j=1,4
                  do m=1,2
                     do mm=1,2
                        call corr(k,l,i,j,m,mm,corr1)
c     print *, 'corr1 = ',corr1
                        call corr(i,j,k,l,m,mm,corr2)
                        Ecor = Ecor + (0.25*g**2*corr1*corr2
     +                       /Espec(k,l,i,j))
c                        print *, 'corr1 = ',corr1,'corr2 = ',corr2
                     end do
                  end do
               end do
            end do
         end do
      end do
c
      
c
      print *, 'Correlation energy = ', Ecor
c
      end program PairingModel
c
      subroutine corr(k,l,i,j,m,mm,cout)
      integer N,nelvl,ns,Nst
      data N,nelvl,ns,Nst /4,4,2,8/
      integer Pstates,spst(4,2),elvl(4,2)
      common /state/ N,ns,Nst,spst,elvl
c
      if(elvl(k,m).ne.elvl(l,mm).or.elvl(i,m).ne.elvl(j,mm)) then
         cout=0.
c         print *, 'in if 1'
c         return
      else if(spst(i,m).eq.spst(j,mm).or.spst(k,m).eq.spst(l,mm)) then
         cout=0.
c         print *, 'in if 2'
c         return
      else if(spst(i,m).eq.spst(l,mm).and.spst(j,m).eq.spst(k,mm)) then
         cout=-0.5
         print *, 'in if 3'
         return
      else if(spst(k,m).eq.spst(i,mm).and.spst(l,m).eq.spst(i,mm)) then 
         cout=0.5
         print *, 'in if 4'
         return
      end if
      end subroutine

      real function Espec(k,l,i,j)
      integer N,nelvl,ns,Nst
      data N,nelvl,ns,Nst /4,4,2,8/
      integer Pstates,spst(4,2),elvl(4,2)
      common /state/ N,ns,Nst,spst,elvl
      if (k.eq.3 .or. k.eq.4 .or. l.eq.3 .or. l.eq.4) then
         Espec = Espec-elvl(k,1)+1-elvl(l,1)+1+elvl(i,1)-1+elvl(j,1)-1
      end if
c      else if( Espec = 1 ! finish this for the reverse case 
      end function
      
