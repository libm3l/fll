
!     Copyright (C) 2018  Adam Jirasek
! 
!     This program is free software: you can redistribute it and/or modify
!     it under the terms of the GNU Lesser General Public License as published by
!     the Free Software Foundation, either version 3 of the License, or
!     (at your option) any later version.
! 
!     This program is distributed in the hope that it will be useful,
!     but WITHOUT ANY WARRANTY; without even the implied warranty of
!     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!     GNU Lesser General Public License for more details.
! 
!     You should have received a copy of the GNU Lesser General Public License
!     along with this program.  If not, see <http://www.gnu.org/licenses/>.
!     
!     contact: libm3l@gmail.com
! 
!

!
!     Sample program
!
!     Date: 2018-11-07
! 
! 
!
!
!     Description: fast array operations
!
!
!     Input parameters:
! 
!
!     Return value:
! 
! 
!
!     Modifications:
!     Date		Version		Patch number		CLA 
!
!
!     Description
!
! sort routine programmed according to:Juli Rew, SCD Consulting (juliana@ucar.edu), 9/03
! Based on algorithm from Cormen et al., Introduction to Algorithms,
! 1997 printing
!

module fast_arr_ops_m
  
  use fll_mods_m

  implicit none

  contains

  recursive subroutine quicksort(A)

    implicit none
    integer(lint), intent(inout) :: A(:)
    integer(lint) :: ind

    if(size(A) > 1) then
       call partition(A, ind)
       call quicksort(A(:ind-1))
       call quicksort(A(ind:))
    endif
  end subroutine quicksort

  subroutine partition(A, ind)

    implicit none
    integer(lint), intent(inout) :: A(:)
    integer(lint), intent(out) :: ind
    integer(lint) :: i, j
    integer(lint) :: temp
    integer(lint) :: x

    x = A(1)
    i = 0
    j = size(A, dim=1, kind = lint) + 1

    do
      j = j-1
      do
        if (A(j) <= x) exit
        j = j-1
      end do
      i = i+1
      do
        if (A(i) >= x) exit
        i = i+1
      end do
      if (i < j) then
        temp = A(i)
        A(i) = A(j)
        A(j) = temp
      elseif (i == j) then
        ind = i+1
        return
      else
        ind = i
        return
      endif
    end do

  end subroutine partition
  
!
!  heap sort algorithm with bug fixe, according to Messiner, L. P: Fortran 90 & 95
!  Array and Pointer Techniques, Computer Science Department, University of 
!  San Francisc
!
  subroutine heapsort(array)
  implicit none
  
  integer(lint), intent(inout) :: array(:)
  integer(lint) :: i,j,n
  integer(lint) :: next
  
  n = size(array, dim = 1, kind = lint)
  
  do j=n/2,1,-1
    next = array(j)
    i = 2*j
    do while(i<=n)
      if(i<n)then
        if(array(i) < array(i+1)) i = i+1
      end if
      if(array(i) <= next)exit
      array(i/2) = array(i)
      i = 2*i
    end do
    array(i/2) = next
  end do
  
  do j = n,2,-1
    next     = array(j)
    array(j) = array(1)
    array(1) = next

    i = 2
    do while( i <= j-1)
      if(i<j-1)then
        if(array(i) < array(i+1)) i = i+1
      end if
      if(array(i) <= next)exit
      array(i/2) = array(i)
      i = 2*i
    end do
    array(i/2) = next
  end do
  
  return 
  
  end subroutine heapsort



  function arrindex(array,value) result(index)
!
!  function gives index of value in array
!  binary search algorithm
!
    implicit none
!
! input/output parameters
!
    integer(lint), intent(in)   :: array(:)
    integer(lint), intent(in)   :: value
    integer(lint) :: index
!
! local parameters:
!
    integer(lint) indexf,indexl
    integer(lint) valuetmp
!
!  set lower and upper bound 
!
    indexf = 1
    indexl = size(array,dim = 1,kind = lint)
!
!  as long as indexf is less then indexl, loop
!
    do while(indexf .le. indexl)
      index = (indexf+indexl)/2
      valuetmp = array(index)
!
!  get value in index, if smaller increment lower bound otherwise
!  decrement upper bound
!
      if(valuetmp .gt. value) then
        indexl=index-1
      else if(valuetmp .lt. value) then
        indexf =index+1
      else
!
!  index found
!
        return
      end if
    end do
!
!  index not found, give index -1
!
    index = -1
    return

  end function arrindex
  
  
   subroutine unique(iinodes,iuniquenodes,k)
!
!  requires sorted array, then just simply compares following values
!  https://stackoverflow.com/questions/3350641/array-remove-duplicate-elements
!
    use fll_mods_m
    implicit none
!
! input/output parameters
!
    integer(lint), intent(in) :: iinodes(:)
    integer(lint), intent(out) :: iuniquenodes(:)
    integer(lint) :: k
!
!  lcoal parameters
!
   integer(lint) :: i,nunique,n,ind,lastind
   iuniquenodes = iinodes

   k  = 1
   lastind = iuniquenodes(1)

   do n=2,size(iuniquenodes, dim=1, kind = lint)
      ind = iuniquenodes(n)
      if(ind == lastind) cycle
      k = k+1
      iuniquenodes(k)= ind
      lastind  = ind
   end do
    
 end subroutine unique
 

end module fast_arr_ops_m


