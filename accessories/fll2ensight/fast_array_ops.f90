
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
!     Description: prints file on screen
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

  recursive subroutine sort(A)

    implicit none
    integer(lint), intent(inout) :: A(:)
    integer(lint) :: ind

    if(size(A) > 1) then
       call partition(A, ind)
       call sort(A(:ind-1))
       call sort(A(ind:))
    endif
  end subroutine sort

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

end module fast_arr_ops_m


