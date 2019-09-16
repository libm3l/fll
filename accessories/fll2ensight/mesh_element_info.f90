!
!     Copyright (C) 2018  Adam Jirasek
! 
!     This program is free software: you can redistribute it and/or modify
!     it under the terms of the GNU Lesser General Public License as published by
!     the Free Software Foundation, either version 3 of the License, or
!     (at your option) any later version.
! 
!     This program is distributed in the hope that it will be useful,
!     but WITHOUT ANY WARRANTY; without even the implJd warranty of
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
!     Date: 2018-11-12
! 
! 
!
!
!     Description: export interfaces 
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
!
module mesh_element_info_m

contains

  subroutine mesh_element_info(iunit, nodenum,etype)
!
!     write elements into ensight file
!
!
!***********************************************************************
    use fll_mods_m
    implicit none

    integer(lint)        :: ndim1, ndim2
    integer(lint)        :: nodenum(:,:)
    integer                :: iunit
    character(len=*) :: etype
    integer(lint)        :: j, i
    character(80)     :: buf
    ndim1 = size(nodenum, dim = 1, kind = lint)
    ndim2 = size(nodenum, dim = 2, kind = lint)

    select case (etype)
!
!  make sure int numbers are in 4byte integer
!
    case('node')
      buf = 'point'
      write(iunit) buf
      write(iunit) int(ndim1, kind = sint)
      write(iunit) ((int(nodenum(j,i), kind = sint),i=1,ndim2),j=1,ndim1)

    case('bar2','bar')
      buf = 'bar2'
      write(iunit) buf
      write(iunit) int(ndim1, kind = sint)
      write(iunit) ((int(nodenum(j,i), kind = sint),i=1,ndim2),j=1,ndim1)

    case('tria3','tria')
      buf = 'tria3'
      write(iunit) buf
      write(iunit) int(ndim1, kind = sint)
      write(iunit) ((int(nodenum(j,i), kind = sint),i=1,ndim2),j=1,ndim1)

    case('quad4','quad')
      buf = 'quad4'
      write(iunit) buf
      write(iunit) int(ndim1, kind = sint)
      write(iunit) ((int(nodenum(j,i), kind = sint),i=1,ndim2),j=1,ndim1)

    case('tetra4','tetra')
      buf = 'tetra4'
      write(iunit) buf
      write(iunit) int(ndim1, kind = sint)
      write(iunit) ((int(nodenum(j,i), kind = sint),i=1,ndim2),j=1,ndim1)

    case('hexa8', 'hexa')
      buf = 'hexa8'
      write(iunit) buf
      write(iunit) int(ndim1, kind = sint)
      write(iunit) ((int(nodenum(j,i), kind = sint),i=1,ndim2),j=1,ndim1)

    case('penta6','penta')
      buf = 'penta6'
      write(iunit) buf
      write(iunit) int(ndim1, kind = sint)
      write(iunit) ((int(nodenum(j,i), kind = sint),i=1,ndim2),j=1,ndim1)

    case('penta5','pyra')
      buf = 'pyramid5'
      write(iunit) buf
      write(iunit) int(ndim1, kind = sint)
      write(iunit) ((int(nodenum(j,i), kind = sint),i=1,ndim2),j=1,ndim1)

    case default
      write(*,*) 'error: undefind lement type "',trim(etype),'" undefined'
      stop

    end select

    return

  end subroutine mesh_element_info
  
end module mesh_element_info_m
