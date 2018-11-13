!
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
!
module ffa2fll_m

contains

  subroutine ffa2fll(pglob)
!
!  reads ugrid file
!
    use fll_mods_m
    
    implicit none
! 
! input/output data
!
    type(dnode), pointer :: pglob
!
! local paramaters
!
    type(dnode), pointer :: pgrid,pbound,pelem
    type(func_data_set) :: fpar

    integer(lint) :: igrid, ngrid,ibound, nbound, ielem,nelem
!
!   loop over grids
!
    pglob%ltype='DIR'
    ngrid = fll_nnodes(pglob,'region','*',-1_lint,.false.,fpar)
!
!  loop over grids
!
    loop_grid: do igrid = 1,ngrid 

      pgrid => fll_locate(pglob,'region','*',-1_lint,igrid,.false.,fpar,errmsg='ALL')
      pgrid%lname='grid'
      pgrid%ltype='DIR'

      nbound = fll_nnodes(pgrid,'boundary','*',-1_lint,.false.,fpar)

      do ibound = 1,nbound

        pbound => fll_locate(pgrid,'boundary','*',-1_lint,ibound,.false.,fpar,errmsg='ALL')
        pbound%ltype='DIR'
        nelem = fll_nnodes(pbound,'belem_group','*',-1_lint,.false.,fpar)
        do ielem = 1,nelem
          pelem => fll_locate(pbound,'belem_group','*',-1_lint,ielem,.false.,fpar,errmsg='ALL')
          pelem%ltype='DIR'
          pelem%lname = 'bound_elem_group'
        end do

      end do

      nbound = fll_nnodes(pgrid,'element_group','*',-1_lint,.false.,fpar)
      do ibound = 1,nbound
        pelem => fll_locate(pgrid,'element_group','*',-1_lint,ibound,.false.,fpar,errmsg='ALL')
        pelem%ltype='DIR'
      end do

    end do loop_grid

   
end subroutine ffa2fll
   

end module ffa2fll_m
