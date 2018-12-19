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
module mesh_ffa2fll_m

CONTAINS

  subroutine mesh_ffa2fll(pglob)
!
!  converts file to ensight format
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
    type(dnode), pointer :: ptmp,ptmp1,pelem,pbound,pgrid
    TYPE(FUNC_DATA_SET) :: FPAR
    integer(lint) :: ndim, nsize, ibound, nbound, ielem, nelem,ngrid,igrid
    logical :: ok
!
!  do:
!  1. chage region to grid
!  2. chaneg belem_group to bound_elem_group
!  3. indexes from integer to long integer
!
    ngrid = fll_nnodes(pglob,'region','*',-1_lint,.false.,fpar)
    
    do igrid = 1, ngrid
      pgrid =>  fll_locate(pglob,'region','*',-1_lint,igrid,.false.,fpar,errmsg='ALL')
      pgrid%lname = 'grid'
      
      nbound = fll_nnodes(pgrid,'boundary','*',-1_lint,.false.,fpar)
      
      do ibound = 1,nbound
      
            pbound =>  fll_locate(pgrid,'boundary','*',-1_lint,ibound,.false.,fpar,errmsg='ALL')
            nelem = fll_nnodes(pbound,'belem_group','*',-1_lint,.false.,fpar)

            do ielem =1, nelem
!
!  always look for the 1st belem_group, it will be renamed 
!  and next search would not find it
!
               pelem =>  fll_locate(pbound,'belem_group','*',-1_lint,1_lint,.false.,fpar,errmsg='ALL')
               pelem%lname = 'bound_elem_group'
               
               ptmp => fll_locate(pelem,'bound_elem_nodes','*',-1_lint,1_lint,.false.,fpar,errmsg='ALL')
               ndim = size(ptmp%i2, dim = 1, kind = lint )
               nsize = size(ptmp%i2, dim = 2, kind = lint )
               
               ptmp1 => fll_mk('bound_elem_nodes', 'L', ndim, nsize, fpar)
               ndim = size(ptmp1%l2, dim = 1, kind = lint )
               nsize = size(ptmp1%l2, dim = 2, kind = lint )
               
               ptmp1%l2 = ptmp%i2
               ok = fll_mv(ptmp1, pelem, fpar)
               call fll_rm(ptmp, fpar)
            
            end do
      
      end do
      
      
      nelem = fll_nnodes(pgrid,'element_group','*',-1_lint,.false.,fpar)

      do ielem =1, nelem
          pelem =>  fll_locate(pgrid,'element_group','*',-1_lint,ielem,.false.,fpar,errmsg='ALL')
               
          ptmp => fll_locate(pelem,'element_nodes','*',-1_lint,1_lint,.false.,fpar,errmsg='ALL')
          ndim = size(ptmp%i2, dim = 1, kind = lint )
          nsize = size(ptmp%i2, dim = 2, kind = lint )
               
          ptmp1 => fll_mk('element_nodes', 'L', ndim, nsize, fpar)
          ndim = size(ptmp1%l2, dim = 1, kind = lint )
          nsize = size(ptmp1%l2, dim = 2, kind = lint )
               
          ptmp1%l2 = ptmp%i2
          ok = fll_mv(ptmp1, pelem, fpar)
          call fll_rm(ptmp, fpar)
            
      end do

    end do

            
 end subroutine mesh_ffa2fll
            
            
end module mesh_ffa2fll_m
