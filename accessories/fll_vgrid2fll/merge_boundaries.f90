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
module merge_boundaries_m

contains

  subroutine merge_boundaries(pglob)
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
    type(dnode), pointer :: pcogsg,pvgrid,&
       pbc, pmapbc, ptmp
    type(func_data_set) :: fpar
    logical :: ok
    character(len=lstring_length), pointer :: family(:)
    integer(lint), pointer :: patch(:), bcpatch(:),bcnode(:,:)
    integer(lint) :: nbound,nuniquebc,patchnum,oldpatchnum,npatch,i,j,k
    integer :: istat
    integer(lint), allocatable :: newpatchn(:)
    character(len=lstring_length), allocatable :: newbc(:)
!
!  find pointers to cogsg data
!
    pvgrid => fll_locate(pglob,'Vgrid','*',-1_lint,1_lint,.false.,fpar,errmsg='ALL')
    pcogsg => fll_locate(pvgrid,'Cogsg','*',-1_lint,1_lint,.false.,fpar,errmsg='ALL')
!
!  do boundary conditions
!
    pmapbc => fll_locate(pvgrid,'Mapbc','*',-1_lint,1_lint,.false.,fpar,errmsg='ALL')
    pbc        => fll_locate(pvgrid,'Bc','*',-1_lint,1_lint,.false.,fpar,errmsg='ALL')
!
!  find name of boundary conditions and their associate path number
!
    family => fll_getndata_s1(pmapbc, 'Family', 1_lint, fpar)
    patch  => fll_getndata_l1(pmapbc, 'patch', 1_lint, fpar)
!
!  gat patch array and BC element from Bc
!
    bcpatch  => fll_getndata_l1(pbc, 'patch', 1_lint, fpar)
    bcnode   => fll_getndata_l2(pbc, 'node', 1_lint, fpar)
    
    npatch = size(bcpatch, dim = 1, kind = lint)
!
    nbound = size(family, dim = 1, kind = lint)
    allocate(newbc(nbound), newpatchn(nbound), stat = istat)
    if(istat /= 0)then
       write(*,*)'ERROR ALLOCATING'
       write(*,*)'terminating ...'
       stop
    end if
!
!  find unique names
!
    nuniquebc = 1
    newbc(1) = family(1)
    newpatchn(1) = patch(1)
    do i=2,nbound
       if (any( newbc(1:nuniquebc) == family(i) )) cycle
       nuniquebc = nuniquebc + 1
       newbc(nuniquebc) = family(i)
       newpatchn(nuniquebc) = patch(i)
    end do    
    
    write(*,*)' number of unique boundaries is ', nuniquebc
    do i=1,nuniquebc
      write(*,*)'Name of BC is ',trim(newbc(i))
    end do
!
!  we have nuniquebc BCs, renumber patch numbers
!
    do i=1,nuniquebc
       patchnum = newpatchn(i)
       
       do j=1,nbound
       
          if(newbc(i) == family(j))then
             oldpatchnum = patch(j)             
               do k=1,npatch
                 if(bcpatch(k) == oldpatchnum)bcpatch(k) = patchnum
               end do
               
          end if
          
       end do
    end do
!
!   reallocate
!
    ptmp => fll_locate(pmapbc,'Family','*',-1_lint,1_lint,.false.,fpar,errmsg='ALL')
    call fll_rm(ptmp, fpar)
    ptmp => fll_locate(pmapbc,'patch','*',-1_lint,1_lint,.false.,fpar,errmsg='ALL')
    call fll_rm(ptmp, fpar)
  
  
    ptmp => fll_mk('patch','L',nuniquebc,1_lint,fpar)
    ptmp%l1 = newpatchn(1:nuniquebc)
    ok = fll_mv(ptmp, pmapbc, fpar)

    ptmp => fll_mk('Family','S',nuniquebc,1_lint,fpar)
    ptmp%s1 = newbc(1:nuniquebc)
    ok = fll_mv(ptmp, pmapbc, fpar)
  
  !  family => fll_getndata_s1(pmapbc, 'Family', 1_lint, fpar)
  !  patch  => fll_getndata_l1(pmapbc, 'patch', 1_lint, fpar)
  !  pmapbc => fll_locate(pvgrid,'Mapbc','*',-1_lint,1_lint,.false.,fpar,errmsg='ALL')
  
    deallocate(newbc, newpatchn, stat = istat)
    if(istat /= 0)then
       write(*,*)'ERROR DEALLOCATING'
       write(*,*)'terminating ...'
       stop
    end if
    
    
  end subroutine merge_boundaries

end module merge_boundaries_m
