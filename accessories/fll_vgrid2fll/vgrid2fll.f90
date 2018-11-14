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
module vgrid2fll_m

contains

  subroutine vgrid2fll(pglob)
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
    type(dnode), pointer :: pgrid,pbound,pelem,ptmp,pcogsg,pvgrid,&
       pbc, pmapbc,pbgroupd
    type(func_data_set) :: fpar
    logical :: ok
    character(len=lstring_length), pointer :: family(:)
    integer(lint), pointer :: patch(:), bcpatch(:),bcnode(:,:)
    integer(lint) :: ibound, nbound,ipatch,nelems,i
!
!   loop over grids
!
    pgrid => fll_mkdir('grid', fpar)
    ok = fll_mv(pgrid, pglob, fpar)
!
!  find coordinates and move the to grid
!
    pvgrid => fll_locate(pglob,'Vgrid','*',-1_lint,1_lint,.false.,fpar,errmsg='ALL')
    pcogsg => fll_locate(pvgrid,'Cogsg','*',-1_lint,1_lint,.false.,fpar,errmsg='ALL')
    
    ptmp => fll_locate(pcogsg,'coordinates','*',-1_lint,1_lint,.false.,fpar,errmsg='ALL')
    ok = fll_mv(ptmp, pgrid, fpar)
!
!  save volume elements in grid
!
    pelem => fll_mkdir('element_group',fpar)
    ok = fll_mv(pelem, pgrid, fpar)
    
    ptmp => fll_mk('element_type','S',1_lint,1_lint,fpar)
    ok = fll_mv(ptmp, pelem, fpar)
    ptmp%s0 = 'tetra4'
    
    ptmp => fll_locate(pcogsg,'nodes','*',-1_lint,1_lint,.false.,fpar,errmsg='ALL')
    ok = fll_mv(ptmp, pelem, fpar)
    ptmp%lname = 'element_nodes'
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
!
!  loop over family and create boundary 
!
    nbound = size(family, kind = lint)
    do ibound = 1,nbound
    
       pbound => fll_mkdir('boundary', fpar)
       ok = fll_mv(pbound, pgrid, fpar)
       
       ptmp => fll_mk('boundary_name','S',1_lint,1_lint,fpar)
       ptmp%s0 = family(ibound)
       ok = fll_mv(ptmp,pbound, fpar)
       
       pbgroupd => fll_mkdir('bound_elem_group', fpar)
       ok = fll_mv(pbgroupd,pbound, fpar)
       
       ptmp => fll_mk('bound_elem_type','S',1_lint,1_lint,fpar)
       ptmp%s0 = 'tria3'
       ok = fll_mv(ptmp,pbgroupd, fpar)
!
!  identify elements belonging to this patch
!
       ipatch = patch(ibound)
       nelems = 0
       do i=1,size(bcpatch,dim=1,kind = lint)
           if(bcpatch(i) == ipatch) nelems = nelems + 1
       end do 
       
       ptmp => fll_mk('bound_elem_nodes','L',nelems,3_lint,fpar)
       ok = fll_mv(ptmp,pbgroupd, fpar)

       nelems = 0
       do i=1,size(bcpatch,dim=1,kind = lint)
           if(bcpatch(i) == ipatch) then
              nelems = nelems + 1
              ptmp%l2(nelems,:) = bcnode(i,:)
           end if
       end do

    end do
   
  end subroutine vgrid2fll





  subroutine realloc(a, c)
    use fll_mods_m
    implicit none
!
!***********************************************************************
!
!     function : reallocates 2 dimensional double array a
!                so that new a = a + c
!
    real(rdouble), pointer, dimension(:,:) :: a
    real(rdouble), intent(in) :: c(:,:)

    real(rdouble), allocatable :: b(:,:)

    integer :: istat
    integer(lint) :: sizea,sizeb
    
    sizea = size(a,dim=1,kind=lint) + size(c,dim=1,kind=lint)
    sizeb = size(a,dim=2,kind=lint)


    allocate(b(size(a,dim=1,kind=lint), sizeb), stat = istat)
    if(istat /= 0)then
      write(*,*)'ERROR ALLOCATING MEMORY'
      stop
    end if

    b = a

    deallocate(a, stat = istat)
    if(istat /= 0)then
      write(*,*)'ERROR ALLOCATING MEMORY  '
      stop
    end if

    allocate(a(sizea,sizeb), stat = istat)
    if(istat /= 0)then
      write(*,*)'ERROR ALLOCATING MEMORY  '
      stop
    end if

    a(1:size(b,dim=1,kind=lint),:) = b
        a(size(b,dim=1,kind=lint)+1:,:) = c

    deallocate(b, stat = istat)
    if(istat /= 0)then
      write(*,*)'ERROR DEALLOCATING MEMORY '
      stop
    end if

  end subroutine realloc
   

end module vgrid2fll_m
