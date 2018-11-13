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
!     Description: read vgrid files 
!
!     the procedure for reading is found at 
!     http://www.simcenter.msstate.edu/software/downloads/doc/ug_io/3d_grid_file_type_vgrid.html
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
module fll_read_vgrid_m

contains

  subroutine fll_read_vgrid_mapbc(pglob,filemapbc)
!
!  reads vgrid mapbc file
!
    use fll_mods_m
    
    implicit none
! 
! input/output data
!
    type(dnode), pointer :: pglob
    character(len=*) :: filemapbc
!
! local declarations
!    
    type(dnode), pointer :: pvgrid,pmabbc,ptmp
    type(func_data_set) :: fpar
    character(len=80) :: text
    integer :: a,b,c,d,e
    integer(lint) :: ipatch, npatch
    integer(lint), pointer :: patch(:),bcs(:)
    character(len=lstring_length), pointer :: family(:)
    logical :: ok
!
    pvgrid => fll_mkdir('Vgrid', fpar)
    ok = fll_mv(pvgrid, pglob, fpar)
    pmabbc => fll_mkdir('Mapbc', fpar)
    ok = fll_mv(pmabbc, pvgrid, fpar)
!
!   find number of patches
!
    open(15,file=filemapbc,form='formatted')
    npatch = 0

    read(15,'(a80)')text
    read(15,'(a80)')text
    read(15,'(a80)')text
    read(15,'(a80)')text
    
    do 
10      read(15,*, err =10, end =100)a,b,c,e,d,text
      npatch = npatch + 1
    end do
100 close(15)
!
    ptmp => fll_mk('patch','L',npatch,1_lint,fpar)
    patch => ptmp%l1
    ok = fll_mv(ptmp, pmabbc, fpar)
    ptmp => fll_mk('BC','L',npatch,1_lint,fpar)
    bcs  => ptmp%l1
    ok = fll_mv(ptmp, pmabbc, fpar)
    ptmp => fll_mk('Family','S',npatch,1_lint,fpar)
    family => ptmp%s1
    ok = fll_mv(ptmp, pmabbc, fpar)

    open(15,file=filemapbc,form='formatted')   
    read(15,'(a80)')text
    read(15,'(a80)')text
    read(15,'(a80)')text
    read(15,'(a80)')text 

    do ipatch=1,npatch
      read(15,*)patch(ipatch),bcs(ipatch),a,b,c,family(ipatch)
    enddo

    close(15)
   
  end subroutine fll_read_vgrid_mapbc



  subroutine fll_read_vgrid_bc(pglob,filebc)

    use fll_mods_m
    implicit none
!
!  input/output variables
!
    type(dnode), pointer :: pglob
    character(len=*) :: filebc
!
!  local variables
!
    type(dnode), pointer :: pvgrid,pmabbc,ptmp
    type(func_data_set) :: fpar
    character(len=80) :: text
    integer(lint), pointer :: patch(:), nodes(:,:)
    integer(lint) :: if, in, a, nbf,nbc,npatch,igrid
    logical :: ok

    open(15,file=filebc,form='formatted')

    pvgrid => fll_locate(pglob,'Vgrid','*',-1_lint,1_lint,.false.,fpar,errmsg='ALL')
    pmabbc => fll_mkdir('Bc', fpar)
    ok = fll_mv(pmabbc, pvgrid, fpar)

    read(15,*)nbf,nbc,npatch,igrid
    read(15,'(a80)')text   

    ptmp => fll_mk('patch','L',nbf,1_lint,fpar)
    patch => ptmp%l1
    ok = fll_mv(ptmp, pmabbc, fpar)
    ptmp => fll_mk('node','L',nbf,3_lint,fpar)
    nodes=> ptmp%l2
    ok = fll_mv(ptmp, pmabbc, fpar)

    do if=1,nbf
      read(15,*)a,patch(if),(nodes(if,in),in=1,3)
    end do
    close(15)

  end subroutine fll_read_vgrid_bc



  subroutine fll_read_vgrid_cogsg(pglob, filecogsg)

   use fll_mods_m
   implicit none
!
!  input/output parameters
!
   type(dnode), pointer :: pglob
   character(len=*) :: filecogsg
!
!  local parameters
!
    type(dnode), pointer :: pvgrid,pmabbc,ptmp
    type(func_data_set) :: fpar

    integer :: ios,inew,nc,npo,nbn,npv,nev,ic,in,istat,ip
    integer, allocatable :: cnode(:,:)
    integer(lint), pointer :: nodes(:,:)
    real(rdouble) :: t
    real(rdouble), pointer :: crd(:,:)
    logical :: ok

    pvgrid => fll_locate(pglob,'Vgrid','*',-1_lint,1_lint,.false.,fpar,errmsg='ALL')
    pmabbc => fll_mkdir('Cogsg', fpar)
    ok = fll_mv(pmabbc, pvgrid, fpar)
!
    write(*,*)' opening file ', trim(filecogsg)
    open(15,file=trim(filecogsg),form='unformatted',iostat=ios,status='old',convert='big_endian')

    read(15)inew,nc,npo,nbn,npv,nev,t

    write(*,*)inew,nc,npo,nbn,npv,nev,t
    rewind (unit=15, iostat=ios)

    allocate(cnode(nc,4), stat = istat)
    if(istat /= 0)then
      write(*,*)'Error allocating cnode array'
      write(*,*)'terminating ...'
      stop
    end if

    ptmp => fll_mk('coordinates','D',int(npo, kind = lint),3_lint,fpar)
    crd => ptmp%d2
    ok = fll_mv(ptmp, pmabbc, fpar)

    ptmp => fll_mk('nodes','L',int(nc, kind = lint),4_lint,fpar)
    nodes => ptmp%l2
    ok = fll_mv(ptmp, pmabbc, fpar)
! 
    read(15)inew,nc,npo,nbn,npv,nev,t,((cnode(ic,in),ic=1,nc),in=1,4)
    read(15)(crd(ip,1),ip=1,npo), &
            (crd(ip,2),ip=1,npo), &
            (crd(ip,3),ip=1,npo)
    close(15)

    nodes = cnode
!
    deallocate(cnode)

  end subroutine fll_read_vgrid_cogsg

   
end module fll_read_vgrid_m
