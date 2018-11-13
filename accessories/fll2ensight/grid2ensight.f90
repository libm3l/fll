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
module grid2ensight_m

CONTAINS

  subroutine grid2ensight(pglob, name,vol)
!
!  converts file to ensight format
!
    use fll_mods_m
    use mesh_element_info_m

    implicit none
! 
! input/output data
!
    type(dnode), pointer :: pglob
    type(func_data_set) :: fpar
    character(len=*) :: name
    logical :: vol
!
! local paramaters
!
    type(dnode), pointer :: pgrid,pelem,pbound
    integer(lint) :: igrid,ngrid, nelem,ielem,nbound,ibound,ndim1,ndim2,&
       i,nnodes,nunique
    integer(lint), pointer :: nindex(:,:)
    integer(lint), allocatable :: nindex_scaled(:,:),iuniquenodes(:)
    integer :: npart,iunit, istat
    real(rdouble), pointer :: coo(:,:)
    real(rsingle), allocatable :: coord(:)
    character(len=lstring_length) :: fmsh
    character(80) :: buffer
    character(len=lstring_length) :: etype, bcname
    integer tmpi
!
!  find number of grids
!
    ngrid = fll_nnodes(pglob,'grid','*',-1_lint,.false.,fpar)
    if(ngrid < 1)then
      write(*,*)' ERROR: did not find grid in mesh file, check your input files'
      write(*,*)'terminating ....'
      stop
    end if
!
!   open ensigth .case file and write header
!
     iunit = 15
     open(iunit,file=trim(name)//'.case')
     write(iunit,10) trim(name)//'.geo'

   10 format(&
     'FORMAT'        ,/ ,&
     'type:	ensight gold',//,&
     'GEOMETRY'      ,/ ,&
     'model:	'       ,a20)  !,//,&
!     'VARIABLE'       )
     close(iunit)
!
!  ... and geo file header
!
     fmsh = trim(name)//'.geo'
     open(iunit, file=fmsh, form='unformatted', access='stream', status='replace')   !, convert="little_endian")
     buffer = 'C Binary'
     write(iunit) buffer
     buffer = 'Unstructured grid of configuration:'
     write(iunit) buffer
     buffer = 'Mesh'
     write(iunit) buffer
     buffer = 'node id assign'
     write(iunit) buffer
     buffer = 'element id assign'
     write(iunit) buffer
!
!  set part counter to 1
!
     npart = 0
!
!  loop over grids
!
    loop_grid: do igrid = 1,ngrid 

      pgrid => fll_locate(pglob,'grid','*',-1_lint,igrid,.false.,fpar,errmsg='ALL')
!
!  get coordinates
!
      coo => fll_getndata_d2(pgrid, 'coordinates', 1_lint, fpar)
!
!  if volume 
!
      volume: if(vol)then
!
!  write coordinates 
!
        npart = npart + 1
        buffer = 'part'
        write(iunit) buffer
        write(iunit)  npart
        buffer= 'Volume'
        write(iunit) buffer
        buffer = 'coordinates'
        write(iunit) buffer
        
        nnodes = size(coo, dim = 1) 

        allocate(coord(nnodes), stat = istat)
          if(istat /= 0)then
             write(*,*)'ERROR ALLOCATING MEMORY'
             stop
          end if
                
        tmpi = nnodes
        write(iunit) tmpi
        coord = coo(:,1)
        write(iunit) (coord(i),i=1,nnodes)
        coord = coo(:,2)
        write(iunit) (coord(i),i=1,nnodes)
        coord = 0
        write(iunit) (coord(i),i=1,nnodes)
        

        deallocate(coord, stat = istat)
          if(istat /= 0)then
            write(*,*)'ERROR DEALLOCATING MEMORY'
            stop
          end if
!
!  write element info
!
        nelem = fll_nnodes(pgrid,'element_group','*',-1_lint,.false.,fpar)
        
        do ielem = 1,nelem
          pelem => fll_locate(pgrid,'element_group','*',-1_lint,ielem,.false.,fpar,errmsg='ALL')
          etype = fll_getndata_s0(pelem,'element_type', 1_lint, fpar)
          nindex => fll_getndata_l2(pelem, 'element_nodes', 1_lint, fpar)
          call mesh_element_info(iunit,nindex,etype)
        end do        
      end if volume
!
!  write boundaries
!
      nbound = fll_nnodes(pgrid,'boundary','*',-1_lint,.false.,fpar)
      if(nbound < 1) then
        write(*,*)' NOTE: Did not find any boundary conditions in mesh file'
      end if
      
      bcloop: do ibound = 1,nbound
      
         pbound =>  fll_locate(pgrid,'boundary','*',-1_lint,ibound,.false.,fpar,errmsg='ALL')
         bcname = fll_getndata_s0(pbound, 'boundary_name', 1_lint, fpar)
         write(*,*)' processing boundary ', trim(bcname)
!
!  find number of element types
!
         nelem = fll_nnodes(pbound,'bound_elem_group','*',-1_lint,.false.,fpar)
         if(nelem < 0)then
            write(*,*)' ERROR: did not find element info for boundary ', trim(bcname)
            write(*,*)' terminating ...'
            stop
         end if 
!
!  loop over elements
!
         belemloop: do ielem = 1, nelem
             pelem => fll_locate(pbound,'bound_elem_group','*',-1_lint,ielem,.false.,fpar,errmsg='ALL')
             etype = fll_getndata_s0(pelem,'bound_elem_type', 1_lint, fpar)
             nindex => fll_getndata_l2(pelem, 'bound_elem_nodes', 1_lint, fpar)
             ndim1 = size(nindex, dim = 1, kind = lint)
             ndim2 = size(nindex, dim = 2, kind = lint)
!
!  find unique coordinates from element information
!
            allocate(iuniquenodes(ndim1*ndim2), stat = istat)
            if(istat /= 0)then
               write(*,*)'ERROR ALLOCATING MEMORY'
               stop
            end if
            
            call unique21(nindex,iuniquenodes, nunique)
            write(*,*)' unique boundary nodes: ', nunique
!
!   write coordinates
!
            npart = npart + 1
            buffer = 'part'
            write(iunit) buffer
            write(iunit)  npart
            buffer=bcname
            write(iunit) buffer
            buffer = 'coordinates'
            write(iunit) buffer
        
            allocate(coord(nunique), stat = istat)
            if(istat /= 0)then
               write(*,*)'ERROR ALLOCATING MEMORY'
               stop
            end if
         
            tmpi = nunique
            write(iunit) tmpi

            do i=1,nunique
              coord(i) = coo(iuniquenodes(i),1)
            end do
            write(iunit) (coord(i),i=1,nunique)
            
            do i=1,nunique
              coord(i) = coo(iuniquenodes(i),2)
            end do            
            write(iunit) (coord(i),i=1,nunique)
            
            do i=1,nunique
              coord(i) = coo(iuniquenodes(i),3)
            end do            
            write(iunit) (coord(i),i=1,nunique)

            deallocate(coord, stat = istat)
              if(istat /= 0)then
                write(*,*)'ERROR DEALLOCATING MEMORY'
                stop
              end if
!
!   write element information
!
            allocate(nindex_scaled(ndim1,ndim2), stat = istat)
            if(istat /= 0)then
               write(*,*)'ERROR ALLOCATING MEMORY'
               stop
            end if
!
!  sync and renumber
!            
            call renumber(iuniquenodes(1:nunique),nindex,nindex_scaled)
!
!  write mesh element
!
            call mesh_element_info(iunit,nindex_scaled,etype)
            
            deallocate(iuniquenodes, nindex_scaled,  stat = istat)
               if(istat /= 0)then
                write(*,*)'ERROR DEALLOCATING MEMORY'
                stop
              end if                   
         end do belemloop
      end do bcloop

    end do loop_grid
!
!  close file
!
    close(iunit)
   
end subroutine grid2ensight



  subroutine unique21(iinodes,iuniquenodes,k)
  
    use fll_mods_m
    implicit none
!
! input/output parameters
!
    integer(lint), intent(in) :: iinodes(:,:)
    integer(lint), intent(out) :: iuniquenodes(:)
    integer(lint) :: k
!
!  lcoal parameters
!
   integer(lint) :: i,nunique,j,dim2
   
    dim2 = size(iinodes, dim = 2, kind = lint)
    k = 1
    iuniquenodes(1) = iinodes(1,1)

    nunique = size(iinodes, dim=1, kind = lint)
    do i=2,nunique
!
!     if the number already exist check next
!
      do j=1,dim2
        if (any( iuniquenodes(1:k) == iinodes(i,j) )) cycle
!
!     No match found so add it to the iuniquenodes
!
        k = k + 1
        iuniquenodes(k) = iinodes(i,j)
      end do
    end do
    
 end subroutine unique21
 
 
 
 subroutine renumber(iuniquenodes,nindex,nindex_renum)
 
     use fll_mods_m
     implicit none
 
     integer(lint)  ::  iuniquenodes(:)
     integer(lint)  :: nindex(:,:),nindex_renum(:,:)
     integer(lint)  :: j,k,l,k3,k4
     
     k3 = size(nindex, dim = 1, kind = lint)
     k4 = size(nindex, dim = 2, kind = lint)
     
     nindex_renum = 1
     do l=1,k3
        do j = 1,k4
            do k=1,size(iuniquenodes, dim=1, kind = lint)
 !               write(*,*)nindex(l,j), iuniquenodes(k)
                if(nindex(l,j) == iuniquenodes(k))then
 !                  write(*,*)nindex(l,j),k
 !                  read(*,*)
                   nindex_renum(l,j)=k
                   cycle
                 end if
             end do
          end do
       end do
       
       return
            
 end subroutine renumber
            
            
end module grid2ensight_m
