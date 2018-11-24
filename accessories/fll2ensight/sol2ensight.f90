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
module sol2ensight_m

CONTAINS

  subroutine sol2ensight(pglob, name,vol)
!
!  converts file to ensight format
!
    use fll_mods_m
    use mesh_element_info_m
    use fast_arr_ops_m

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
    integer :: iunit_case, iunit, istat, npart
    type(dnode), pointer :: pgrid, psol,psolgrid, pbound,ptmp
    integer(lint) :: nnodes,nchildren,ichildren,nsize,ibound,nbound,i,j,&
        igrid,ngrid,ndim
    real(rsingle), allocatable :: coord(:)
    integer(lint), pointer :: bcindex(:)
    character(len=file_name_length) :: varname, filename
    character(80) :: buffer
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
!  fiind solution
!
    psol => fll_locate(pglob,'solution','*',-1_lint,1_lint,.false.,fpar,errmsg='ALL')
!
!   open ensigth .case file and write header
!
     iunit_case = 15
     iunit = 16
     open(iunit_case,file=trim(name)//'.case', access = 'append')
     
     write(iunit_case,*)
     write(iunit_case,105)
105   format('VARIABLE')
!
!  loop over grids
!
    loop_grid: do igrid = 1,ngrid 

      pgrid      => fll_locate(pglob,'grid','*',-1_lint,igrid,.false.,fpar,errmsg='ALL')
      nnodes  = fll_getndata_l0(pgrid, 'nodes', 1_lint, fpar)
      
      allocate(coord(nnodes), stat = istat)
        if(istat /= 0)then
          write(*,*)'ERROR ALLOCATING'
          write(*,*)'Terminating ...'
          stop
       end if
       
       psolgrid => fll_locate(psol,'grid','*',-1_lint,igrid,.false.,fpar,errmsg='ALL')
       if(.not.fpar%success)then
          write(*,*)'ERROR - did not find any grid pointer in solution file'
          write(*,*)'Terminating ...'
          stop
       end if 
!
!    find number of items in grid, then loop over them and find if it is double array 
!    whose size is nnodes and then save this
!
      nchildren = psolgrid%ndim
      
      loopchildren: do ichildren = 1,nchildren
      
         ptmp => fll_locate(psolgrid,'*','*',-1_lint,ichildren,.false.,fpar,errmsg='ALL')
!
!    type not double
!
         if(ptmp%ltype /= 'D') cycle
!
!   first dimension of double array not equal to number of mesh points
!
         if(ptmp%ndim /= nnodes) cycle
         nsize = ptmp%nsize
!
!  get name of variable
!
         varname = trim(adjustl(ptmp%lname))
         write(*,*)' processing variable: ',trim(varname)
!
!   add record to .case file
!
         if(nsize == 1)then
            filename = trim(name)//'.'//trim(varname)//'.scl'
            write(iunit_case,100)trim(varname), trim(filename)
100 FORMAT('scalar per node:        ', A,'   ',A)
!
!  scalar
!
         else
!
!  vector
!
            filename = trim(name)//'_'//trim(varname)//'.vec'
            write(iunit_case,101)trim(varname), trim(filename)
101 FORMAT('vector per node:        ', A,'   ',A)
         end if
!
         open(iunit, file=filename, form='unformatted', access='stream', status='replace')   !, convert="little_endian")
!
!  set part counter to 1
!
         npart = 0
         buffer = trim(varname)
         write(iunit) buffer
!
!  write volume data
!
         volume: if(vol)then

             buffer = 'part'
             write(iunit) buffer
             npart = npart + 1
             write(iunit) npart
             buffer = 'coordinates'
             write(iunit) buffer 
!
             if(nsize > 1)then
               do i=1,nsize
                 coord = ptmp%d2(:,i)
                 write(iunit) (coord(j),j=1,nnodes)
               end do
             else
               coord = ptmp%d1(:)
               write(iunit) (coord(j),j=1,nnodes)
             end if

         end if volume
!
!  loop over boundaries
!  
         nbound = fll_nnodes(pgrid,'boundary','*',-1_lint,.false.,fpar)
      
         bcloop: do ibound = 1,nbound
      
            pbound =>  fll_locate(pgrid,'boundary','*',-1_lint,ibound,.false.,fpar,errmsg='ALL') 
            bcindex => fll_getndata_l1(pbound, 'unique_ind_arr', 1_lint, fpar)
            ndim = size(bcindex, dim = 1, kind = lint)
            
            buffer = 'part'
            write(iunit) buffer
            npart = npart + 1
            write(iunit) npart
            buffer = 'coordinates'
            write(iunit) buffer 
!
            if(nsize > 1)then
               do i=1,nsize
                 do j=1,ndim
                   coord(j) = ptmp%d2(bcindex(j),i)
                 end do
                 write(iunit) (coord(j),j=1,nnodes)
               end do
            else
               do j=1,ndim
                  coord(j) = ptmp%d1(bcindex(j))
               end do               
               write(iunit) (coord(j),j=1,nnodes)
            end if
            
         end do bcloop
         
         close(iunit)
      
      end do loopchildren
      
           
      deallocate(coord, stat = istat)
        if(istat /= 0)then
            write(*,*)'ERROR DEALLOCATING'
            write(*,*)'Terminating ...'
            stop
       end if
       
      end do loop_grid
      
      close(iunit_case)

       
     return
            
 end subroutine sol2ensight
            
            
end module sol2ensight_m
