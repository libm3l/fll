!
!------------------------------------------------------------------------------
! NASA/LaRC, Computational Aerosciences Branch, D302
!------------------------------------------------------------------------------
!
! MODULE: fun3d_export_interface
!
!> @author
!> Adam Jirasek / Jirasek Technology LLC / USAFA
!> Date: 10/11/2018
!
!> Copyright (C) 2018  Jirasek Technology LLC
!> 
!
! DESCRIPTION:
!> Export interface mesh
!>
!> External dependencies
!>
!> this module uses fll library which is available at github.com/libm3l/fll
!> and is LGPL library. The basic type is type(dnode), pointer
!> Modules using this library need to add line
!>
!> use fll_mods_m
!
! REVISION HISTORY:
!------------------------------------------------------------------------------
MODULE EXPORT_INTERFACES_M

CONTAINS

  SUBROUTINE EXPORT_INTERFACES(PFLL, PINTERF, FMT, OUTPUTFILE)
!
!  reads ugrid file
!
    use fll_mods_m
    use fast_arr_ops_m

    implicit none
! 
! input/output data
!
    type(dnode), pointer :: pinterf,pfll
    character(len=*) :: outputfile
    character :: fmt
!
!   local data
! 
    type(dnode), pointer :: pgrid, pintf, pmesh, pbc,pnewintf,&
      pglobab,ptmp,pglbc,pbelem,pintfgrid,puniq,pnbc,pcopy
    type(func_data_set) :: fpar
    integer(lint) :: intf, nintf, igrid, ngrid, ibc,nbc, igbc,ngbc,&
       ibelem, nbcelem,tmpind,k3,k4,i,j,l,ind
    integer :: istat
    integer(lint), pointer :: bcind(:,:), bctria3(:,:), bcquad4(:,:),&
        bcuniqueu4(:),bcuniqueu3(:),bcuniqueutmp(:),bcunique(:)
    integer(lint), allocatable :: tmparray(:,:),tmparray1d(:)
    character(len=lstring_length), pointer :: bcnames(:)
    character(len=lstring_length) :: gbcname,bcname,intfname
    character(len=file_name_length) :: outfile
    real(rdouble), pointer :: coo(:,:)
    logical :: ok

    bctria3 => NULL(); bcquad4 => NULL()
!
!   get mesh grid pointer
!
    pmesh => fll_locate(PFLL,'grid','*',-1_lint,1_lint,.false.,fpar,errmsg='ALL')
    ngbc  = fll_nnodes(pmesh,'boundary','*',-1_lint,.false.,fpar)
    coo   => fll_getndata_d2(pmesh, 'coordinates', 1_lint, fpar)
!
!   loop over interfaces
!
    ngrid = fll_nnodes(pinterf,'grid','*',-1_lint,.false.,fpar)
!
!  loop over grids
!
    loop_grid: do igrid = 1,ngrid 

      pgrid => fll_locate(pinterf,'grid','*',-1_lint,igrid,.false.,fpar,errmsg='ALL')
!
!  get number of interfaces in 'grid'
!
      nintf = fll_nnodes(pgrid,'Interface','*',-1_lint,.false.,fpar)
!
!  loop over interfaces
!
      intfloop: do intf = 1,nintf

        pintf => fll_locate(pgrid,'Interface','*',-1_lint,igrid,.false.,fpar,errmsg='ALL')
        intfname = fll_getndata_s0(pintf,'Iname', 1_lint, fpar, errmsg='NONE')
        bcnames => fll_getndata_s1(pintf,'Ibname', 1_lint, fpar, errmsg='NONE')
        if(.not.fpar%success)then
          bcname = fll_getndata_s0(pintf,'Ibname', 1_lint, fpar, errmsg='NONE')
          nbc = 1
          if(.not.fpar%success)cycle
        else
          nbc = size(bcnames, dim = 1 , kind = lint)
        end if

        pnewintf => fll_mkdir('interface_mesh', fpar)
        
        pintfgrid  => fll_mkdir('grid', fpar)
        ok = fll_mv(pintfgrid, pnewintf, fpar)

        pglobab  => fll_mkdir('global_indexing', fpar)
!
!  just for clarity of output for user, add this after adding boundary
!
!        ok = fll_mv(pglobab, pintfgrid, fpar)

        loopibc: do ibc = 1,nbc
          loopigbc: do igbc = 1, ngbc
!
!  loop over boundary structure in mesh file and find if name is in Interface
!
            pbc => fll_locate(pmesh,'boundary','*',-1_lint,igbc,.false.,fpar,errmsg='ALL')
            gbcname = fll_getndata_s0(pbc,'boundary_name', 1_lint, fpar)

            if(nbc > 1) bcname = bcnames(ibc)

            if(trim(bcname) == trim(gbcname))then
!
!   loop over boundary element group
!
              nbcelem = fll_nnodes(pbc,'bound_elem_group','*',-1_lint,.false.,fpar)  

              ibelemloop: do ibelem = 1,nbcelem   
   
                pbelem => fll_locate(pbc,'bound_elem_group','*',-1_lint,ibelem,.false.,fpar)    
                bcind => fll_getndata_l2(pbelem,'bound_elem_nodes', 1_lint, fpar)
                gbcname = fll_getndata_s0(pbelem,'bound_elem_type', 1_lint, fpar)
!
! make boundary DIR in Interface
!
                if(trim(gbcname) == 'tria3')then
!
!  concentanate indexes in one array
!
                  if(.not.associated(bctria3))then 
                    allocate(bctria3(size(bcind, dim=1, kind = lint), 3), stat = istat)
                    bctria3 = bcind
                  else
                    call realloc_ld2arr(bctria3,bcind)
                  end if

                else if(trim(gbcname) ==   'quad4')then
!
!  concentanate indexes in one array
!
                  if(.not.associated(bcquad4))then 
                    allocate(bcquad4(size(bcind, dim=1, kind = lint), 4), stat = istat)
                    bcquad4 = bcind
                  else
                    call realloc_ld2arr(bcquad4,bcind)
                  end if

                else
                 write(*,*)' wrong element type'
                 stop
                end if

             end do ibelemloop

            end if

          end do loopigbc

        end do loopibc
        
        bcuniqueu3 => NULL()
        bcuniqueu4 => NULL()
!
!  save elements
!
        if(associated(bctria3))then

          pglbc => fll_mkdir('bound_elem_group', fpar)
          ok = fll_mv(pglbc, pglobab, fpar)

          ptmp => fll_mk('bound_elem_type', 'S', 1_lint, 1_lint, fpar)
          ok = fll_mv(ptmp, pglbc, fpar)
          ptmp%s0 = 'tria3'
          tmpind = size(bctria3, dim = 1, kind = lint)
          ptmp => fll_mk('bound_elem_nodes', 'L', tmpind, 3_lint, fpar)
          ok = fll_mv(ptmp, pglbc, fpar)
          ptmp%l2 = bctria3 

        end if

        if(associated(bcquad4))then

          pglbc => fll_mkdir('bound_elem_group', fpar)

          ok = fll_mv(pglbc, pglobab, fpar)
          ptmp => fll_mk('bound_elem_type', 'S', 1_lint, 1_lint, fpar)
          ok = fll_mv(ptmp, pglbc, fpar)
          ptmp%s0 = 'quad4'

          tmpind = size(bcquad4, dim = 1, kind = lint)
          ptmp => fll_mk('bound_elem_nodes', 'L', tmpind, 4_lint, fpar)
          ok = fll_mv(ptmp, pglbc, fpar)
          ptmp%l2 = bcquad4

        end if
!
!   find unique indexes
!
        if(associated(bctria3) .and. associated(bcquad4))then
!
!   if both tria and quad elements
!   concentenate arrays with unique indexes from tria3 and quad4 elements
!   and find unique indexes
!
           k3 = size(bctria3,    dim = 1, kind = lint)
           k4 = size(bcquad4, dim = 1, kind = lint)
           
           allocate(bcuniqueutmp(k3*3 + k4*4), tmparray1d(k3*3 + k4*4), stat = istat)
           
           ind = 1
           do i=1,k3
               do j=1,3
                  tmparray1d(ind) = bctria3(i,j)
                  ind = ind + 1
               end do
           end do
           do i=1,k4
               do j=1,4
                  tmparray1d(ind) = bcquad4(i,j)
                  ind = ind + 1
               end do
           end do
           
           call sort(tmparray1d) 
           call unique(tmparray1d,bcuniqueu4,k3)
           
           puniq => fll_mk('unique-global', 'L', k3, 1_lint, fpar)
           puniq%l1 = bcuniqueu4(1:k3)
           ok = fll_mv(puniq, pglobab, fpar) 
           tmpind = k3
         
         else

             if(associated(bctria3))then
            
                tmpind = size(bctria3, dim = 1, kind = lint)
                allocate(bcuniqueu3(tmpind*3),tmparray1d(tmpind*3),stat = istat)
                ind = 1
                do i=1,tmpind
                     do j=1,3
                       tmparray1d(ind) = bctria3(i,j)
                       ind = ind + 1
                     end do
                end do
! 
! get unique elements in bctria3 array
!
                call sort(tmparray1d) 
                call unique(tmparray1d,bcuniqueu3,k3)
                deallocate(bctria3,tmparray1d)           
            
                puniq => fll_mk('unique-global', 'L', k3, 1_lint, fpar)
                puniq%l1 = bcuniqueu3(1:k3)
                ok = fll_mv(puniq, pglobab, fpar) 
                tmpind = k3
               
            else if(associated(bcquad4))then
            
                tmpind = size(bcquad4, dim = 1, kind = lint)
                allocate(bcuniqueu4(tmpind*4),tmparray1d(tmpind*4),stat = istat)
                ind = 1
                do i=1,tmpind
                     do j=1,4
                       tmparray1d(ind) = bcquad4(i,j)
                       ind = ind + 1
                     end do
                end do
! 
! get unique elements in bcquad4 array
!
                call sort(tmparray1d) 
                call unique(tmparray1d,bcuniqueu4,k4)
                deallocate(bcquad4,tmparray1d)
            
                puniq => fll_mk('unique-global', 'L', k4, 1_lint, fpar)
                puniq%l1 = bcuniqueu4(1:k4)
                ok = fll_mv(puniq, pglobab, fpar)    
                tmpind = k4
            end if
         end if 
         
         bcuniqueutmp => puniq%l1
!
!  free memory
!
         if(associated(bcuniqueu3))deallocate(bcuniqueu3)
         if(associated(bcuniqueu4))deallocate(bcuniqueu4)
!
!  save coordinates
!
         ptmp => fll_mk('coordinates', 'D', tmpind, 3_lint, fpar)
         do i=1,tmpind
           ptmp%d2(i,:) = coo(bcuniqueutmp(i),:)
         end do
         ok = fll_mv(ptmp, pintfgrid, fpar)
!
!  immitate mesh structure
!
        pnbc => fll_mkdir('boundary', fpar)
        ok = fll_mv(pnbc,pintfgrid,fpar)
        
        ptmp => fll_mk('boundary_name', 'S', 1_lint, 1_lint, fpar)
        ok = fll_mv(ptmp,pnbc,fpar)
        ptmp%s0 = intfname
        
        k3  = fll_nnodes(pglobab,'bound_elem_group','*',-1_lint,.false.,fpar)
        do i=1,k3
            ptmp => fll_locate(pglobab,'bound_elem_group','*',-1_lint,i,.false.,fpar,errmsg='ALL')
            pcopy => fll_cp(ptmp,pnbc,fpar )
!
!   get bound_elem_nodes, sync with puniq and renumber
!
            ptmp => fll_locate(pcopy,'bound_elem_nodes','*',-1_lint,1_lint,.false.,fpar,errmsg='ALL')
            
            k3  = size(ptmp%l2, dim=1, kind = lint)
            k4  = size(ptmp%l2, dim=2, kind = lint)
            
            allocate(tmparray(k3,k4))
            tmparray = 0
!
!  renumber
! 
            do l=1,k3
               do j = 1,k4
                  tmparray(l,j) = arrindex(puniq%l1,ptmp%l2(l,j))
               end do
            end do
            
            ptmp%l2 = tmparray
            
            deallocate(tmparray)
        end do
!
!   add global-index here
!
        ok = fll_mv(pglobab, pintfgrid, fpar)

!        call fll_cat(pnewintf, 6, .true., fpar)
        
        outfile = "Interface_meshdata_"//trim(bcname)//".afll"
        
        ok = fll_write(pnewintf,outfile,9,FMT,fpar)
        call fll_rm(pnewintf,fpar)

      end do intfloop

    end do loop_grid

   
end subroutine export_interfaces
  
  subroutine realloc_ld2arr(a, c)
    use fll_mods_m
    implicit none
!
!***********************************************************************
!
!     function : reallocates two dimensional long integer array a
!                so that new a = a + c
!
    integer(lint), pointer, dimension(:,:) :: a
    integer(lint), intent(in) :: c(:,:)

    integer(lint), allocatable :: b(:,:)

    integer :: istat
    integer(lint) :: sizea,sizeb

    sizea = size(a,dim=1,kind=lint) + size(c,dim=1,kind=lint)
    sizeb = size(a,dim=2,kind=lint)

    allocate(b(size(a,dim=1,kind=lint), sizeb), stat = istat)
    if(istat /= 0)then
      write(*,*)'ERROR ALLOCATING MEMORY ==> interfaces.F90 ERR:3323 '
      stop
    end if

    b = a

    deallocate(a, stat = istat)
    if(istat /= 0)then
      write(*,*)'ERROR ALLOCATING MEMORY ==> interfaces.F90 ERR:3331 '
      stop
    end if

    allocate(a(sizea,sizeb), stat = istat)
    if(istat /= 0)then
      write(*,*)'ERROR ALLOCATING MEMORY ==> interfaces.F90 ERR:3337 '
      stop
    end if

    a(1:size(b),:) = b
    a(size(b)+1:,:) = c

    deallocate(b, stat = istat)
    if(istat /= 0)then
      write(*,*)'ERROR DEALLOCATING MEMORY ==> interfaces.F90 ERR:3346 '
      stop
    end if

  end subroutine realloc_ld2arr  
 
  
end module export_interfaces_m
