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
       ibelem, nbcelem,tmpind,k3,k4,i,j,l,k
    integer :: istat
    integer(lint), pointer :: bcind(:,:), bctria3(:,:), bcquad4(:,:),&
        bcuniqueu4(:),bcuniqueu3(:),bcuniqueutmp(:),bcunique(:)
    integer(lint), allocatable :: tmparray(:,:)
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


        call fll_cat(pfll, 6, .true., fpar)

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
          
          allocate(bcuniqueu3(tmpind*3), stat = istat)
! 
! get unique elements in bctria3 array
!
          call unique21(bctria3,bcuniqueu3,k3)
          deallocate(bctria3)

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
! 
! get unique elements in bctria4 array
!
          allocate(bcuniqueu4(tmpind*4), stat = istat)
          call unique21(bcquad4,bcuniqueu4,k4)
          deallocate(bcquad4)
        end if
!
!   concentenate arrays with unique indexes from tria3 and quad4 elements
!
        if(associated(bcuniqueu3))then
           allocate(bcuniqueutmp(k3), stat = istat)
           bcuniqueutmp = bcuniqueu3(1:k3)
           if(associated(bcuniqueu4))then
              call realloc_l1arr(bcuniqueutmp, bcuniqueu4(1:k4))
              call unique21(bcquad4,bcuniqueu4,k4)
           end if
        else
          bcuniqueutmp => bcuniqueu4(1:k4)
        end if
!
!  check for bcuniqueutmp nodes
!
        allocate( bcunique(size(bcuniqueutmp, dim = 1, kind = lint)))
!
!  find unique elements, the double elements may be at the intersection
!  of quad4 and tria elements
!
        call unique(bcuniqueutmp, bcunique, tmpind)
!
!  sort punique array
!
        call sort(bcuniqueutmp) 
!
!       save unique array of global indexes
!
         puniq => fll_mk('unique-global', 'L', tmpind, 1_lint, fpar)
         puniq%l1 = bcuniqueutmp
         ok = fll_mv(puniq, pglobab, fpar)
!
!  free memory
!
         if(associated(bcuniqueu3))deallocate(bcuniqueu3)
         if(associated(bcuniqueu4))deallocate(bcuniqueu4)
         deallocate(bcunique)
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
!                 do k=1, size(puniq%l1)
                  tmparray(l,j) = arrindex(puniq%l1,ptmp%l2(l,j))
!                   if(ptmp%l2(l,j) == puniq%l1(k))then
!                     tmparray(l,j) = k
!                     cycle
!                   end if
!                  end do
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

   
END SUBROUTINE EXPORT_INTERFACES
  
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
  
  
  
  subroutine realloc_d2arr(a, c)
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
      write(*,*)'ERROR ALLOCATING MEMORY'
      stop
    end if

    allocate(a(sizea,sizeb), stat = istat)
    if(istat /= 0)then
      write(*,*)'ERROR ALLOCATING MEMORY'
      stop
    end if

    a(1:size(b,dim=1,kind=lint),:) = b
        a(size(b,dim=1,kind=lint)+1:,:) = c

    deallocate(b, stat = istat)
    if(istat /= 0)then
      write(*,*)'ERROR DEALLOCATING MEMORY'
      stop
    end if

  end subroutine realloc_d2arr
  

  subroutine realloc_l1arr(a, c)
    use fll_mods_m
    implicit none
!
!***********************************************************************
!
!     function : reallocates 1 dimensional double array a
!                so that new a = a + c
!
    integer(lint), pointer, dimension(:) :: a
    integer(lint), intent(in) :: c(:)

    integer(lint), allocatable :: b(:)

    integer :: istat
    integer(lint) :: sizea
    
    sizea = size(a,dim=1,kind=lint) + size(c,dim=1,kind=lint)


    allocate(b(size(a,dim=1,kind=lint)), stat = istat)
    if(istat /= 0)then
      write(*,*)'ERROR ALLOCATING MEMORY ==> interfaces.F90 ERR:3376 '
      stop
    end if

    b = a

    deallocate(a, stat = istat)
    if(istat /= 0)then
      write(*,*)'ERROR ALLOCATING MEMORY ==> interfaces.F90 ERR:3384 '
      stop
    end if

    allocate(a(sizea), stat = istat)
    if(istat /= 0)then
      write(*,*)'ERROR ALLOCATING MEMORY ==> interfaces.F90 ERR:3390 '
      stop
    end if

    a(1:size(b,dim=1,kind=lint)) = b
    a(size(b,dim=1,kind=lint)+1:) = c

    deallocate(b, stat = istat)
    if(istat /= 0)then
      write(*,*)'ERROR DEALLOCATING MEMORY ==> interfaces.F90 ERR:3399 '
      stop
    end if

  end subroutine realloc_l1arr  
  
  
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
   integer(lint) :: i,nunique,j
   
    k = 1
    iuniquenodes(1) = iinodes(1,1)

    nunique = size(iinodes, dim=1, kind = lint)
    do i=2,nunique
!
!     if the number already exist check next
!
      do j=1,size(iinodes, dim = 2, kind = lint)
        if (any( iuniquenodes(1:k) == iinodes(i,j) )) cycle
!
!     No match found so add it to the iuniquenodes
!
        k = k + 1
        iuniquenodes(k) = iinodes(i,j)
      end do
    end do
    
 end subroutine unique21
 
 
 
  subroutine unique(iinodes,iuniquenodes,k)
  
    use fll_mods_m
    implicit none
!
! input/output parameters
!
    integer(lint), intent(in) :: iinodes(:)
    integer(lint), intent(out) :: iuniquenodes(:)
    integer(lint) :: k
!
!  lcoal parameters
!
   integer(lint) :: i,nunique
   
    k = 1
    iuniquenodes(1) = iinodes(1)

    nunique = size(iinodes, dim=1, kind = lint)
    do i=2,nunique
!
!     if the number already exist check next
!
      if (any( iuniquenodes(1:k) == iinodes(i) )) cycle
!
!     No match found so add it to the iuniquenodes
!
         k = k + 1
         iuniquenodes(k) = iinodes(i)
    end do
    
 end subroutine unique
 
END MODULE EXPORT_INTERFACES_M
