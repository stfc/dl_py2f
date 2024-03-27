! DL_PY2F is a library for direct coupling of Python/NumPy and Fortran developed by
! You Lu and Thomas W. Keal
! STFC Daresbury Laboratory
! under licence LGPLv3
! E-mail: you.lu@stfc.ac.uk

!  Copyright (C) 2017 The authors of DL_PY2F
!
!  This file is part of DL_PY2F.
!
!  DL_PY2F is free software: you can redistribute it and/or modify
!  it under the terms of the GNU Lesser General Public License as
!  published by the Free Software Foundation, either version 3 of the
!  License, or (at your option) any later version.
!
!  DL_PY2F is distributed in the hope that it will be useful,
!  but WITHOUT ANY WARRANTY; without even the implied warranty of
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!  GNU Lesser General Public License for more details.
!
!  You should have received a copy of the GNU Lesser General Public
!  License along with DL_PY2F.  If not, see
!  <http://www.gnu.org/licenses/>.

module DL_PY2F

    use iso_c_binding

    implicit none

    private
    public  :: ptr2dict, dictType, PyType

    integer , parameter :: LENSTR = 32

    class(*), pointer :: scalarPtr => null()

    type, bind(c) :: CStructType
        ! YL 09/10/2018: components %type, %name, or %dtype of length > 1 no longer allowed by gfortran 8 or later
        character(len=1, kind=c_char) :: type(LENSTR)
        character(len=1, kind=c_char) :: name(LENSTR)
        character(len=1, kind=c_char) :: dtype(LENSTR)
        integer(c_long)               :: size
        integer(c_long)               :: sizem
        logical(c_bool)               :: isfield
        type(c_ptr)                   :: attr
    endtype CStructType
    ! Python object type: the components must be in the same order as defined in __init__.py
    ! YL: bind(c) is necessary here since PyType objects are used
    !     as arguments of c-binding procedures called by Python code;
    !     however, bind(c) and 'contains' section can NOT coexist!
    !     as a result, there is no way to apply an OO manner (e.g.,
    !     call theoryObj%py2f() nor 'extends')
    ! YL 10/02/2018: moved from py2f_datatypes.f90 and py2f_dictionary.f90, which have been deleted,
    !                to allow importing just the module DL_PY2F by the user to use the API. The reason
    !                is that PyType has to be bind(c) and so cannot be extensible at the same time.
    !                If it was not so, we could simply place it in py2f_datatypes.f90 as private_PyType and use
    !                    type, extend(private_PyType) :: PyType
    !                    endtype PyType
    !                in py2f.f90
    type, bind(c) :: PyType
        integer(c_long)   :: nattrs = 0
        type(c_ptr)       :: class_
        integer(c_long)   :: address_
        integer(c_long)   :: width
        ! YL NB 15/08/2021: on the Python side pointers are used because an error is encountered otherwise
        !                   so that debug has to be decoded by c_f_pointer()
        type(c_ptr)       :: debug
        type(CStructType) :: cdata
    endtype PyType
    ! a general-purpose dictionary-like class
    type dictType
        character(len=LENSTR)     :: type  = ""
! for now impossible to implement unlimited polymorphic arrays (see below) 
        integer                   :: sizem =  0
        integer                   :: sizen =  0
        character(len=:), pointer :: key             => null()
        class(*)        , pointer :: scalar          => null()
        class(*)        , pointer :: array(:)        => null()
        type(PyType)    , pointer :: PyPtr           => null()
        character(len=8), pointer :: onedimchar(:)   => null()
        integer(kind=8) , pointer :: onedimint(:)    => null()
        integer(kind=8) , pointer :: twodimint(:,:)  => null()
        real(kind=8)    , pointer :: onedimdbl(:)    => null()
        real(kind=8)    , pointer :: twodimdbl(:,:)  => null()
        real(kind=8)    , pointer :: onedimcdbl(:)   => null()
        real(kind=8)    , pointer :: twodimcdbl(:,:) => null()
        type(dictType)  , pointer :: next            => null()
        contains
        generic, public :: assign => assignScalar,     &
!                                     assignNull,       &
                                     assignOneDimChar, &
                                     assignOneDimDbl,  &
                                     assignTwoDimDbl,  &
                                     assignOneDimInt,  &
                                     assignTwoDimInt
        generic, public :: get    => getInt,        &
                                     getIntCLong,   &
                                     getReal,       &
                                     getDouble,     &
                                     getOneDimInt,  &
                                     getOneDimLong, &
                                     getOneDimDbl,  &
                                     getTwoDimInt,  &
                                     getTwoDimLong, &
                                     getTwoDimDbl,  &
                                     getChar,       &
                                     getCCharArray, &
                                     getLogical,    &
                                     getCLogical,   &
                                     getPyPtr,      &
!                                     getCPtr,       &
                                     getCFuncPtr
        generic, public :: set    => setOneDimDbl,  &
                                     setTwoDimDbl,  &
                                     setOneDimInt,  &
                                     setTwoDimInt,  &
                                     setOneDimLong, &
                                     setTwoDimLong, &
                                     setCCharArray, &
                                     setScalar
        ! initialiser
        procedure, public :: initialise => private_initialise
        procedure, public :: init       => private_initialise
        procedure, public :: finalise   => private_finalise
        ! assigners
        procedure, private :: assignScalar
!        procedure, private :: assignNull
        procedure, private :: assignOneDimChar
        procedure, private :: assignOneDimInt
        procedure, private :: assignTwoDimInt
        procedure, private :: assignOneDimDbl
        procedure, private :: assignTwoDimDbl
        procedure, private :: assignPyPtr
        ! getters
        ! Intel compiler requires to define the procedures line by line, rather than "procedure, private :: getInt, getReal, getChar" in a single line
        procedure, private :: getIntCLong
        procedure, private :: getInt
        procedure, private :: getReal
        procedure, private :: getDouble
        procedure, private :: getOneDimInt
        procedure, private :: getOneDimLong
        procedure, private :: getOneDimDbl
        procedure, private :: getTwoDimInt
        procedure, private :: getTwoDimLong
        procedure, private :: getTwoDimDbl
        procedure, private :: getChar
        procedure, private :: getCCharArray
        procedure, private :: getLogical
        procedure, private :: getCLogical
        procedure, private :: getPyPtr
!        procedure, private :: getCPtr
        procedure, private :: getCFuncPtr
        ! setters
        procedure, private :: setOneDimDbl
        procedure, private :: setTwoDimDbl
        procedure, private :: setOneDimInt
        procedure, private :: setTwoDimInt
        procedure, private :: setOneDimLong
        procedure, private :: setTwoDimLong
        procedure, private :: setCCharArray
        procedure, private :: setScalar
        ! pointers
!        procedure, public  :: ptr
        ! private tools
        procedure, private :: returnScalar
        procedure, private :: returnPyPtr
        procedure, private :: returnCFuncPtr
        procedure, private :: returnOneDimChar
        procedure, private :: returnOneDimInt
        procedure, private :: returnTwoDimInt
        procedure, private :: returnOneDimDbl
        procedure, private :: returnTwoDimDbl
        procedure, private :: returnSize
        ! enquiries
        procedure, public  :: keys
        procedure, public  :: enquireKey
        procedure, public  :: enquireShape
    endtype dictType

    integer    , parameter :: debug = 0
    type(c_ptr), parameter :: dummyPtr = c_null_ptr

    ! YL 20/09/2020: place the array buffers at the module level to deallocate later
!    integer(kind=8), pointer :: onedimint(:)

    contains

    function ptr2dict(metaPtr) result(dict)

        implicit none

        type(PyType)  , target , intent(in) :: metaPtr
        type(dictType)                      :: dict

        type(PyType)  , pointer             :: PyPtr

        ! ifort: must cast, otherwise error #6691: A pointer dummy argument may only be argument associated with a pointer.
        PyPtr => metaPtr

        call dict%init(PyPtr)

    endfunction ptr2dict

! INITIALISER

    subroutine private_initialise(metaObj, PyPtr, keepRef)

        class(dictType)            , intent(out)           :: metaObj
        type(PyType)      , pointer, intent(in)            :: PyPtr
        logical                    , intent(in) , optional :: keepRef

        type(CStructType) , pointer                        :: cdata(:)

        integer                                            :: i, j, ncols, sizem, sizen
        logical                                            :: keepReference

        character(len=8)  , parameter :: nonetype = "NoneType"
        type(PyType)      , pointer   :: pyptrbuff => null()
        integer(c_long)   , pointer   :: cintbuff  => null()
        type(c_funptr)    , pointer   :: cfuncbuff => null()
        integer(c_long)   , pointer   :: intarraybuff(:,:) => null(), onedimintbuff(:) => null(), twodimintbuff(:,:) => null()
        logical(c_bool)   , pointer   :: bbuff     => null()
        real(c_double)    , pointer   :: cdblbuff  => null()
        real(c_double)    , pointer   :: dblarraybuff(:,:) => null() , onedimdblbuff(:) => null() , twodimdblbuff(:,:) => null()
        character(len=LENSTR)         :: namebuff, typebuff, dtypebuff
        character(len=8)  , pointer   :: chararraybuff(:,:) => null()
        character(len=8)  , pointer   :: onedimcharbuff(:) => null(), twodimcharbuff(:,:) => null()
        ! should NOT have deferred length!
        character(len=255), pointer   :: cbuff => null()
        integer(c_long)   , pointer   :: debug_ptr

        if(.not.present(keepRef)) then
            keepReference = .true.
        endif

        ! map: cdata => PyPtr%cdata
        call c_f_pointer(c_loc(PyPtr%cdata), cdata, [PyPtr%nattrs])

        ! loop over components of Python object and set values to dictionary
        do i = 1, int(PyPtr%nattrs)

            ! YL 09/10/2018: as per gfortran 8, components %type, %name, or %dtype cannot be of length > 1
            ! (Error: Component ‘dtype’ of BIND(C) type at (1) must have length one)
            ! see: https://gcc.gnu.org/bugzilla/show_bug.cgi?id=84885
            ! so let's use array of characters
            do j = 1, LENSTR
                typebuff(j:j)  = cdata(i)%type(j)
                namebuff(j:j)  = cdata(i)%name(j)
                dtypebuff(j:j) = cdata(i)%dtype(j)
            enddo

            selectcase(trim(typebuff))

                ! YL 05/07/2023: previously "dict" type was not properly handled
                case('NoneType', 'dict')
                    ! YL 19/01/2021: assign to meet two cases
                    ! - meta%get('entry', pyptr) ... associated(ptr).eq..false. when a Python None is assigned
                    ! - meta%get('entry', intVar|realVar) doesn't crash when a Python None is assigned
                    call metaobj%assign(trim(namebuff), nonetype)
!                    call metaobj%assign(trim(namebuff), dummyPtr)

                case('function')
                    call c_f_pointer(cdata(i)%attr, cfuncbuff)
                    call metaobj%assign(trim(namebuff), cfuncbuff)

                case('object')
                    call c_f_pointer(cdata(i)%attr, pyptrbuff)
                    call metaObj%assign(trim(namebuff), pyptrbuff)

                case('int')
                    call c_f_pointer(cdata(i)%attr, cintbuff)
                    call metaObj%assign(trim(namebuff), cintbuff)

                case('float')
                    call c_f_pointer(cdata(i)%attr, cdblbuff)
                    call metaObj%assign(trim(namebuff), cdblbuff)

                case('bool')
                    call c_f_pointer(cdata(i)%attr, bbuff)
                    call metaObj%assign(trim(namebuff), bbuff)

                case('str')
                    call c_f_pointer(cdata(i)%attr, cbuff)
                    call metaObj%assign(trim(namebuff), cbuff)

                case('list')

                    ! ncols: number of array's columns (NB: ONLY _master!)
                    ! sizem: size m of the (/m, n/) array
                    ! sizen: size n of the (/m, n/) array
                    ncols = int(PyPtr%width)
                    sizem = int(cdata(i)%sizem)
                    if(cdata(i)%size.gt.0) then
                        sizen = int(cdata(i)%size/cdata(i)%sizem)
                    elseif(cdata(i)%size.eq.0) then
                        sizen = 0
                    endif

                    selectcase(trim(dtypebuff))

                        case('bytes64')

                            ! array is part of the _master array
                            if(cdata(i)%isfield) then

                                if(ncols.gt.0) then
                                    call c_f_pointer(cdata(i)%attr, chararraybuff, (/ncols, sizem/))
                                else
                                    call c_f_pointer(cdata(i)%attr, chararraybuff, (/sizen, sizem/))
                                endif
            
                                if(sizen.eq.1) then
                                    onedimcharbuff => chararraybuff(1,1:sizem)
                                    call metaObj%assign(trim(namebuff), onedimcharbuff, keepReference)
                                elseif(sizen.gt.1) then
                                    print *, ">>> DL_PY2F ERROR: 2-D char array not yet supported..."
    !                                twodimcharbuff => chararraybuff(1:sizen,:)
    !                                call metaObj%assign(trim(namebuff), twodimcharbuff, keepReference)
                                endif

                            ! array is not part of the _master array
                            else

                                ! receive the array using a (1, sizem*sizen) array
                                call c_f_pointer(cdata(i)%attr, chararraybuff, (/1, sizem*sizen/))

                                if(sizen.eq.1) then

                                    onedimcharbuff => chararraybuff(1,:)
                                    call metaObj%assign(trim(namebuff), onedimcharbuff, keepReference)

                                elseif(sizen.gt.1) then

                                    ! cast to a 2D array
                                    call c_f_pointer(c_loc(chararraybuff), twodimcharbuff, [sizen,sizem])
                                    print *, ">>> DL_PY2F ERROR: 2-D char array not yet supported..."
                                    !call metaObj%assign(trim(namebuff), twodimcharbuff, keepReference)

                                endif

                            endif
                                

                        case('float64')

                            ! array is part of the _master array
                            if(cdata(i)%isfield) then

                                if(ncols.gt.0) then
                                    call c_f_pointer(cdata(i)%attr, dblarraybuff, (/ncols, sizem/))
                                else
                                    call c_f_pointer(cdata(i)%attr, dblarraybuff, (/sizen, sizem/))
                                endif
            
                                if(sizen.eq.1) then
                                    onedimdblbuff => dblarraybuff(1,1:sizem)
                                    call metaObj%assign(trim(namebuff), onedimdblbuff, keepReference)
                                elseif(sizen.gt.1) then
                                    twodimdblbuff => dblarraybuff(1:sizen,:)
                                    call metaObj%assign(trim(namebuff), twodimdblbuff, keepReference)
                                endif

                            ! array is not part of the _master array
                            else

                                ! receive the array using a (1, sizem*sizen) array
                                call c_f_pointer(cdata(i)%attr, dblarraybuff, (/1, sizem*sizen/))

                                if(sizen.eq.1) then

                                    onedimdblbuff => dblarraybuff(1,:)
                                    call metaObj%assign(trim(namebuff), onedimdblbuff, keepReference)

                                elseif(sizen.gt.1) then

                                    ! cast to a 2D array
                                    call c_f_pointer(c_loc(dblarraybuff), twodimdblbuff, [sizen,sizem])
                                    call metaObj%assign(trim(namebuff), twodimdblbuff, keepReference)

                                endif

                            endif

                        case('int64')

                            ! array is part of the _master array
                            if(cdata(i)%isfield) then

                                if(ncols.gt.0) then
                                    call c_f_pointer(cdata(i)%attr, intarraybuff, (/ncols, sizem/))
                                else
                                    call c_f_pointer(cdata(i)%attr, intarraybuff, (/sizen, sizem/))
                                endif
            
                                if(sizen.eq.1) then
                                    onedimintbuff => intarraybuff(1,1:sizem)
                                    call metaObj%assign(trim(namebuff), onedimintbuff, keepReference)
                                elseif(sizen.gt.1) then
                                    twodimintbuff => intarraybuff(1:sizen,:)
                                    call metaObj%assign(trim(namebuff), twodimintbuff, keepReference)
                                endif

                            ! array is not part of the _master array
                            else

                                ! receive the array using a (1, sizem*sizen) array
                                call c_f_pointer(cdata(i)%attr, intarraybuff, (/1, sizem*sizen/))

                                if(sizen.eq.1) then

                                    onedimintbuff => intarraybuff(1,:)
                                    call metaObj%assign(trim(namebuff), onedimintbuff, keepReference)

                                elseif(sizen.gt.1) then

                                    ! cast to a 2D array
                                    call c_f_pointer(c_loc(intarraybuff), twodimintbuff, [sizen,sizem])
                                    call metaObj%assign(trim(namebuff), twodimintbuff, keepReference)

                                endif

                            endif

                        case default

                    endselect
                    ! YL 16/12/2023: support for structured array and record array (TODO: the latter not yet tested)
                    if(dtypebuff(1:4)=='void'.or.dtypebuff(1:6)=='record') then
                        ! array is part of the _master array
                        if(cdata(i)%isfield) then
                            if(ncols.gt.0) then
                                call c_f_pointer(cdata(i)%attr, dblarraybuff, (/ncols, sizem/))
                            else
                                call c_f_pointer(cdata(i)%attr, dblarraybuff, (/sizen, sizem/))
                            endif
                            if(sizen.eq.1) then
                                onedimdblbuff => dblarraybuff(1,1:sizem)
                                call metaObj%assign(trim(namebuff), onedimdblbuff, keepReference)
                            elseif(sizen.gt.1) then
                                twodimdblbuff => dblarraybuff(1:sizen,:)
                                call metaObj%assign(trim(namebuff), twodimdblbuff, keepReference)
                            endif
                        ! array is not part of the _master array
                        else
                            ! receive the array using a (1, sizem*sizen) array
                            call c_f_pointer(cdata(i)%attr, dblarraybuff, (/1, sizem*sizen/))
                            if(sizen.eq.1) then
                                onedimdblbuff => dblarraybuff(1,:)
                                call metaObj%assign(trim(namebuff), onedimdblbuff, keepReference)
                            elseif(sizen.gt.1) then
                                ! cast to a 2D array
                                call c_f_pointer(c_loc(dblarraybuff), twodimdblbuff, [sizen,sizem])
                                call metaObj%assign(trim(namebuff), twodimdblbuff, keepReference)
                            endif
                        endif
                    endif

                case default
                    ! YL 15/08/2021: PyPtr%debug is type(c_ptr), see above
                    call c_f_pointer(PyPtr%debug, debug_ptr)
                    if(debug_ptr.gt.2) then
                        write(*, '(/1X,A,1X,3A,1X,A/)') ">>> DL_PY2F WARNING: type",        &
                                                        '"', trim(typebuff), '"',           &
                                                        "could not be assigned for entity", &
                                                        '"', trim(namebuff), '"'
                    endif

            endselect

        enddo

! YL 19/01/2021: deallocate() segfaults
!        if(associated(pyptrbuff)     ) deallocate(pyptrbuff)
!        if(associated(cintbuff)      ) deallocate(cintbuff)
!        if(associated(cfuncbuff)     ) deallocate(cfuncbuff)
!        if(associated(intarraybuff)  ) deallocate(intarraybuff)
!        if(associated(onedimintbuff) ) deallocate(onedimintbuff)
!        if(associated(twodimintbuff) ) deallocate(twodimintbuff)
!        if(associated(bbuff)         ) deallocate(bbuff)
!        if(associated(cdblbuff)      ) deallocate(cdblbuff)
!        if(associated(dblarraybuff)  ) deallocate(dblarraybuff)
!        if(associated(onedimdblbuff) ) deallocate(onedimdblbuff)
!        if(associated(twodimdblbuff) ) deallocate(twodimdblbuff)
!        if(associated(chararraybuff) ) deallocate(chararraybuff)
!        if(associated(onedimcharbuff)) deallocate(onedimcharbuff)
!        if(associated(twodimcharbuff)) deallocate(twodimcharbuff)
!        if(associated(cbuff)         ) deallocate(cbuff)

    endsubroutine private_initialise
    recursive subroutine private_finalise(metaObj)

        class(dictType), intent(inout) :: metaObj

        ! recurse until we reach the end of the linked table
        if(associated(metaObj%key).and.associated(metaObj%next)) then

            call private_finalise(metaObj%next)

            deallocate(metaObj%next)

!            if(debug.gt.4) then
!                write(*,'(//A,1X,A)', advance="no"), ' >>> DL_PY2F: deallocating for entry', '"'//metaObj%key//'"... '
!            endif
            if(associated(metaObj%key)) then
!                if(debug.gt.4) write(*, "(1x,A)", advance="no") " key"
                deallocate(metaObj%key)
            endif
            if(associated(metaObj%scalar)) then
                selecttype(tmp=>metaObj%scalar)
                    class default
!                        if(debug.gt.4) write(*, "(1x,A)", advance="no") " scalar"
                        metaObj%scalar => null()
                endselect
            endif
            ! YL 19/01/2021: the following memory space is allocated by Python,
            !                so that can only be nullified
            if(associated(metaObj%array)) then
!                if(debug.gt.4) write(*, "(1x,A)", advance="no") " array"
                metaObj%array => null()
            endif
            if(associated(metaObj%PyPtr)) then
!                if(debug.gt.4) write(*, "(1x,A)", advance="no") " PyPtr"
                metaObj%PyPtr => null()
            endif
            if(associated(metaObj%onedimchar)) then
!                if(debug.gt.4) write(*, "(1x,A)", advance="no") " onedimchar"
                metaObj%onedimchar => null()
            endif
            if(associated(metaObj%onedimint)) then
!                if(debug.gt.4) write(*, "(1x,A)", advance="no") " onedimint"
                metaObj%onedimint  => null()
            endif
            if(associated(metaObj%twodimint)) then
!                if(debug.gt.4) write(*, "(1x,A)", advance="no") " twodimint"
                metaObj%twodimint  => null()
            endif
            if(associated(metaObj%onedimdbl)) then
!                if(debug.gt.4) write(*, "(1x,A)", advance="no") " onedimdbl"
                metaObj%onedimdbl  => null()
            endif
            if(associated(metaObj%twodimdbl)) then
!                if(debug.gt.4) write(*, "(1x,A)", advance="no") " twodimdbl"
                metaObj%twodimdbl  => null()
            endif
            if(associated(metaObj%onedimcdbl)) then
!                if(debug.gt.4) write(*, "(1x,A)", advance="no") " onedimcdbl"
                metaObj%onedimcdbl => null()
            endif
            if(associated(metaObj%twodimcdbl)) then
!                if(debug.gt.4) write(*, "(1x,A)", advance="no") " twodimcdbl"
                metaObj%twodimcdbl => null()
            endif

        ! end of linked table
        elseif(associated(metaObj%key).and..not.associated(metaObj%next)) then

!            if(debug.gt.4) then
!                write(*,'(//A,1X,A)', advance="no"), ' >>> DL_PY2F: deallocating for entry', '"'//metaObj%key//'"... '
!            endif
            if(associated(metaObj%key)) then
!                if(debug.gt.4) write(*, "(1x,A)", advance="no") " key"
                deallocate(metaObj%key)
            endif
            if(associated(metaObj%scalar)) then
                selecttype(tmp=>metaObj%scalar)
                    class default
!                        if(debug.gt.4) write(*, "(1x,A)", advance="no") " scalar"
                        metaObj%scalar => null()
                endselect
            endif
            ! YL 19/01/2021: the following memory space is allocated by Python,
            !                so that can only be nullified
            if(associated(metaObj%array)) then
!                if(debug.gt.4) write(*, "(1x,A)", advance="no") " array"
                metaObj%array => null()
            endif
            if(associated(metaObj%PyPtr)) then
!                if(debug.gt.4) write(*, "(1x,A)", advance="no") " PyPtr"
                metaObj%PyPtr => null()
            endif
            if(associated(metaObj%onedimchar)) then
!                if(debug.gt.4) write(*, "(1x,A)", advance="no") " onedimchar"
                metaObj%onedimchar => null()
            endif
            if(associated(metaObj%onedimint)) then
!                if(debug.gt.4) write(*, "(1x,A)", advance="no") " onedimint"
                metaObj%onedimint  => null()
            endif
            if(associated(metaObj%twodimint)) then
!                if(debug.gt.4) write(*, "(1x,A)", advance="no") " twodimint"
                metaObj%twodimint  => null()
            endif
            if(associated(metaObj%onedimdbl)) then
!                if(debug.gt.4) write(*, "(1x,A)", advance="no") " onedimdbl"
                metaObj%onedimdbl  => null()
            endif
            if(associated(metaObj%twodimdbl)) then
!                if(debug.gt.4) write(*, "(1x,A)", advance="no") " twodimdbl"
                metaObj%twodimdbl  => null()
            endif
            if(associated(metaObj%onedimcdbl)) then
!                if(debug.gt.4) write(*, "(1x,A)", advance="no") " onedimcdbl"
                metaObj%onedimcdbl => null()
            endif
            if(associated(metaObj%twodimcdbl)) then
!                if(debug.gt.4) write(*, "(1x,A)", advance="no") " twodimcdbl"
                metaObj%twodimcdbl => null()
            endif

        endif

        call flush(6)

    endsubroutine private_finalise

! END OF INITIALISER
! ASSIGNERS

    recursive subroutine assignScalar(metaObj, key, source)

        class(dictType) ,         intent(inout) :: metaObj
        character(len=*),         intent(in)    :: key
        class(*)        , target, intent(in)    :: source

        if(associated(metaObj%key)) then
            if(metaObj%key.eq.key) then
                deallocate(metaObj%scalar)
                metaObj%scalar => source
            else
                if(.not.associated(metaObj%next)) then
                    allocate(metaObj%next)
                endif
                call assignScalar(metaObj%next, key, source)
            endif
        else
            allocate(metaObj%key, source=key)
            metaObj%scalar => source
            if(debug.gt.5) then
                write(*,*) " DL_PY2F DEBUG: assignScalar"
                selecttype(tmp=>metaObj%scalar)
                    type is(character(kind=c_char, len=*))
                        write(*,*) " key = ", key, ", loc(metaObj%scalar) =", loc(tmp)
                    class default
                        write(*,*) ">>> DL_PY2F assignScalar: assigning character(kind=c_char, len=*)..."
                        write(*,*) "                          key = ", key
                        write(*,*) "                          loc(metaObj%scalar) =", loc(tmp)
                endselect
                write(*,*) ""
            endif
        endif

    endsubroutine assignScalar
! YL 19/01/2021: this is not distinguishable from assignScalar
!    recursive subroutine assignNull(metaObj, key, source)
!
!        class(dictType) ,          intent(inout) :: metaObj
!        character(len=*),          intent(in)    :: key
!        integer         , pointer, intent(in)    :: source
!
!        if(associated(metaObj%key)) then
!            if(metaObj%key.eq.key) then
!                deallocate(metaObj%scalar)
!                metaObj%scalar => source
!            else
!                if(.not.associated(metaObj%next)) then
!                    allocate(metaObj%next)
!                endif
!                call assignNull(metaObj%next, key, source)
!            endif
!        else
!            allocate(metaObj%key, source=key)
!            metaObj%scalar => source
!        endif
!
!    endsubroutine assignNull
! not in use
    recursive subroutine assignPyPtr(metaObj, key, source)

        class(dictType)         , intent(inout) :: metaObj
        character(len=*)        , intent(in)    :: key
        type(PyType)    , target, intent(in)    :: source

        if(associated(metaObj%key)) then
            if(metaObj%key.eq.key) then
                deallocate(metaObj%PyPtr)
                metaObj%PyPtr => source
            else
                if(.not.associated(metaObj%next)) then
                    allocate(metaObj%next)
                endif
                call assignPyPtr(metaObj%next, key, source)
            endif
        else
            allocate(metaObj%key, source=key)
            metaObj%PyPtr => source
        endif

    endsubroutine assignPyPtr
    recursive subroutine assignOneDimChar(metaObj, key, source, keepRef)

        class(dictType)          , intent(inout)           :: metaObj
        character(len=*)         , intent(in)              :: key
        character(len=8), pointer, intent(in)              :: source(:)
        logical                  , intent(in)   , optional :: keepRef

        logical                                            :: keepReference = .true.

        if(present(keepRef)) then
            keepReference = keepRef
        endif

        ! if the current obj already associated
        if(associated(metaObj%key)) then
            ! rewrite the current obj if key exists here
            if(metaObj%key.eq.key) then
                metaObj%sizem = size(source)
                deallocate(metaObj%onedimchar)
                ! will keep the reference pointing to the original Python object
                if(keepReference) then
                    metaObj%onedimchar => source
                ! will copy data and lose the reference to Python object
                else
                    allocate(metaObj%onedimchar(metaObj%sizem), source=source)
                endif
            ! move to next if this is not the key
            else
                if(.not.associated(metaObj%next)) then
                    allocate(metaObj%next)
                endif
                call assignOneDimChar(metaObj%next, key, source)
            endif
        ! create new if not associated
        else
            allocate(metaObj%key, source=key)
            metaObj%sizem = size(source)
            if(keepReference) then
                metaObj%onedimchar => source
            else
                allocate(metaObj%onedimchar(metaObj%sizem), source=source)
            endif
        endif

    endsubroutine assignOneDimChar
    recursive subroutine assignOneDimInt(metaObj, key, source, keepRef)

        class(dictType)          , intent(inout)           :: metaObj
        character(len=*)         , intent(in)              :: key
        integer(kind=8) , pointer, intent(in)              :: source(:)
        logical                  , intent(in)   , optional :: keepRef

        logical                                            :: keepReference = .true.

        if(present(keepRef)) then
            keepReference = keepRef
        endif

        ! if the current obj already associated
        if(associated(metaObj%key)) then
            ! rewrite the current obj if key exists here
            if(metaObj%key.eq.key) then
                metaObj%sizem = size(source)
                deallocate(metaObj%onedimint)
                ! will keep the reference pointing to the original Python object
                if(keepReference) then
                    metaObj%onedimint => source
                ! will copy data and lose the reference to Python object
                else
                    allocate(metaObj%onedimint(metaObj%sizem), source=source)
                endif
            ! move to next if this is not the key
            else
                if(.not.associated(metaObj%next)) then
                    allocate(metaObj%next)
                endif
                call assignOneDimInt(metaObj%next, key, source)
            endif
        ! create new if not associated
        else
            allocate(metaObj%key, source=key)
            metaObj%sizem = size(source)
            if(keepReference) then
                metaObj%onedimint => source
            else
                allocate(metaObj%onedimint(metaObj%sizem), source=source)
            endif
        endif

    endsubroutine assignOneDimInt
    recursive subroutine assignTwoDimInt(metaObj, key, source, keepRef)

        class(dictType)          , intent(inout)           :: metaObj
        character(len=*)         , intent(in)              :: key
        integer(kind=8) , pointer, intent(in)              :: source(:,:)
        logical                  , intent(in)   , optional :: keepRef

        integer                                            :: shp(2)
        logical                                            :: keepReference = .true.

        if(present(keepRef)) then
            keepReference = keepRef
        endif

        shp = (/shape(source)/)

        ! if the current obj already associated
        if(associated(metaObj%key)) then
            ! rewrite the current obj if key exists here
            if(metaObj%key.eq.key) then
                metaObj%sizem = shp(1)
                metaObj%sizen = shp(2)
                deallocate(metaObj%twodimint)
                if(keepReference) then
                    metaObj%twodimint => source
                else
                    allocate(metaObj%twodimint(metaObj%sizem,metaObj%sizen), source=source)
                endif
            ! move to next if this is not the key
            else
                if(.not.associated(metaObj%next)) then
                    allocate(metaObj%next)
                endif
                call assignTwoDimInt(metaObj%next, key, source)
            endif
        ! create new if not associated
        else
            allocate(metaObj%key, source=key)
            metaObj%sizem = shp(1)
            metaObj%sizen = shp(2)
            if(keepReference) then
                metaObj%twodimint => source
            else
                allocate(metaObj%twodimint(metaObj%sizem,metaObj%sizen), source=source)
            endif
        endif

    endsubroutine assignTwoDimInt
    recursive subroutine assignOneDimDbl(metaObj, key, source, keepRef)

        class(dictType)          , intent(inout)           :: metaObj
        character(len=*)         , intent(in)              :: key
        real(kind=8)    , pointer, intent(in)              :: source(:)
        logical                  , intent(in)   , optional :: keepRef

        logical                                            :: keepReference = .true.

        if(present(keepRef)) then
            keepReference = keepRef
        endif

        ! if the current obj already associated
        if(associated(metaObj%key)) then
            ! rewrite the current obj if key exists here
            if(metaObj%key.eq.key) then
                metaObj%sizem = size(source)
                deallocate(metaObj%onedimdbl)
                ! will keep the reference pointing to the original Python object
                if(keepReference) then
                    metaObj%onedimdbl => source
                ! will copy data and lose the reference to Python object
                ! not in use now but maybe useful in the future if we want local temporary copies
                else
                    allocate(metaObj%onedimdbl(metaObj%sizem), source=source)
                endif
            ! move to next if this is not the key
            else
                if(.not.associated(metaObj%next)) then
                    allocate(metaObj%next)
                endif
                call assignOneDimDbl(metaObj%next, key, source)
            endif
        ! create new if not associated
        else
            allocate(metaObj%key, source=key)
            metaObj%sizem = size(source)
            if(keepReference) then
                metaObj%onedimdbl => source
            else
                allocate(metaObj%onedimdbl(metaObj%sizem), source=source)
            endif
        endif

    endsubroutine assignOneDimDbl
    recursive subroutine assignTwoDimDbl(metaObj, key, source, keepRef)

        class(dictType)          , intent(inout)           :: metaObj
        character(len=*)         , intent(in)              :: key
        real(kind=8)    , pointer, intent(in)              :: source(:,:)
        logical                  , intent(in)   , optional :: keepRef

        integer                                            :: shp(2)
        logical                                            :: keepReference = .true.

        if(present(keepRef)) then
            keepReference = keepRef
        endif

        shp = (/shape(source)/)

        ! if the current obj already associated
        if(associated(metaObj%key)) then
            ! rewrite the current obj if key exists here
            if(metaObj%key.eq.key) then
                metaObj%sizem = shp(1)
                metaObj%sizen = shp(2)
                deallocate(metaObj%twodimdbl)
                if(keepReference) then
                    metaObj%twodimdbl => source
                else
                    allocate(metaObj%twodimdbl(metaObj%sizem,metaObj%sizen), source=source)
                endif
            ! move to next if this is not the key
            else
                if(.not.associated(metaObj%next)) then
                    allocate(metaObj%next)
                endif
                call assignTwoDimDbl(metaObj%next, key, source)
            endif
        ! create new if not associated
        else
            metaObj%sizem = shp(1)
            metaObj%sizen = shp(2)
            allocate(metaObj%key, source=key)
            if(keepReference) then
                metaObj%twodimdbl => source
            else
                allocate(metaObj%twodimdbl(metaObj%sizem,metaObj%sizen), source=source)
            endif
        endif

    endsubroutine assignTwoDimDbl

! END OF SETTERS
! ENQUIRIES

    recursive subroutine keys(metaObj, initialised)

        class(dictType), intent(in)           :: metaObj
        logical        , intent(in), optional :: initialised

        if(.not.present(initialised)) then
            write(*, "(/A)") " >>> DL_PY2F: The current dictionary object contains entries:"
            write(*, "(3X)", advance="no")
        endif

        if(associated(metaObj%key).and.associated(metaObj%next)) then
            write(*, "(2X,A)", advance="no") metaObj%key
            call keys(metaObj%next, .true.)
        ! end of linked table
        elseif(associated(metaObj%key).and..not.associated(metaObj%next)) then
            write(*, "(2X,A/)", advance="yes") metaObj%key
        endif

    endsubroutine keys
    ! YL 06/07/2023: we need such a function that returns .false. but doesn't throw out an error or stop the program
    recursive function enquireKey(metaObj, key) result(val)

        class(dictType) , intent(in) :: metaObj
        character(len=*), intent(in) :: key
        logical                      :: val

        if(associated(metaObj%key)) then
            if(metaObj%key.eq.trim(key)) then
                val = .true.
                return
            else
                ! end of search
                if(.not.associated(metaObj%next)) then
                    val = .false.
                    return
                else
                    ! go on trying
                    val = enquireKey(metaObj%next, key)
                endif
            endif
        ! end of search
        else
            val = .false.
            return
        endif

    endfunction enquireKey

! END OF ENQUIRIES
! TOOLS

    ! gfortran forces to declare result variable
    recursive function returnScalar(metaObj, key) result(val)

        class(dictType) , intent(in) :: metaObj
        character(len=*), intent(in) :: key
        class(*)        , pointer    :: val

        if(associated(metaObj%key)) then
            if(metaObj%key.eq.key) then
                ! YL: since gfortran/gcc 7 `val => metaObj%scalar` does not work!
!                val => metaObj%scalar
                allocate(val, source=metaObj%scalar)
                if(debug.gt.5) then
                    write(*,*) " DL_PY2F DEBUG: returnScalar"
                    selecttype(tmp=>metaObj%scalar)
                        class default
                            write(*,*) " key = ", key, ", loc(metaObj%scalar) =", loc(tmp)
                    endselect
                    write(*,*) ""
                endif
            else
                ! YL: since gfortran/gcc 7 `val => returnScalar(metaObj%next, key)` does not work!
!                val => returnScalar(metaObj%next, key)
                ! YL 06/02/2020: added printing of specific information
                if(.not.associated(metaObj%next)) then
                    print *, '>>> DL_PY2F ERROR: keyword "', key, '" not found in dictionary.'
                    print *
                    call exit(999)
                else
                    allocate(val, source=returnScalar(metaObj%next, key))
                endif
!                selecttype(tmp=>val)
!                    type is(character(*))
!                        if(trim(key)=='crystal_type') then
!                            print *, "### returnScalar: tmp =", tmp
!                        endif
!                endselect
            endif
        else
            print *, '>>> DL_PY2F ERROR: keyword \"', key, '\"not found in dictionary.'
        endif

    endfunction returnScalar
    recursive function returnCFuncPtr(metaObj, key) result(val)

        class(dictType)          , intent(in) :: metaObj
        character(len=*)         , intent(in) :: key
        type(c_funptr)  , pointer             :: val

        if(associated(metaObj%key)) then
            if(metaObj%key.eq.key) then
                call flush(6)
!                allocate(val, source=metaObj%scalar)
                val => metaObj%scalar
            else
                if(.not.associated(metaObj%next)) then
                    print *, '>>> DL_PY2F ERROR: keyword "', key, '" not found in dictionary.'
                    print *
                    call exit(999)
                else
                    val => returnCFuncPtr(metaObj%next, key)
                endif
!                val = c_loc(returnCFuncPtr(metaObj%next, key))
            endif
        else
            print *, '>>> DL_PY2F ERROR: keyword \"', key, '\"not found in dictionary.'
            call flush(6)
        endif

    endfunction returnCFuncPtr
    recursive function returnPyPtr(metaObj, key) result(val)

        class(dictType) , intent(in) :: metaObj
        character(len=*), intent(in) :: key
        type(PyType)    , pointer    :: val

        ! YL: it would be even safer if dict%type could be checked
        type(c_ptr)     , pointer    :: ptr

        if(associated(metaObj%key)) then
            if(metaObj%key.eq.key) then
                ptr => metaObj%scalar
                ! YL 18/09/18: the method of c_null_ptr does not work (randomly returning F or T when querying with c_associated) since some point (GNU 5 or 7 in Ubuntu 18?)
!                if(c_associated(ptr)) then
!                    val => metaObj%scalar
!                else
!                    val => null()
!                endif
                selecttype(tmp=>metaObj%scalar)
                    ! YL 20/01/2021: it's proven only this works well with Intel, GNU, and Cray after many attempts
                    type is(character(len=*))
                        if(tmp.eq."NoneType") then
                            val => null()
                        else
                            val => metaObj%scalar
                        endif
                    ! YL 20/01/2021: this works OK with GNU but not allowed by Intel
!                    type is(c_ptr)
!                        val => null()
                    ! YL 20/01/2021: obsolete
                    class default
                        ! Python reference
                        if(associated(metaObj%scalar)) then
                            val => metaObj%scalar
                        ! Python None
                        else
                            val => null()
                        endif
                endselect
            else
                val => returnPyPtr(metaObj%next, key)
            endif
        else
            print *, '\n>>> DL_PY2F ERROR: keyword \"', key, '\"not found in dictionary.\n'
            call flush(6)
        endif

    endfunction returnPyPtr
    recursive function returnSize(metaObj, key) result(size)

        class(dictType) , intent(in) :: metaObj
        character(len=*), intent(in) :: key
        integer                      :: size

        if(associated(metaObj%key)) then
            if(metaObj%key.eq.key) then
                size = metaObj%sizem
            else
                size = returnSize(metaObj%next, key)
            endif
        else
            print *, '>>> DL_PY2F ERROR: keyword \"', key, '\"not found in dictionary.'
        endif

    endfunction returnSize
    recursive function enquireShape(metaObj, key) result(shp)

        class(dictType) , intent(in) :: metaObj
        character(len=*), intent(in) :: key
        integer                      :: shp(2)

        if(associated(metaObj%key)) then
            call flush(6)
            if(metaObj%key.eq.trim(key)) then
                shp(1) = metaObj%sizem
                shp(2) = metaObj%sizen
            else
                shp = enquireShape(metaObj%next, key)
            endif
        else
            print *, '>>> DL_PY2F ERROR: keyword \"', key, '\"not found in dictionary.'
        endif

    endfunction enquireShape
    recursive function returnOneDimChar(metaObj, key) result(onedimchar)

        class(dictType) , intent(in) :: metaObj
        character(len=*), intent(in) :: key
        ! YL 20/09/2020: changed from pointer to allocatable so that the array gets automatically deallocated when going out of scope
        character(len=8), allocatable :: onedimchar(:)

        if(associated(metaObj%key)) then
            if(metaObj%key.eq.key) then
                allocate(onedimchar(metaObj%sizem), source=metaObj%onedimchar)
            else
                allocate(onedimchar(returnSize(metaObj%next, key)), source=returnOneDimChar(metaObj%next, key))
            endif
        else
            print *, '>>> DL_PY2F ERROR: keyword "', key, '" not found in dictionary.'
        endif

    endfunction returnOneDimChar
    recursive function returnOneDimInt(metaObj, key) result(onedimint)

        class(dictType) , intent(in)  :: metaObj
        character(len=*), intent(in)  :: key
        ! YL 20/09/2020: changed from pointer to allocatable so that the array gets automatically deallocated when going out of scope
        integer(kind=8) , allocatable :: onedimint(:)


        if(associated(metaObj%key)) then
            if(metaObj%key.eq.key) then
                allocate(onedimint(metaObj%sizem), source=metaObj%onedimint)
            else
                allocate(onedimint(returnSize(metaObj%next, key)), source=returnOneDimInt(metaObj%next, key))
            endif
        else
            print *, '>>> DL_PY2F ERROR: keyword "', key, '" not found in dictionary.'
        endif

    endfunction returnOneDimInt
    recursive function returnTwoDimInt(metaObj, key) result(twodimint)

        class(dictType) , intent(in)  :: metaObj
        character(len=*), intent(in)  :: key
        ! YL 20/09/2020: changed from pointer to allocatable so that the array gets automatically deallocated when going out of scope
        integer(kind=8) , allocatable :: twodimint(:,:)
        integer                       :: shp(2)

        if(associated(metaObj%key)) then
            if(metaObj%key.eq.key) then
                allocate(twodimint(metaObj%sizem, metaObj%sizen), source=metaObj%twodimint)
            else
                shp = enquireShape(metaObj%next, key)
                allocate(twodimint(shp(1), shp(2)), source=returnTwoDimInt(metaObj%next, key))
            endif
        else
            print *, '>>> DL_PY2F ERROR: keyword "', key, '" not found in dictionary.'
        endif

    endfunction returnTwoDimInt
    recursive function returnTwoDimDbl(metaObj, key) result(twodimdbl)

        class(dictType) , intent(in)  :: metaObj
        character(len=*), intent(in)  :: key
        ! YL 20/09/2020: changed from pointer to allocatable so that the array gets automatically deallocated when going out of scope
        real(kind=8)    , allocatable :: twodimdbl(:,:)
        integer                       :: shp(2)

        if(associated(metaObj%key)) then
            if(metaObj%key.eq.key) then
                allocate(twodimdbl(metaObj%sizem, metaObj%sizen), source=metaObj%twodimdbl)
            else
                shp = enquireShape(metaObj%next, key)
                allocate(twodimdbl(shp(1), shp(2)), source=returnTwoDimDbl(metaObj%next, key))
            endif
        else
            print *, '>>> DL_PY2F ERROR: keyword "', key, '" not found in dictionary.'
        endif

    endfunction returnTwoDimDbl
! for now impossible to implement unlimited polymorphic arrays (see below) 
    recursive function returnOneDimDbl(metaObj, key) result(onedimdbl)

        class(dictType) , intent(in)  :: metaObj
        character(len=*), intent(in)  :: key
        ! YL 20/09/2020: changed from pointer to allocatable so that the array gets automatically deallocated when going out of scope
        real(kind=8)    , allocatable :: onedimdbl(:)

        if(associated(metaObj%key)) then
            if(metaObj%key.eq.key) then
                allocate(onedimdbl(metaObj%sizem), source=metaObj%onedimdbl)
            else
                allocate(onedimdbl(returnSize(metaObj%next, key)), source=returnOneDimDbl(metaObj%next, key))
            endif
        else
            print *, '>>> DL_PY2F ERROR: keyword "', key, '" not found in dictionary.'
        endif

    endfunction returnOneDimDbl

! END OF TOOLS
! SETTERS

    ! YL 08/07/2019: in Python this can only change the value of the c_types-wrapped entity rather than the original attribute because only address of the former is passed in
    recursive subroutine setScalar(metaObj, key, val)

        class(dictType) , intent(inout) :: metaObj
        character(len=*), intent(in)    :: key
        class(*)        , intent(in)    :: val

        if(associated(metaObj%key)) then
            if(metaObj%key.eq.key) then
                selecttype(val)
                    type is(integer(kind=4))
                        selecttype(tmp=>metaObj%scalar)
                            type is(integer(kind=4))
                                tmp = int(val, kind=4)
                            type is(integer(kind=8))
                                tmp = int(val, kind=8)
                            class default
                                write(*,*) ">>> DL_PY2F ERROR: given value's type does not match the saved entry ", key
                        endselect
                    type is(integer(kind=8))
                        selecttype(tmp=>metaObj%scalar)
                            type is(integer(kind=4))
                                tmp = int(val, kind=4)
                            type is(integer(kind=8))
                                tmp = int(val, kind=8)
                            class default
                                write(*,*) ">>> DL_PY2F ERROR: given value's type does not match the saved entry ", key
                        endselect
                    type is(real(kind=8))
                        selecttype(tmp=>metaObj%scalar)
                            type is(real(kind=8))
                                tmp = int(val, kind=8)
                        endselect
                endselect
            else
                call setScalar(metaObj%next, key, val)
            endif
        else
            print *, '>>> DL_PY2F ERROR: keyword "', key, '" not found in dictionary.'
        endif

    endsubroutine setScalar
    recursive subroutine setCCharArray(metaObj, key, array)

        class(dictType)       , intent(inout) :: metaObj
        character(len=*)      , intent(in)    :: key
        character(kind=c_char), intent(in)    :: array(:)

        integer                               :: i

        if(associated(metaObj%key)) then
            if(metaObj%key.eq.key) then
                selecttype(tmp=>metaObj%scalar)
                    type is(character(kind=c_char, len=*))
                        do i = 1, size(array)
                            tmp(i:i) = array(i)
                        enddo
                endselect
            else
                call setCCharArray(metaObj%next, key, array)
            endif
        else
            print *, '>>> DL_PY2F ERROR: keyword "', key, '" not found in dictionary.'
        endif

    endsubroutine setCCharArray
    recursive subroutine setOneDimDbl(metaObj, key, array)

        class(dictType) , intent(inout) :: metaObj
        character(len=*), intent(in)    :: key
        real(kind=8)    , intent(in)    :: array(:)

        if(associated(metaObj%key)) then
            if(metaObj%key.eq.key) then
                metaObj%onedimdbl = array
            else
                call setOneDimDbl(metaObj%next, key, array)
            endif
        else
            print *, '>>> DL_PY2F ERROR: keyword "', key, '" not found in dictionary.'
        endif

    endsubroutine setOneDimDbl
    recursive subroutine setTwoDimDbl(metaObj, key, array)

        class(dictType) , intent(inout) :: metaObj
        character(len=*), intent(in)    :: key
        real(kind=8)    , intent(in)    :: array(:,:)

        if(associated(metaObj%key)) then
            if(metaObj%key.eq.key) then
                metaObj%twodimdbl = array
            else
                call setTwoDimDbl(metaObj%next, key, array)
            endif
        else
            print *, '>>> DL_PY2F ERROR: keyword "', key, '" not found in dictionary.'
        endif

    endsubroutine setTwoDimDbl
    recursive subroutine setOneDimInt(metaObj, key, array)

        class(dictType) , intent(inout) :: metaObj
        character(len=*), intent(in)    :: key
        integer(kind=4) , intent(in)    :: array(:)

        if(associated(metaObj%key)) then
            if(metaObj%key.eq.key) then
                metaObj%onedimint = array
            else
                call setOneDimInt(metaObj%next, key, array)
            endif
        else
            print *, '>>> DL_PY2F ERROR: keyword "', key, '" not found in dictionary.'
        endif

    endsubroutine setOneDimInt
    recursive subroutine setTwoDimInt(metaObj, key, array)

        class(dictType) , intent(inout) :: metaObj
        character(len=*), intent(in)    :: key
        integer(kind=4) , intent(in)    :: array(:,:)

        if(associated(metaObj%key)) then
            if(metaObj%key.eq.key) then
                metaObj%twodimint = array
            else
                call setTwoDimInt(metaObj%next, key, array)
            endif
        else
            print *, '>>> DL_PY2F ERROR: keyword "', key, '" not found in dictionary.'
        endif

    endsubroutine setTwoDimInt
    recursive subroutine setOneDimLong(metaObj, key, array)

        class(dictType) , intent(inout) :: metaObj
        character(len=*), intent(in)    :: key
        integer(kind=8) , intent(in)    :: array(:)

        if(associated(metaObj%key)) then
            if(metaObj%key.eq.key) then
                metaObj%onedimint = array
            else
                call setOneDimLong(metaObj%next, key, array)
            endif
        else
            print *, '>>> DL_PY2F ERROR: keyword "', key, '" not found in dictionary.'
        endif

    endsubroutine setOneDimLong
    recursive subroutine setTwoDimLong(metaObj, key, array)

        class(dictType) , intent(inout) :: metaObj
        character(len=*), intent(in)    :: key
        integer(kind=8) , intent(in)    :: array(:,:)

        if(associated(metaObj%key)) then
            if(metaObj%key.eq.key) then
                metaObj%twodimint = array
            else
                call setTwoDimLong(metaObj%next, key, array)
            endif
        else
            print *, '>>> DL_PY2F ERROR: keyword "', key, '" not found in dictionary.'
        endif

    endsubroutine setTwoDimLong

! END OF SETTERS
! GETTERS

    ! have to do interfaced overloading, because GNU compiler strictly requires actual argument to be polymorphic if we use class(*) as dummy argument (only intent=out)
    ! (!!!) Neither GNU nor Intel compilers distinguishes between "integer, pointer" and "integer" for overloading! But when actual argument is "integer", it cannot find the right interfaced subroutine with 'integer, pointer'!!!
    subroutine getInt(metaObj, key, val)

        class(dictType) , intent(in)  :: metaObj
        character(len=*), intent(in)  :: key
        integer(kind=4) , intent(out) :: val
  
        type(dictType)  , pointer     :: metaObjPtr

        ! Cray compiler thinks metaObj uninitialised and segfaults! so we have to allocate a temporary pointer
        allocate(metaObjPtr, source=metaObj)

        ! DANGEROUS! do NOT use local (temporary) pointer to pass value
        selecttype(tmp=>metaObjPtr%returnScalar(key))
            ! (integer and integer(c_int) are equivalent in 'selecttype')
            type is(integer(kind=4))
                ! for future development with pointers:
                !     Intel compiler requires tmp to possess target/pointer attribute,
                !     then we will have to declare tmp and cannot use "selecttype
                !     (tmp=>metaObj%returnScalar(key))" without decaration allocate(tmp, source=metaObj%returnScalar(key))
                !     so we may try val => metaObj%returnScalar(key) instead which should be fast enough
                val = tmp
            ! int type conversion (integer(kind=8) and integer(c_long) are equivalent in 'selecttype')
            type is(integer(kind=8))
                val = int(tmp)
            ! logical -> int conversion
            type is(logical)
                val = merge(1, 0, tmp)
            type is(logical(c_bool))
                val = merge(1, 0, tmp)
        endselect

        deallocate(metaObjPtr)

    endsubroutine getInt
    subroutine getIntCLong(metaObj, key, val)

        class(dictType) , intent(in)  :: metaObj
        character(len=*), intent(in)  :: key
        integer(kind=8) , intent(out) :: val
        type(dictType)  , pointer     :: metaObjPtr

        ! Cray compiler thinks metaObj uninitialised and segfaults! so we have to allocate a temporary pointer
        allocate(metaObjPtr, source=metaObj)
        
        selecttype(tmp=>metaObjPtr%returnScalar(key))
            type is(integer(kind=4))
                val = tmp
            type is(integer(kind=8))
                val = tmp
        endselect

        deallocate(metaObjPtr)

    endsubroutine getIntCLong
    subroutine getReal(metaObj, key, val)

        class(dictType) , intent(in)  :: metaObj
        character(len=*), intent(in)  :: key
        real(kind=4)    , intent(out) :: val
        type(dictType)  , pointer     :: metaObjPtr

        ! Cray compiler thinks metaObj uninitialised and segfaults! so we have to allocate a temporary pointer
        allocate(metaObjPtr, source=metaObj)

        selecttype(tmp=>metaObjPtr%returnScalar(key))
            type is(real(kind=4))
                val = tmp
            ! real type conversion (c_long and c_int are equivalent in 'selecttype')
            type is(real(kind=8))
                val = real(tmp, kind=4)
        endselect

        deallocate(metaObjPtr)

    endsubroutine getReal
    subroutine getDouble(metaObj, key, val)

        class(dictType) , intent(in)  :: metaObj
        character(len=*), intent(in)  :: key
        real(kind=8)    , intent(out) :: val

        type(dictType)  , pointer     :: metaObjPtr

        ! Cray compiler thinks metaObj uninitialised and segfaults! so we have to allocate a temporary pointer
        allocate(metaObjPtr, source=metaObj)

        selecttype(tmp=>metaObjPtr%returnScalar(key))
            type is(real(kind=4))
                val = real(tmp, kind=8)
            type is(real(kind=8))
                val = tmp
        endselect

        deallocate(metaObjPtr)

    endsubroutine getDouble
    subroutine getOneDimInt(metaObj, key, array)

        class(dictType) , intent(in)  :: metaObj
        character(len=*), intent(in)  :: key
        integer(kind=4) , intent(out) :: array(:)

        type(dictType)  , pointer     :: metaObjPtr

        ! Cray compiler thinks metaObj uninitialised and segfaults! so we have to allocate a temporary pointer
        allocate(metaObjPtr, source=metaObj)

        array = metaObjPtr%returnOneDimInt(key)

        deallocate(metaObjPtr)

    endsubroutine getOneDimInt
    subroutine getTwoDimInt(metaObj, key, array)

        class(dictType) , intent(in)  :: metaObj
        character(len=*), intent(in)  :: key
        integer(kind=4) , intent(out) :: array(:,:)

        type(dictType)  , pointer     :: metaObjPtr

        ! Cray compiler thinks metaObj uninitialised and segfaults! so we have to allocate a temporary pointer
        allocate(metaObjPtr, source=metaObj)

        array = metaObjPtr%returnTwoDimInt(key)

        deallocate(metaObjPtr)

    endsubroutine getTwoDimInt
    subroutine getOneDimLong(metaObj, key, array)

        class(dictType) , intent(in)  :: metaObj
        character(len=*), intent(in)  :: key
        integer(kind=8) , intent(out) :: array(:)

        type(dictType)  , pointer     :: metaObjPtr

        ! Cray compiler thinks metaObj uninitialised and segfaults! so we have to allocate a temporary pointer
        allocate(metaObjPtr, source=metaObj)

        array = metaObjPtr%returnOneDimInt(key)

        deallocate(metaObjPtr)

    endsubroutine getOneDimLong
    subroutine getTwoDimLong(metaObj, key, array)

        class(dictType) , intent(in)  :: metaObj
        character(len=*), intent(in)  :: key
        integer(kind=8) , intent(out) :: array(:,:)

        type(dictType)  , pointer     :: metaObjPtr

        ! Cray compiler thinks metaObj uninitialised and segfaults! so we have to allocate a temporary pointer
        allocate(metaObjPtr, source=metaObj)

        array = metaObjPtr%returnTwoDimInt(key)

        deallocate(metaObjPtr)

    endsubroutine getTwoDimLong
! doens't work for ifort, either
!    function getElement(array) result(element)
!        class(*)        , intent(in) :: array(:)
!        class(*)        , pointer    :: element
!        allocate(element, source=array(1))
!    endfunction getElement
    subroutine getOneDimDbl(metaObj, key, array)

        class(dictType) , intent(in)  :: metaObj
        character(len=*), intent(in)  :: key
        real(kind=8)    , intent(out) :: array(:)

        type(dictType)  , pointer     :: metaObjPtr

        ! Cray compiler thinks metaObj uninitialised and segfaults! so we have to allocate a temporary pointer
        allocate(metaObjPtr, source=metaObj)

        array = metaObjPtr%returnOneDimDbl(key)

        deallocate(metaObjPtr)

    endsubroutine getOneDimDbl
    subroutine getTwoDimDbl(metaObj, key, array)

        class(dictType) , intent(in)  :: metaObj
        character(len=*), intent(in)  :: key
        real(kind=8)    , intent(out) :: array(:,:)

        type(dictType)  , pointer     :: metaObjPtr

        ! Cray compiler thinks metaObj uninitialised and segfaults! so we have to allocate a temporary pointer
        allocate(metaObjPtr, source=metaObj)

        array = metaObjPtr%returnTwoDimDbl(key)

        deallocate(metaObjPtr)

    endsubroutine getTwoDimDbl
    subroutine getCCharArray(metaObj, key, array)

        class(dictType)                , intent(in)  :: metaObj
        character(len=*)               , intent(in)  :: key
        character(kind=c_char)         , intent(out) :: array(:)

        ! YL 14/08/2019: strange gfortran bug that it doesn't compile if cbuff is initialised as cbuff => null()
        character(len=:)      , pointer              :: cbuff
        integer                                      :: i
        type(dictType)  , pointer     :: metaObjPtr

        ! Cray compiler thinks metaObj uninitialised and segfaults! so we have to allocate a temporary pointer
        allocate(metaObjPtr, source=metaObj)

        call metaObjPtr%getChar(key, cbuff)

        do i = 1, len(trim(cbuff))
            array(i) = cbuff(i:i)
        enddo
        
        deallocate(metaObjPtr)

    endsubroutine getCCharArray
    subroutine getChar(metaObj, key, val)

        class(dictType)          , intent(in)  :: metaObj
        character(len=*)         , intent(in)  :: key
        character(len=:), pointer, intent(out) :: val

!        class(*)        , pointer              :: gnu, intel
        character       , target               :: blanc = ""

!        character(len=*), pointer, bind(c)              :: tmp

        ! this is a workaround before gfortran 9 is widedly used (see below)
        ! must be sequence to point to unlimited polymorphic returned by metaObj%returnScalar()
        type charType
            sequence
            ! hopefully 255 is long enough in most cases
            character(len=255) :: char
        endtype charType
        type(charType)  , pointer              :: charbuff
        type(dictType)  , pointer     :: metaObjPtr

        ! Cray compiler thinks metaObj uninitialised and segfaults! so we have to allocate a temporary pointer
        allocate(metaObjPtr, source=metaObj)

!        gnu   => blanc
!        intel => blanc

        ! we are sure metaObj%returnScalar(key) is correct, however
        ! have to use this step to temporarily hold the result in pointer tmp (see below)
        ! YL 01/03/2018: allocate(tmp, metaObj%returnScalar(key)) won't work
        ! YL 01/03/2018: gfortran 7 does not work with tmp => metaObj%returnScalar(key) and selecttype(tmp) however
        !                ifort does not work with selecttype(tmp=>metaObj%returnScalar(key))
        ! YL 08/01/2019: no longer works as per Intel 18: intel => metaObj%returnScalar(key)
        !intel => metaObj%returnScalar(key)
        ! YL 15/01/2019: however gfortran 7/8 does not compile this at all! see: https://github.com/oseledets/ttpy/issues/60
        !                it's said to be fixed in gfortran 9
!        allocate(intel, source=metaObj%returnScalar(key))
!        selecttype(tmp=>intel)
!            type is(character(*))
!                if(trim(tmp).ne."") then
!                    val => tmp
!                endif
!        endselect

        ! this is a workaround for Intel since gfortran 7/8 doesn't compile the above method
        charbuff => metaObjPtr%returnScalar(key)
        val => charbuff%char

        ! this is the GNU way
        selecttype(gnu=>metaObjPtr%returnScalar(key))
            type is(character(*))
                ! YL 19/10/2021: this will cause a runtime error when compiled with -check all or -check pointer (Intel bug?)
                if(trim(gnu).ne."") then
                    val => gnu
                endif
        endselect

        deallocate(metaObjPtr)

    endsubroutine getChar
!    subroutine getCPtr(metaObj, key, val)
!
!        class(dictType)          , intent(in)  :: metaObj
!        character(len=*)         , intent(in)  :: key
!        type(c_ptr)     , pointer, intent(out) :: val
!
!!        selecttype(tmp=>metaObj%returnScalar(key))
!!!            type is(type(c_ptr))
!!            class default
!!                val => tmp
!!        endselect
!
!    endsubroutine getCPtr
    subroutine getCFuncPtr(metaObj, key, val)

        class(dictType)          , intent(in)  :: metaObj
        character(len=*)         , intent(in)  :: key
        ! GNU segfaults at runtime if val is a pointer; while with Intel compiler val can be a pointer (val => metaObj%...)
        type(c_funptr)           , intent(out) :: val
        type(dictType)  , pointer     :: metaObjPtr

        ! Cray compiler thinks metaObj uninitialised and segfaults! so we have to allocate a temporary pointer
        allocate(metaObjPtr, source=metaObj)

!        allocate(val, source=metaObj%returnCFuncPtr(key))
        val = metaObjPtr%returnCFuncPtr(key)
!        selecttype(tmp=>metaObj%returnCFuncPtr(key))
!            class is(type(c_funptr))
!            class default
!                val => tmp
!        endselect

        deallocate(metaObjPtr)

    endsubroutine getCFuncPtr
    subroutine getLogical(metaObj, key, val)

        class(dictType) , intent(in)  :: metaObj
        character(len=*), intent(in)  :: key
        logical         , intent(out) :: val
        type(dictType)  , pointer     :: metaObjPtr

        ! Cray compiler thinks metaObj uninitialised and segfaults! so we have to allocate a temporary pointer
        allocate(metaObjPtr, source=metaObj)

        selecttype(tmp=>metaObjPtr%returnScalar(key))
            type is(logical(c_bool))
                val = merge(.true., .false., tmp)
        endselect

        deallocate(metaObjPtr)

    endsubroutine getLogical
    subroutine getCLogical(metaObj, key, val)

        class(dictType) , intent(in)  :: metaObj
        character(len=*), intent(in)  :: key
        logical(c_bool) , intent(out) :: val
        type(dictType)  , pointer     :: metaObjPtr

        ! Cray compiler thinks metaObj uninitialised and segfaults! so we have to allocate a temporary pointer
        allocate(metaObjPtr, source=metaObj)

        selecttype(tmp=>metaObjPtr%returnScalar(key))
            type is(logical(c_bool))
                val = merge(.true., .false., tmp)
        endselect

        deallocate(metaObjPtr)

    endsubroutine getCLogical
    subroutine getPyPtr(metaObj, key, val)

        class(dictType)          , intent(in)  :: metaObj
        character(len=*)         , intent(in)  :: key
        type(PyType)    , pointer, intent(out) :: val

        type(dictType)  , pointer              :: metaObjPtr
        integer ncols

        ! Cray compiler thinks metaObj uninitialised and segfaults! so we have to allocate a temporary pointer
        allocate(metaObjPtr, source=metaObj)
!        selecttype(tmp=>metaObj)
!            type is(dictType)
!                print *, "### getPyPtr: type is dictType, loc tmp =", loc(tmp)
!            class is(dictType)
!                print *, "### getPyPtr: class is dictType, loc tmp =", loc(tmp)
!            class default
!                print *, "### getPyPtr: class is default, loc tmp =", loc(tmp)
!        endselect
        val => metaObjPtr%returnPyPtr(key)

        ! some versions of GNU compiler segfaults if the Python object is None (c_ptr_null)
        if(associated(val)) then
            ncols = int(val%width)
        endif

        deallocate(metaObjPtr)

    endsubroutine getPyPtr

! END OF GETTERS
! POINTERS

!    function ptr(metaObj, key) result(val)
!
!        class(dictType)          , intent(in)  :: metaObj
!        character(len=*)         , intent(in)  :: key
!        class(*)        , pointer              :: val
!
!!        val => metaObj%returnOneDimDbl(key)
!
!    endfunction ptr

! END OF POINTERS

endmodule DL_PY2F


