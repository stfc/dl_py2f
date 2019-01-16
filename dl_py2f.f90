! DL_PY2F is a library for direct coupling of Python/NumPy and Fortran developed by
! You Lu and Thomas W. Keal
! STFC Daresbury Laboratory
! under licence LGPLv3
! E-mail: you.lu@stfc.ac.uk

!  Copyright (C) 2017 The authors of Py-ChemShell
!
!  This file is part of Py-ChemShell.
!
!  Py-ChemShell is free software: you can redistribute it and/or modify
!  it under the terms of the GNU Lesser General Public License as
!  published by the Free Software Foundation, either version 3 of the
!  License, or (at your option) any later version.
!
!  Py-ChemShell is distributed in the hope that it will be useful,
!  but WITHOUT ANY WARRANTY; without even the implied warranty of
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!  GNU Lesser General Public License for more details.
!
!  You should have received a copy of the GNU Lesser General Public
!  License along with Py-ChemShell.  If not, see
!  <http://www.gnu.org/licenses/>.

module DL_PY2F

    use iso_c_binding

    implicit none

    private
    public  :: ptr2dict, dictType, PyType

    type, bind(c) :: CStructType
        ! YL 09/10/2018: components %type, %name, or %dtype of length > 1 no longer allowed by gfortran 8 or later
        character(len=1, kind=c_char) :: type(16)
        character(len=1, kind=c_char) :: name(16)
        character(len=1, kind=c_char) :: dtype(16)
        integer(c_long)               :: size
        integer(c_long)               :: sizem
        logical(c_bool)               :: isfield
        type(c_ptr)                   :: attr
    endtype CStructType
    ! Python object type
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
        type(CStructType) :: cdata
    endtype PyType
    ! a general-purpose dictionary-like class
    type dictType
        type(dictType)  , pointer :: next     => null()
        character(len=:), pointer :: key      => null()
        character(len=16)         :: type     = ""
        class(*)        , pointer :: scalar   => null()
! for now impossible to implement unlimited polymorphic arrays (see below) 
        class(*)        , pointer :: array(:) => null()
        type(PyType)    , pointer :: PyPtr    => null()
        integer                   :: sizem    =  0
        integer                   :: sizen    =  0
        character(len=8), pointer :: onedimchar(:)
        integer(kind=8) , pointer :: onedimint(:) , twodimint(:,:)
        real(kind=8)    , pointer :: onedimdbl(:) , twodimdbl(:,:)
        real(kind=8)    , pointer :: onedimcdbl(:), twodimcdbl(:,:)
        contains
        generic, public :: assign => assignScalar,     &
                                     assignOneDimChar, &
                                     assignOneDimDbl,  &
                                     assignTwoDimDbl,  &
                                     assignOneDimInt,  &
                                     assignTwoDimInt
        generic, public :: get    => getInt,        &
                                     getIntCLong,   &
                                     getReal,       &
                                     getDouble,     &
                                     getOneDimChar, &
                                     getOneDimInt,  &
                                     getOneDimLong, &
                                     getOneDimDbl,  &
                                     getTwoDimInt,  &
                                     getTwoDimLong, &
                                     getTwoDimDbl,  &
                                     getChar,       &
                                     getBool,       &
                                     getPyPtr,      &
                                     getCPtr
        generic, public :: set    => setOneDimDbl,  &
                                     setTwoDimDbl,  &
                                     setOneDimInt,  &
                                     setTwoDimInt,  &
                                     setOneDimLong, &
                                     setTwoDimLong
        ! initialiser
        procedure, public  :: init
        ! assigners
        procedure, private :: assignScalar
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
        procedure, private :: getOneDimChar
        procedure, private :: getOneDimInt
        procedure, private :: getOneDimLong
        procedure, private :: getOneDimDbl
        procedure, private :: getTwoDimInt
        procedure, private :: getTwoDimLong
        procedure, private :: getTwoDimDbl
        procedure, private :: getChar
        procedure, private :: getBool
        procedure, private :: getPyPtr
        procedure, private :: getCPtr
        ! setters
        procedure, private :: setOneDimDbl
        procedure, private :: setTwoDimDbl
        procedure, private :: setOneDimInt
        procedure, private :: setTwoDimInt
        procedure, private :: setOneDimLong
        procedure, private :: setTwoDimLong
        ! pointers
        procedure, public  :: ptr
        ! tools
        procedure, private :: returnScalar
        procedure, private :: returnPyPtr
        procedure, private :: returnOneDimChar
        procedure, private :: returnOneDimInt
        procedure, private :: returnTwoDimInt
        procedure, private :: returnOneDimDbl
        procedure, private :: returnTwoDimDbl
        procedure, private :: returnSize
        procedure, private :: returnShape
        procedure, public  :: keys
    endtype dictType

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

    subroutine init(metaObj, PyPtr, keepRef)

        class(dictType)            , intent(out)           :: metaObj
        type(PyType)      , pointer, intent(in)            :: PyPtr
        logical                    , intent(in) , optional :: keepRef

        integer           , parameter                      :: LENSTR = 16
        type(CStructType) , pointer                        :: cdata(:)
        type(PyType)      , pointer                        :: pyptrbuff
        integer(c_long)   , pointer                        :: cintbuff
        character(len=8)  , parameter                      :: nonetype = "NoneType"
        integer(c_long)   , pointer                        :: intarraybuff(:,:) , onedimintbuff(:) , twodimintbuff(:,:)
        logical(c_bool)   , pointer                        :: bbuff
        real(c_double)    , pointer                        :: cdblbuff
        real(c_double)    , pointer                        :: dblarraybuff(:,:) , onedimdblbuff(:) , twodimdblbuff(:,:)
        real(c_double)    , pointer                        :: twodimdblbuff2(:,:), twodimdblbuff3(:,:)
        character(len=LENSTR)                              :: namebuff, typebuff, dtypebuff
        character(len=8)  , pointer                        :: chararraybuff(:,:)
        character(len=8)  , pointer                        :: onedimcharbuff(:), twodimcharbuff(:,:)
        ! should NOT have deferred length!
        character(len=255), pointer                        :: cbuff

        integer                                            :: i, j, ncols, sizem, sizen
        logical                                            :: keepReference

        if(.not.present(keepRef)) then
            keepReference = .true.
        endif

        ! map: cdata => PyPtr%cdata
        call c_f_pointer(c_loc(PyPtr%cdata), cdata, [PyPtr%nattrs])

        ! loop over components of Python object and set values to dictionary
        do i = 1, PyPtr%nattrs

            ! YL 09/10/2018: as per gfortran 8, components %type, %name, or %dtype cannot be of length > 1
            ! (Error: Component ‘dtype’ of BIND(C) type at (1) must have length one)
            ! see: https://gcc.gnu.org/bugzilla/show_bug.cgi?id=84885
            ! so let's use array of characters
            do j = 1, 16
                typebuff(j:j)  = cdata(i)%type(j)
                namebuff(j:j)  = cdata(i)%name(j)
                dtypebuff(j:j) = cdata(i)%dtype(j)
            enddo

            selectcase(trim(typebuff))

                case('NoneType')
                    call metaobj%assign(trim(namebuff), nonetype)

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
                    ncols = PyPtr%width
                    sizem = cdata(i)%sizem
                    if(cdata(i)%size.gt.0) then
                        sizen = cdata(i)%size/cdata(i)%sizem
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

            endselect

        enddo

    endsubroutine init

! END OF INITIALISER
! ASSIGNERS

    ! val shall not be pointer, otherwise the current method is not recognised as "type-bound generic subroutine"
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
        endif

    endsubroutine assignScalar
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
! TOOLS

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
    ! gfortran forces to declare result variable
    recursive function returnScalar(metaObj, key) result(val)

        class(dictType) , intent(in) :: metaObj
        character(len=*), intent(in) :: key
        class(*)        , pointer    :: val
        class(*)        , pointer              :: tmp

        if(associated(metaObj%key)) then
            if(metaObj%key.eq.key) then
                ! YL: since gfortran/gcc 7 `val => metaObj%scalar` does not work!
!                val => metaObj%scalar
                allocate(val, source=metaObj%scalar)
            else
                ! YL: since gfortran/gcc 7 `val => returnScalar(metaObj%next, key)` does not work!
!                val => returnScalar(metaObj%next, key)
                allocate(val, source=returnScalar(metaObj%next, key))
!                selecttype(tmp=>val)
!                    type is (character(*))
!                        if(trim(key)=='crystal_type') then
!                            print *, "### returnScalar: tmp =", tmp
!                        endif
!                endselect
            endif
        else
            print *, '>>> DL_PY2F ERROR: keyword \"', key, '\"not found in dictionary.'
        endif

    endfunction returnScalar
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
                    type is (character(len=*))
                        if(tmp.eq."NoneType") then
                            val => null()
                        else
                            val => metaObj%scalar
                        endif
                    class default
                        val => metaObj%scalar
                endselect
            else
                val => returnPyPtr(metaObj%next, key)
            endif
        else
            print *, '\n>>> DL_PY2F ERROR: keyword \"', key, '\"not found in dictionary.\n'
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
    recursive function returnShape(metaObj, key) result(shp)

        class(dictType) , intent(in) :: metaObj
        character(len=*), intent(in) :: key
        integer                      :: shp(2)

        if(associated(metaObj%key)) then
            if(metaObj%key.eq.key) then
                shp(1) = metaObj%sizem
                shp(2) = metaObj%sizen
            else
                shp = returnShape(metaObj%next, key)
            endif
        else
            print *, '>>> DL_PY2F ERROR: keyword \"', key, '\"not found in dictionary.'
        endif

    endfunction returnShape
    recursive function returnOneDimChar(metaObj, key) result(onedimchar)

        class(dictType) , intent(in) :: metaObj
        character(len=*), intent(in) :: key
        character(len=8), pointer    :: onedimchar(:)

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

        class(dictType) , intent(in) :: metaObj
        character(len=*), intent(in) :: key
        integer(kind=8) , pointer    :: onedimint(:)

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

        class(dictType) , intent(in) :: metaObj
        character(len=*), intent(in) :: key
        integer(kind=8) , pointer    :: twodimint(:,:)
        integer                      :: shp(2)

        if(associated(metaObj%key)) then
            if(metaObj%key.eq.key) then
                allocate(twodimint(metaObj%sizem, metaObj%sizen), source=metaObj%twodimint)
            else
                shp = returnShape(metaObj%next, key)
                allocate(twodimint(shp(1), shp(2)), source=returnTwoDimInt(metaObj%next, key))
            endif
        else
            print *, '>>> DL_PY2F ERROR: keyword "', key, '" not found in dictionary.'
        endif

    endfunction returnTwoDimInt
    recursive function returnTwoDimDbl(metaObj, key) result(twodimdbl)

        class(dictType) , intent(in) :: metaObj
        character(len=*), intent(in) :: key
        real(kind=8)    , pointer    :: twodimdbl(:,:)
        integer                      :: shp(2)

        if(associated(metaObj%key)) then
            if(metaObj%key.eq.key) then
                allocate(twodimdbl(metaObj%sizem, metaObj%sizen), source=metaObj%twodimdbl)
            else
                shp = returnShape(metaObj%next, key)
                allocate(twodimdbl(shp(1), shp(2)), source=returnTwoDimDbl(metaObj%next, key))
            endif
        else
            print *, '>>> DL_PY2F ERROR: keyword "', key, '" not found in dictionary.'
        endif

    endfunction returnTwoDimDbl
! for now impossible to implement unlimited polymorphic arrays (see below) 
    recursive function returnOneDimDbl(metaObj, key) result(onedimdbl)

        class(dictType) , intent(in) :: metaObj
        character(len=*), intent(in) :: key
!        class(*)        , pointer    :: array(:)
        real(kind=8)    , pointer    :: onedimdbl(:)

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

        ! DAGEROUS! do NOT use local (temporary) pointer to pass value
        selecttype(tmp=>metaObj%returnScalar(key))
            ! (integer and integer(c_int) are equivalent in 'selecttype')
            type is (integer(kind=4))
                ! for future development with pointers:
                !     Intel compiler requires tmp to possess target/pointer attribute,
                !     then we will have to declare tmp and cannot use "selecttype
                !     (tmp=>metaObj%returnScalar(key))" without decaration allocate(tmp, source=metaObj%returnScalar(key))
                !     so we may try val => metaObj%returnScalar(key) instead which should be fast enough
                val = tmp
            ! int type conversion (integer(kind=8) and integer(c_long) are equivalent in 'selecttype')
            type is (integer(kind=8))
                val = int(tmp)
            ! logical -> int conversion
            type is (logical)
                val = merge(1, 0, tmp)
            type is (logical(c_bool))
                val = merge(1, 0, tmp)
        endselect

    endsubroutine getInt
    subroutine getIntCLong(metaObj, key, val)

        class(dictType) , intent(in)  :: metaObj
        character(len=*), intent(in)  :: key
        integer(kind=8) , intent(out) :: val
        
        selecttype(tmp=>metaObj%returnScalar(key))
            type is (integer(kind=4))
                val = tmp
            type is (integer(kind=8))
                val = tmp
        endselect

    endsubroutine getIntCLong
    subroutine getReal(metaObj, key, val)

        class(dictType) , intent(in)  :: metaObj
        character(len=*), intent(in)  :: key
        real            , intent(out) :: val

        selecttype(tmp=>metaObj%returnScalar(key))
            type is (real)
                val = tmp
            ! real type conversion (c_long and c_int are equivalent in 'selecttype')
            type is (real(kind=8))
                val = dble(tmp)
        endselect

    endsubroutine getReal
    subroutine getDouble(metaObj, key, val)

        class(dictType) , intent(in)  :: metaObj
        character(len=*), intent(in)  :: key
        real(kind=8)    , intent(out) :: val

        selecttype(tmp=>metaObj%returnScalar(key))
            type is (real)
                val = dble(tmp)
            type is (real(kind=8))
                val = tmp
        endselect

    endsubroutine getDouble
    subroutine getOneDimChar(metaObj, key, array)

        class(dictType) , intent(in)  :: metaObj
        character(len=*), intent(in)  :: key
        character(len=8), intent(out) :: array(:)

! FIXME: gfortran doesn't compile!
!        array = metaObj%returnOneDimChar(key)

    endsubroutine getOneDimChar
    subroutine getOneDimInt(metaObj, key, array)

        class(dictType) , intent(in)  :: metaObj
        character(len=*), intent(in)  :: key
        integer(kind=4) , intent(out) :: array(:)

        array = metaObj%returnOneDimInt(key)

    endsubroutine getOneDimInt
    subroutine getTwoDimInt(metaObj, key, array)

        class(dictType) , intent(in)  :: metaObj
        character(len=*), intent(in)  :: key
        integer(kind=4) , intent(out) :: array(:,:)

        array = metaObj%returnTwoDimInt(key)

    endsubroutine getTwoDimInt
    subroutine getOneDimLong(metaObj, key, array)

        class(dictType) , intent(in)  :: metaObj
        character(len=*), intent(in)  :: key
        integer(kind=8) , intent(out) :: array(:)

        array = metaObj%returnOneDimInt(key)

    endsubroutine getOneDimLong
    subroutine getTwoDimLong(metaObj, key, array)

        class(dictType) , intent(in)  :: metaObj
        character(len=*), intent(in)  :: key
        integer(kind=8) , intent(out) :: array(:,:)

        array = metaObj%returnTwoDimInt(key)

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

        array = metaObj%returnOneDimDbl(key)

    endsubroutine getOneDimDbl
    subroutine getTwoDimDbl(metaObj, key, array)

        class(dictType) , intent(in)  :: metaObj
        character(len=*), intent(in)  :: key
        real(kind=8)    , intent(out) :: array(:,:)

        array = metaObj%returnTwoDimDbl(key)

    endsubroutine getTwoDimDbl
    subroutine getChar(metaObj, key, val)

        class(dictType)          , intent(in)  :: metaObj
        character(len=*)         , intent(in)  :: key
        character(len=:), pointer, intent(out) :: val

        class(*)        , pointer              :: gnu, intel
        character       , target               :: blanc = ""

        gnu   => blanc
        intel => blanc

        ! we are sure metaObj%returnScalar(key) is correct, however
        ! have to use this step to temporarily hold the result in pointer tmp (see below)
        ! YL 01/03/2018: allocate(tmp, metaObj%returnScalar(key)) won't work
        ! YL 01/03/2018: gfortran 7 does not work with tmp => metaObj%returnScalar(key) and selecttype(tmp) however
        !                ifort does not work with selecttype(tmp=>metaObj%returnScalar(key))
        ! YL 08/01/2019: no longer works as per Intel 18: intel => metaObj%returnScalar(key)
! YL 15/01/2019: gfortran does not compile this
!        allocate(intel, source=metaObj%returnScalar(key))
!        selecttype(intel)
!            type is (character(*))
!                if(trim(intel).ne."") then
!                    val => intel
!                endif
!        endselect
        selecttype(gnu=>metaObj%returnScalar(key))
            type is (character(*))
                if(trim(gnu).ne."") then
                    val => gnu
                endif
        endselect

    endsubroutine getChar
    subroutine getCPtr(metaObj, key, val)

        class(dictType) , intent(in)  :: metaObj
        character(len=*), intent(in)  :: key
        type(c_ptr)     , pointer, intent(out) :: val

!        selecttype(tmp=>metaObj%returnScalar(key))
!!            type is (type(c_ptr))
!            class default
!                val => tmp
!        endselect

    endsubroutine getCPtr
    subroutine getBool(metaObj, key, val)

        class(dictType) , intent(in)  :: metaObj
        character(len=*), intent(in)  :: key
        logical         , intent(out) :: val

        selecttype(tmp=>metaObj%returnScalar(key))
            type is (logical(c_bool))
                val = merge(.true., .false., tmp)
        endselect

    endsubroutine getBool
    subroutine getPyPtr(metaObj, key, val)

        class(dictType)          , intent(in)  :: metaObj
        character(len=*)         , intent(in)  :: key
        type(PyType)    , pointer, intent(out) :: val

        integer ncols

        val => metaObj%returnPyPtr(key)

        ! some versions of GNU compiler segfaults if the Python object is None (c_ptr_null)
        if(associated(val)) then
            ncols = val%width
        endif

    endsubroutine getPyPtr

! END OF GETTERS
! POINTERS

    function ptr(metaObj, key) result(val)

        class(dictType)          , intent(in)  :: metaObj
        character(len=*)         , intent(in)  :: key
        class(*)        , pointer              :: val

!        val => metaObj%returnOneDimDbl(key)

    endfunction ptr

! END OF POINTERS
!
    recursive subroutine clean(metaObj)

        class(dictType) , intent(inout) :: metaObj

    endsubroutine clean

endmodule DL_PY2F


