!  Copyright (C) 2025 The authors of DL_PY2F
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
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
!  GNU Lesser General Public License for more details.
!
!  You should have received a copy of the GNU Lesser General Public
!  License along with DL_PY2F. If not, see
!  <https://www.gnu.org/licenses/>.

! you.lu@stfc.ac.uk
module DL_PY2F
    use iso_c_binding
    implicit none
    private
    public  :: ptr2dict, dictType, PyType
    integer , parameter :: LENSTR = 32
    class(*), pointer :: scalarPtr => null()
    type, bind(c) :: CStructType
        character(len=1, kind=c_char) :: type(LENSTR)
        character(len=1, kind=c_char) :: name(LENSTR)
        character(len=1, kind=c_char) :: dtype(LENSTR)
        integer(c_long)               :: size
        integer(c_long)               :: sizem
        logical(c_bool)               :: isfield
        type(c_ptr)                   :: attr
    endtype CStructType
    type, bind(c) :: PyType
        integer(c_long)   :: nattrs = 0
        type(c_ptr)       :: class_
        integer(c_long)   :: address_
        integer(c_long)   :: width
        type(c_ptr)       :: debug
        type(CStructType) :: cdata
    endtype PyType
    type dictType
        character(len=LENSTR)     :: type  = ""
        integer                   :: sizem =  0
        integer                   :: sizen =  0
        character(len=:), pointer :: key             => null()
        class(*)        , pointer :: scalar          => null()
        class(*)        , pointer :: array(:)        => null()
        type(PyType)    , pointer :: PyPtr           => null()
        character(len=8), pointer :: onedimchar(:)   => null()
        integer(kind=4) , pointer :: onedimint(:)    => null()
        integer(kind=4) , pointer :: twodimint(:,:)  => null()
        integer(kind=8) , pointer :: onedimlng(:)    => null()
        integer(kind=8) , pointer :: twodimlng(:,:)  => null()
        real(kind=8)    , pointer :: onedimdbl(:)    => null()
        real(kind=8)    , pointer :: twodimdbl(:,:)  => null()
        real(kind=8)    , pointer :: onedimcdbl(:)   => null()
        real(kind=8)    , pointer :: twodimcdbl(:,:) => null()
        type(dictType)  , pointer :: next            => null()
        contains
        generic, public :: assign => assignScalar,     &
                                     assignOneDimChar, &
                                     assignOneDimDbl,  &
                                     assignTwoDimDbl,  &
                                     assignOneDimInt,  &
                                     assignTwoDimInt,  &
                                     assignOneDimLng,  &
                                     assignTwoDimLng
        generic, public :: get    => getInt,        &
                                     getIntCLong,   &
                                     getReal,       &
                                     getDouble,     &
                                     getOneDimDbl,  &
                                     getTwoDimDbl,  &
                                     getOneDimInt,  &
                                     getTwoDimInt,  &
                                     getOneDimLng,  &
                                     getTwoDimLng,  &
                                     getChar,       &
                                     getCCharArray, &
                                     getLogical,    &
                                     getCLogical,   &
                                     getPyPtr,      &
                                     getCFuncPtr
        generic, public :: set    => setOneDimDbl,  &
                                     setTwoDimDbl,  &
                                     setOneDimInt,  &
                                     setTwoDimInt,  &
                                     setOneDimLng,  &
                                     setTwoDimLng,  &
                                     setCCharArray, &
                                     setScalar
        procedure, public :: initialise => private_initialise
        procedure, public :: init       => private_initialise
        procedure, public :: finalise   => private_finalise
        procedure, private :: assignScalar
        procedure, private :: assignOneDimChar
        procedure, private :: assignOneDimDbl
        procedure, private :: assignTwoDimDbl
        procedure, private :: assignOneDimInt
        procedure, private :: assignTwoDimInt
        procedure, private :: assignOneDimLng
        procedure, private :: assignTwoDimLng
        procedure, private :: assignPyPtr
        procedure, private :: getIntCLong
        procedure, private :: getInt
        procedure, private :: getReal
        procedure, private :: getDouble
        procedure, private :: getOneDimDbl
        procedure, private :: getTwoDimDbl
        procedure, private :: getOneDimInt
        procedure, private :: getTwoDimInt
        procedure, private :: getOneDimLng
        procedure, private :: getTwoDimLng
        procedure, private :: getChar
        procedure, private :: getCCharArray
        procedure, private :: getLogical
        procedure, private :: getCLogical
        procedure, private :: getPyPtr
        procedure, private :: getCFuncPtr
        procedure, private :: setOneDimDbl
        procedure, private :: setTwoDimDbl
        procedure, private :: setOneDimInt
        procedure, private :: setTwoDimInt
        procedure, private :: setOneDimLng
        procedure, private :: setTwoDimLng
        procedure, private :: setCCharArray
        procedure, private :: setScalar
        procedure, private :: returnScalar
        procedure, private :: returnPyPtr
        procedure, private :: returnCFuncPtr
        procedure, private :: returnOneDimDbl
        procedure, private :: returnTwoDimDbl
        procedure, private :: returnOneDimInt
        procedure, private :: returnTwoDimInt
        procedure, private :: returnOneDimLng
        procedure, private :: returnTwoDimLng
        procedure, private :: returnOneDimChar
        procedure, private :: returnSize
        procedure, public  :: keys
        procedure, public  :: enquireKey
        procedure, public  :: enquireShape
        procedure, public  :: enquireType
    endtype dictType
    integer    , parameter :: debug = 0
    type(c_ptr), parameter :: dummyPtr = c_null_ptr
    contains
    function ptr2dict(metaPtr) result(dict)
        implicit none
        type(PyType)  , target , intent(in) :: metaPtr
        type(dictType)                      :: dict
        type(PyType)  , pointer             :: PyPtr
        PyPtr => metaPtr
        call dict%init(PyPtr)
    endfunction ptr2dict
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
        integer(c_int)    , pointer   :: intarraybuff(:,:) => null(), onedimintbuff(:) => null(), twodimintbuff(:,:) => null()
        integer(c_long)   , pointer   :: lngarraybuff(:,:) => null(), onedimlngbuff(:) => null(), twodimlngbuff(:,:) => null()
        logical(c_bool)   , pointer   :: bbuff     => null()
        real(c_double)    , pointer   :: cdblbuff  => null()
        real(c_double)    , pointer   :: dblarraybuff(:,:) => null() , onedimdblbuff(:) => null() , twodimdblbuff(:,:) => null()
        character(len=8)  , pointer   :: chararraybuff(:,:) => null()
        character(len=8)  , pointer   :: onedimcharbuff(:) => null(), twodimcharbuff(:,:) => null()
        character(len=255), pointer   :: cbuff => null()
        integer(c_long)   , pointer   :: debug_ptr => null()
        character(len=LENSTR)         :: namebuff, typebuff, dtypebuff
        if(.not.present(keepRef)) then
            keepReference = .true.
        endif
        call c_f_pointer(c_loc(PyPtr%cdata), cdata, [PyPtr%nattrs])
        do i = 1, int(PyPtr%nattrs)
            do j = 1, LENSTR
                typebuff(j:j)  = cdata(i)%type(j)
                namebuff(j:j)  = cdata(i)%name(j)
                dtypebuff(j:j) = cdata(i)%dtype(j)
            enddo
            selectcase(trim(typebuff))
                case('NoneType', 'dict')
                    call metaobj%assign(trim(namebuff), nonetype)
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
                    ncols = int(PyPtr%width)
                    sizem = int(cdata(i)%sizem)
                    if(cdata(i)%size.gt.0) then
                        sizen = int(cdata(i)%size/cdata(i)%sizem)
                    elseif(cdata(i)%size.eq.0) then
                        sizen = 0
                    endif
                    selectcase(trim(dtypebuff))
                        case('bytes64')
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
                                endif
                            else
                                call c_f_pointer(cdata(i)%attr, chararraybuff, (/1, sizem*sizen/))
                                if(sizen.eq.1) then
                                    onedimcharbuff => chararraybuff(1,:)
                                    call metaObj%assign(trim(namebuff), onedimcharbuff, keepReference)
                                elseif(sizen.gt.1) then
                                    call c_f_pointer(c_loc(chararraybuff), twodimcharbuff, [sizen,sizem])
                                    print *, ">>> DL_PY2F ERROR: 2-D char array not yet supported..."
                                endif
                            endif
                        case('float64')
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
                            else
                                call c_f_pointer(cdata(i)%attr, dblarraybuff, (/1, sizem*sizen/))
                                if(sizen.eq.1) then
                                    onedimdblbuff => dblarraybuff(1,:)
                                    call metaObj%assign(trim(namebuff), onedimdblbuff, keepReference)
                                elseif(sizen.gt.1) then
                                    call c_f_pointer(c_loc(dblarraybuff), twodimdblbuff, [sizen,sizem])
                                    call metaObj%assign(trim(namebuff), twodimdblbuff, keepReference)
                                endif
                            endif
                        case('int32')
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
                            else
                                call c_f_pointer(cdata(i)%attr, intarraybuff, (/1, sizem*sizen/))
                                if(sizen.eq.1) then
                                    onedimintbuff => intarraybuff(1,:)
                                    call metaObj%assign(trim(namebuff), onedimintbuff, keepReference)
                                elseif(sizen.gt.1) then
                                    call c_f_pointer(c_loc(intarraybuff), twodimintbuff, [sizen,sizem])
                                    call metaObj%assign(trim(namebuff), twodimintbuff, keepReference)
                                endif
                            endif
                        case('int64')
                            if(cdata(i)%isfield) then
                                if(ncols.gt.0) then
                                    call c_f_pointer(cdata(i)%attr, lngarraybuff, (/ncols, sizem/))
                                else
                                    call c_f_pointer(cdata(i)%attr, lngarraybuff, (/sizen, sizem/))
                                endif
                                if(sizen.eq.1) then
                                    onedimlngbuff => lngarraybuff(1,1:sizem)
                                    call metaObj%assign(trim(namebuff), onedimlngbuff, keepReference)
                                elseif(sizen.gt.1) then
                                    twodimlngbuff => lngarraybuff(1:sizen,:)
                                    call metaObj%assign(trim(namebuff), twodimlngbuff, keepReference)
                                endif
                            else
                                call c_f_pointer(cdata(i)%attr, lngarraybuff, (/1, sizem*sizen/))
                                if(sizen.eq.1) then
                                    onedimlngbuff => lngarraybuff(1,:)
                                    call metaObj%assign(trim(namebuff), onedimlngbuff, keepReference)
                                elseif(sizen.gt.1) then
                                    call c_f_pointer(c_loc(lngarraybuff), twodimlngbuff, [sizen,sizem])
                                    call metaObj%assign(trim(namebuff), twodimlngbuff, keepReference)
                                endif
                            endif
                        case default
                    endselect
                    if(dtypebuff(1:4)=='void'.or.dtypebuff(1:6)=='record') then
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
                        else
                            call c_f_pointer(cdata(i)%attr, dblarraybuff, (/1, sizem*sizen/))
                            if(sizen.eq.1) then
                                onedimdblbuff => dblarraybuff(1,:)
                                call metaObj%assign(trim(namebuff), onedimdblbuff, keepReference)
                            elseif(sizen.gt.1) then
                                call c_f_pointer(c_loc(dblarraybuff), twodimdblbuff, [sizen,sizem])
                                call metaObj%assign(trim(namebuff), twodimdblbuff, keepReference)
                            endif
                        endif
                    endif
                case default
                    call c_f_pointer(PyPtr%debug, debug_ptr)
                    if(debug_ptr.gt.2) then
                        write(*, '(/1X,A,1X,3A,1X,A/)') ">>> DL_PY2F WARNING: type",        &
                                                        '"', trim(typebuff), '"',           &
                                                        "could not be assigned for entity", &
                                                        '"', trim(namebuff), '"'
                    endif
            endselect
        enddo
        chararraybuff  => null()
        intarraybuff   => null()
        lngarraybuff   => null()
        dblarraybuff   => null()
        onedimintbuff  => null()
        twodimintbuff  => null()
        onedimlngbuff  => null()
        twodimlngbuff  => null()
        onedimdblbuff  => null()
        twodimdblbuff  => null()
        pyptrbuff      => null()
        cintbuff       => null()
        cfuncbuff      => null()
        bbuff          => null()
        cdblbuff       => null()
        onedimcharbuff => null()
        twodimcharbuff => null()
        cbuff          => null()
        debug_ptr      => null()
    endsubroutine private_initialise
    recursive subroutine private_finalise(metaObj)
        class(dictType), intent(inout) :: metaObj
        if(associated(metaObj%key).and.associated(metaObj%next)) then
            call private_finalise(metaObj%next)
            deallocate(metaObj%next)
            if(associated(metaObj%key)) then
                deallocate(metaObj%key)
            endif
            selecttype(tmp=>metaObj%scalar)
                class default
                    metaObj%scalar => null()
            endselect
            metaObj%array => null()
            metaObj%PyPtr => null()
            metaObj%onedimchar => null()
            metaObj%onedimint  => null()
            metaObj%twodimint  => null()
            metaObj%onedimlng  => null()
            metaObj%twodimlng  => null()
            metaObj%onedimdbl  => null()
            metaObj%twodimdbl  => null()
            metaObj%onedimcdbl => null()
            metaObj%twodimcdbl => null()
        elseif(associated(metaObj%key).and..not.associated(metaObj%next)) then
            if(associated(metaObj%key)) then
                deallocate(metaObj%key)
            endif
            selecttype(tmp=>metaObj%scalar)
                class default
                    metaObj%scalar => null()
            endselect
            metaObj%array => null()
            metaObj%PyPtr => null()
            metaObj%onedimchar => null()
            metaObj%onedimint  => null()
            metaObj%twodimint  => null()
            metaObj%onedimlng  => null()
            metaObj%twodimlng  => null()
            metaObj%onedimdbl  => null()
            metaObj%twodimdbl  => null()
            metaObj%onedimcdbl => null()
            metaObj%twodimcdbl => null()
        endif
        call flush(6)
    endsubroutine private_finalise
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
        logical                                            :: keepReference
        keepReference = .true.
        if(present(keepRef)) then
            keepReference = keepRef
        endif
        if(associated(metaObj%key)) then
            if(metaObj%key.eq.key) then
                metaObj%sizem = size(source)
                deallocate(metaObj%onedimchar)
                if(keepReference) then
                    metaObj%onedimchar => source
                else
                    allocate(metaObj%onedimchar(metaObj%sizem), source=source)
                endif
            else
                if(.not.associated(metaObj%next)) then
                    allocate(metaObj%next)
                endif
                call assignOneDimChar(metaObj%next, key, source, keepReference)
            endif
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
        integer(kind=4) , pointer, intent(in)              :: source(:)
        logical                  , intent(in)   , optional :: keepRef
        logical                                            :: keepReference
        keepReference = .true.
        if(present(keepRef)) then
            keepReference = keepRef
        endif
        if(associated(metaObj%key)) then
            if(metaObj%key.eq.key) then
                metaObj%sizem = size(source)
                deallocate(metaObj%onedimint)
                if(keepReference) then
                    metaObj%onedimint => source
                else
                    allocate(metaObj%onedimint(metaObj%sizem), source=source)
                endif
            else
                if(.not.associated(metaObj%next)) then
                    allocate(metaObj%next)
                endif
                call assignOneDimInt(metaObj%next, key, source, keepReference)
            endif
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
        integer(kind=4) , pointer, intent(in)              :: source(:,:)
        logical                  , intent(in)   , optional :: keepRef
        integer                                            :: shp(2)
        logical                                            :: keepReference
        keepReference = .true.
        if(present(keepRef)) then
            keepReference = keepRef
        endif
        shp = (/shape(source)/)
        if(associated(metaObj%key)) then
            if(metaObj%key.eq.key) then
                metaObj%sizem = shp(1)
                metaObj%sizen = shp(2)
                deallocate(metaObj%twodimint)
                if(keepReference) then
                    metaObj%twodimint => source
                else
                    allocate(metaObj%twodimint(metaObj%sizem,metaObj%sizen), source=source)
                endif
            else
                if(.not.associated(metaObj%next)) then
                    allocate(metaObj%next)
                endif
                call assignTwoDimInt(metaObj%next, key, source, keepReference)
            endif
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
    recursive subroutine assignOneDimLng(metaObj, key, source, keepRef)
        class(dictType)          , intent(inout)           :: metaObj
        character(len=*)         , intent(in)              :: key
        integer(kind=8) , pointer, intent(in)              :: source(:)
        logical                  , intent(in)   , optional :: keepRef
        logical                                            :: keepReference
        keepReference = .true.
        if(present(keepRef)) then
            keepReference = keepRef
        endif
        if(associated(metaObj%key)) then
            if(metaObj%key.eq.key) then
                metaObj%sizem = size(source)
                deallocate(metaObj%onedimlng)
                if(keepReference) then
                    metaObj%onedimlng => source
                else
                    allocate(metaObj%onedimlng(metaObj%sizem), source=source)
                endif
            else
                if(.not.associated(metaObj%next)) then
                    allocate(metaObj%next)
                endif
                call assignOneDimLng(metaObj%next, key, source, keepReference)
            endif
        else
            allocate(metaObj%key, source=key)
            metaObj%sizem = size(source)
            if(keepReference) then
                metaObj%onedimlng => source
            else
                allocate(metaObj%onedimlng(metaObj%sizem), source=source)
            endif
        endif
    endsubroutine assignOneDimLng
    recursive subroutine assignTwoDimLng(metaObj, key, source, keepRef)
        class(dictType)          , intent(inout)           :: metaObj
        character(len=*)         , intent(in)              :: key
        integer(kind=8) , pointer, intent(in)              :: source(:,:)
        logical                  , intent(in)   , optional :: keepRef
        integer                                            :: shp(2)
        logical                                            :: keepReference
        keepReference = .true.
        if(present(keepRef)) then
            keepReference = keepRef
        endif
        shp = (/shape(source)/)
        if(associated(metaObj%key)) then
            if(metaObj%key.eq.key) then
                metaObj%sizem = shp(1)
                metaObj%sizen = shp(2)
                deallocate(metaObj%twodimlng)
                if(keepReference) then
                    metaObj%twodimlng => source
                else
                    allocate(metaObj%twodimlng(metaObj%sizem,metaObj%sizen), source=source)
                endif
            else
                if(.not.associated(metaObj%next)) then
                    allocate(metaObj%next)
                endif
                call assignTwoDimLng(metaObj%next, key, source, keepReference)
            endif
        else
            allocate(metaObj%key, source=key)
            metaObj%sizem = shp(1)
            metaObj%sizen = shp(2)
            if(keepReference) then
                metaObj%twodimlng => source
            else
                allocate(metaObj%twodimlng(metaObj%sizem,metaObj%sizen), source=source)
            endif
        endif
    endsubroutine assignTwoDimLng
    recursive subroutine assignOneDimDbl(metaObj, key, source, keepRef)
        class(dictType)          , intent(inout)           :: metaObj
        character(len=*)         , intent(in)              :: key
        real(kind=8)    , pointer, intent(in)              :: source(:)
        logical                  , intent(in)   , optional :: keepRef
        logical                                            :: keepReference
        keepReference = .true.
        if(present(keepRef)) then
            keepReference = keepRef
        endif
        if(associated(metaObj%key)) then
            if(metaObj%key.eq.key) then
                metaObj%sizem = size(source)
                deallocate(metaObj%onedimdbl)
                if(keepReference) then
                    metaObj%onedimdbl => source
                else
                    allocate(metaObj%onedimdbl(metaObj%sizem), source=source)
                endif
            else
                if(.not.associated(metaObj%next)) then
                    allocate(metaObj%next)
                endif
                call assignOneDimDbl(metaObj%next, key, source, keepReference)
            endif
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
        logical                                            :: keepReference
        keepReference = .true.
        if(present(keepRef)) then
            keepReference = keepRef
        endif
        shp = (/shape(source)/)
        if(associated(metaObj%key)) then
            if(metaObj%key.eq.key) then
                metaObj%sizem = shp(1)
                metaObj%sizen = shp(2)
                deallocate(metaObj%twodimdbl)
                if(keepReference) then
                    metaObj%twodimdbl => source
                else
                    allocate(metaObj%twodimdbl(metaObj%sizem,metaObj%sizen), source=source)
                endif
            else
                if(.not.associated(metaObj%next)) then
                    allocate(metaObj%next)
                endif
                call assignTwoDimDbl(metaObj%next, key, source, keepReference)
            endif
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
        elseif(associated(metaObj%key).and..not.associated(metaObj%next)) then
            write(*, "(2X,A/)", advance="yes") metaObj%key
        endif
    endsubroutine keys
    recursive function enquireKey(metaObj, key) result(val)
        class(dictType) , intent(in) :: metaObj
        character(len=*), intent(in) :: key
        logical                      :: val
        if(associated(metaObj%key)) then
            if(metaObj%key.eq.trim(key)) then
                val = .true.
                return
            else
                if(.not.associated(metaObj%next)) then
                    val = .false.
                    return
                else
                    val = enquireKey(metaObj%next, key)
                endif
            endif
        else
            val = .false.
            return
        endif
    endfunction enquireKey
    recursive function returnScalar(metaObj, key) result(val)
        class(dictType) , intent(in) :: metaObj
        character(len=*), intent(in) :: key
        class(*)        , pointer    :: val
        if(associated(metaObj%key)) then
            if(metaObj%key.eq.key) then
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
                if(.not.associated(metaObj%next)) then
                    print *, '>>> DL_PY2F ERROR: keyword "', key, '" not found in dictionary.'
                    print *
                    call exit(999)
                else
                    allocate(val, source=returnScalar(metaObj%next, key))
                endif
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
                val => metaObj%scalar
            else
                if(.not.associated(metaObj%next)) then
                    print *, '>>> DL_PY2F ERROR: keyword "', key, '" not found in dictionary.'
                    print *
                    call exit(999)
                else
                    val => returnCFuncPtr(metaObj%next, key)
                endif
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
        type(c_ptr)     , pointer    :: ptr
        if(associated(metaObj%key)) then
            if(metaObj%key.eq.key) then
                ptr => metaObj%scalar
                selecttype(tmp=>metaObj%scalar)
                    type is(character(len=*))
                        if(tmp.eq."NoneType") then
                            val => null()
                        else
                            val => metaObj%scalar
                        endif
                    class default
                        if(associated(metaObj%scalar)) then
                            val => metaObj%scalar
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
    recursive function enquireType(metaObj, key) result(type)
        class(dictType) , intent(in) :: metaObj
        character(len=*), intent(in) :: key
        character(len=16)            :: type
        if(associated(metaObj%key)) then
            call flush(6)
            if(metaObj%key.eq.trim(key)) then
                if(associated(metaObj%onedimchar)) type = 'character'
                ! MS 22/01/2025: renamed all "dimint" as "dimlng" because they are actually 64-bit
                !                and 32-bit integers are yet to be supported (TODO)
                if(associated(metaObj%onedimlng))  type = 'long'
                if(associated(metaObj%twodimlng))  type = 'long'
                if(associated(metaObj%onedimdbl))  type = 'double'
                if(associated(metaObj%twodimdbl))  type = 'double'
                if(associated(metaObj%onedimcdbl)) type = 'c_double'
                if(associated(metaObj%twodimcdbl)) type = 'c_double'
                ! MS 21/01/2025: added support for scalars
                if(metaObj%sizem.eq.0.and.metaObj%sizen.eq.0) then
                    selecttype(tmp=>metaObj%scalar)
                        type is(integer(kind=4))
                            type = 'integer'
                        type is(integer(kind=8))
                            type = 'long'
                        type is(real(kind=4))
                            type = 'float'
                        type is(real(kind=8))
                            type = 'double'
                        ! MS TODO: to support more types
                        class default
                            type = 'unknown'
                    endselect
                endif
            else
                type = enquireType(metaObj%next, key)
            endif
        else
            print *, '>>> DL_PY2F ERROR: keyword \"', key, '\"not found in dictionary.'
        endif
    endfunction enquireType
    recursive function returnOneDimChar(metaObj, key) result(onedimchar)
        class(dictType) , intent(in) :: metaObj
        character(len=*), intent(in) :: key
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
    recursive function returnOneDimInt(metaObj, key, keepRef) result(onedimint)
        class(dictType)          , intent(in)           :: metaObj
        character(len=*)         , intent(in)           :: key
        integer(kind=4) , pointer                       :: onedimint(:)
        logical                  , intent(in), optional :: keepRef
        logical                                         :: keepReference
        keepReference = .true.
        if(present(keepRef)) then
            keepReference = keepRef
        endif
        if(associated(metaObj%key)) then
            if(metaObj%key.eq.key) then
                if(keepReference) then
                    onedimint => metaObj%onedimint
                else
                    allocate(onedimint(metaObj%sizem), source=metaObj%onedimint)
                endif
            else
                if(keepReference) then
                    onedimint => returnOneDimInt(metaObj%next, key, keepReference)
                else
                    allocate(onedimint(returnSize(metaObj%next, key)), source=returnOneDimInt(metaObj%next, key, keepReference))
                endif
            endif
        else
            print *, '>>> DL_PY2F ERROR: keyword "', key, '" not found in dictionary.'
        endif
    endfunction returnOneDimInt
    recursive function returnTwoDimInt(metaObj, key, keepRef) result(twodimint)
        class(dictType)          , intent(in)           :: metaObj
        character(len=*)         , intent(in)           :: key
        integer(kind=4) , pointer                       :: twodimint(:,:)
        logical                  , intent(in), optional :: keepRef
        integer                                         :: shp(2)
        logical                                         :: keepReference
        if(present(keepRef)) then
            keepReference = keepRef
        endif
        if(associated(metaObj%key)) then
            if(metaObj%key.eq.key) then
                if(keepReference) then
                    twodimint => metaObj%twodimint
                else
                    allocate(twodimint(metaObj%sizem, metaObj%sizen), source=metaObj%twodimint)
                endif
            else
                if(keepReference) then
                    twodimint => returnTwoDimInt(metaObj%next, key, keepReference)
                else
                    shp = enquireShape(metaObj%next, key)
                    allocate(twodimint(shp(1), shp(2)), source=returnTwoDimInt(metaObj%next, key, keepReference))
                endif
            endif
        else
            print *, '>>> DL_PY2F ERROR: keyword "', key, '" not found in dictionary.'
        endif
    endfunction returnTwoDimInt
    recursive function returnOneDimLng(metaObj, key, keepRef) result(onedimlng)
        class(dictType)          , intent(in)           :: metaObj
        character(len=*)         , intent(in)           :: key
        integer(kind=8) , pointer                       :: onedimlng(:)
        logical                  , intent(in), optional :: keepRef
        logical                                         :: keepReference
        if(present(keepRef)) then
            keepReference = keepRef
        endif
        if(associated(metaObj%key)) then
            if(metaObj%key.eq.key) then
                if(keepReference) then
                    onedimlng => metaObj%onedimlng
                else
                    allocate(onedimlng(metaObj%sizem), source=metaObj%onedimlng)
                endif
            else
                if(keepReference) then
                    onedimlng => returnOneDimLng(metaObj%next, key, keepReference)
                else
                    allocate(onedimlng(returnSize(metaObj%next, key)), source=returnOneDimLng(metaObj%next, key, keepReference))
                endif
            endif
        else
            print *, '>>> DL_PY2F ERROR: keyword "', key, '" not found in dictionary.'
        endif
    endfunction returnOneDimLng
    recursive function returnTwoDimLng(metaObj, key, keepRef) result(twodimlng)
        class(dictType)          , intent(in)           :: metaObj
        character(len=*)         , intent(in)           :: key
        integer(kind=8) , pointer                       :: twodimlng(:,:)
        logical                  , intent(in), optional :: keepRef
        integer                                         :: shp(2)
        logical                                         :: keepReference
        if(present(keepRef)) then
            keepReference = keepRef
        endif
        if(associated(metaObj%key)) then
            if(metaObj%key.eq.key) then
                if(keepReference) then
                    twodimlng => metaObj%twodimlng
                else
                    allocate(twodimlng(metaObj%sizem, metaObj%sizen), source=metaObj%twodimlng)
                endif
            else
                if(keepReference) then
                    twodimlng => returnTwoDimLng(metaObj%next, key, keepReference)
                else
                    shp = enquireShape(metaObj%next, key)
                    allocate(twodimlng(shp(1), shp(2)), source=returnTwoDimLng(metaObj%next, key, keepReference))
                endif
            endif
        else
            print *, '>>> DL_PY2F ERROR: keyword "', key, '" not found in dictionary.'
        endif
    endfunction returnTwoDimLng
    recursive function returnOneDimDbl(metaObj, key, keepRef) result(onedimdbl)
        class(dictType)          , intent(in)           :: metaObj
        character(len=*)         , intent(in)           :: key
        real(kind=8)    , pointer                       :: onedimdbl(:)
        logical                  , intent(in), optional :: keepRef
        logical                                         :: keepReference
        if(present(keepRef)) then
            keepReference = keepRef
        endif
        if(associated(metaObj%key)) then
            if(metaObj%key.eq.key) then
                if(keepReference) then
                    onedimdbl => metaObj%onedimdbl
                else
                    allocate(onedimdbl(metaObj%sizem), source=metaObj%onedimdbl)
                endif
            else
                if(keepReference) then
                    onedimdbl => returnOneDimDbl(metaObj%next, key, keepReference)
                else
                    allocate(onedimdbl(returnSize(metaObj%next, key)), source=returnOneDimDbl(metaObj%next, key, keepReference))
                endif
            endif
        else
            print *, '>>> DL_PY2F ERROR: keyword "', key, '" not found in dictionary.'
        endif
    endfunction returnOneDimDbl
    recursive function returnTwoDimDbl(metaObj, key, keepRef) result(twodimdbl)
        class(dictType)          , intent(in)           :: metaObj
        character(len=*)         , intent(in)           :: key
        real(kind=8)    , pointer                       :: twodimdbl(:,:)
        logical                  , intent(in), optional :: keepRef
        integer                                         :: shp(2)
        logical                                         :: keepReference
        if(present(keepRef)) then
            keepReference = keepRef
        endif
        if(associated(metaObj%key)) then
            if(metaObj%key.eq.key) then
                if(keepReference) then
                    twodimdbl => metaObj%twodimdbl
                else
                    allocate(twodimdbl(metaObj%sizem, metaObj%sizen), source=metaObj%twodimdbl)
                endif
            else
                if(keepReference) then
                    twodimdbl => returnTwoDimDbl(metaObj%next, key, keepReference)
                else
                    shp = enquireShape(metaObj%next, key)
                    allocate(twodimdbl(shp(1), shp(2)), source=returnTwoDimDbl(metaObj%next, key, keepReference))
                endif
            endif
        else
            print *, '>>> DL_PY2F ERROR: keyword "', key, '" not found in dictionary.'
        endif
    endfunction returnTwoDimDbl
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
                metaObj%onedimlng = array
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
                metaObj%twodimlng = array
            else
                call setTwoDimInt(metaObj%next, key, array)
            endif
        else
            print *, '>>> DL_PY2F ERROR: keyword "', key, '" not found in dictionary.'
        endif
    endsubroutine setTwoDimInt
    recursive subroutine setOneDimLng(metaObj, key, array)
        class(dictType) , intent(inout) :: metaObj
        character(len=*), intent(in)    :: key
        integer(kind=8) , intent(in)    :: array(:)
        if(associated(metaObj%key)) then
            if(metaObj%key.eq.key) then
                metaObj%onedimlng = array
            else
                call setOneDimLng(metaObj%next, key, array)
            endif
        else
            print *, '>>> DL_PY2F ERROR: keyword "', key, '" not found in dictionary.'
        endif
    endsubroutine setOneDimLng
    recursive subroutine setTwoDimLng(metaObj, key, array)
        class(dictType) , intent(inout) :: metaObj
        character(len=*), intent(in)    :: key
        integer(kind=8) , intent(in)    :: array(:,:)
        if(associated(metaObj%key)) then
            if(metaObj%key.eq.key) then
                metaObj%twodimlng = array
            else
                call setTwoDimLng(metaObj%next, key, array)
            endif
        else
            print *, '>>> DL_PY2F ERROR: keyword "', key, '" not found in dictionary.'
        endif
    endsubroutine setTwoDimLng
    subroutine getInt(metaObj, key, val)
        class(dictType) , intent(in)  :: metaObj
        character(len=*), intent(in)  :: key
        integer(kind=4) , intent(out) :: val
        type(dictType)  , pointer     :: metaObjPtr
        allocate(metaObjPtr, source=metaObj)
        selecttype(tmp=>metaObjPtr%returnScalar(key))
            type is(integer(kind=4))
                val = tmp
            type is(integer(kind=8))
                val = int(tmp)
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
        allocate(metaObjPtr, source=metaObj)
        selecttype(tmp=>metaObjPtr%returnScalar(key))
            type is(real(kind=4))
                val = tmp
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
        allocate(metaObjPtr, source=metaObj)
        selecttype(tmp=>metaObjPtr%returnScalar(key))
            type is(real(kind=4))
                val = real(tmp, kind=8)
            type is(real(kind=8))
                val = tmp
        endselect
        deallocate(metaObjPtr)
    endsubroutine getDouble
    subroutine getOneDimInt(metaObj, key, array, readonly)
        class(dictType)          , intent(in)           :: metaObj
        character(len=*)         , intent(in)           :: key
        integer(kind=4) , pointer, intent(out)          :: array(:)
        logical                  , intent(in), optional :: readonly
        type(dictType)  , pointer                       :: metaObjPtr
        logical                                         :: keepReference
        keepReference = .true.
        if(present(readonly)) then
            keepReference = .not.readonly
        endif
        allocate(metaObjPtr, source=metaObj)
        if(keepReference) then
            array => metaObjPtr%returnOneDimInt(key, keepReference)
        else
            allocate(array, source=metaObjPtr%returnOneDimInt(key, keepReference))
        endif
        deallocate(metaObjPtr)
    endsubroutine getOneDimInt
    subroutine getTwoDimInt(metaObj, key, array, readonly)
        class(dictType)          , intent(in)           :: metaObj
        character(len=*)         , intent(in)           :: key
        integer(kind=4) , pointer, intent(out)          :: array(:,:)
        logical                  , intent(in), optional :: readonly
        type(dictType)  , pointer                       :: metaObjPtr
        logical                                         :: keepReference
        keepReference = .true.
        if(present(readonly)) then
            keepReference = .not.readonly
        endif
        allocate(metaObjPtr, source=metaObj)
        if(keepReference) then
            array => metaObjPtr%returnTwoDimInt(key, keepReference)
        else
            allocate(array, source=metaObjPtr%returnTwoDimInt(key, keepReference))
        endif
        deallocate(metaObjPtr)
    endsubroutine getTwoDimInt
    subroutine getOneDimLng(metaObj, key, array, readonly)
        class(dictType)          , intent(in)           :: metaObj
        character(len=*)         , intent(in)           :: key
        integer(kind=8) , pointer, intent(out)          :: array(:)
        logical                  , intent(in), optional :: readonly
        type(dictType)  , pointer                       :: metaObjPtr
        logical                                         :: keepReference
        keepReference = .true.
        if(present(readonly)) then
            keepReference = .not.readonly
        endif
        allocate(metaObjPtr, source=metaObj)
        if(keepReference) then
            array => metaObjPtr%returnOneDimLng(key, keepReference)
        else
            allocate(array, source=metaObjPtr%returnOneDimLng(key, keepReference))
        endif
        deallocate(metaObjPtr)
    endsubroutine getOneDimLng
    subroutine getTwoDimLng(metaObj, key, array, readonly)
        class(dictType)          , intent(in)           :: metaObj
        character(len=*)         , intent(in)           :: key
        integer(kind=8) , pointer, intent(out)          :: array(:,:)
        logical                  , intent(in), optional :: readonly
        type(dictType)  , pointer                       :: metaObjPtr
        logical                                         :: keepReference
        keepReference = .true.
        if(present(readonly)) then
            keepReference = .not.readonly
        endif
        allocate(metaObjPtr, source=metaObj)
        if(keepReference) then
            array => metaObjPtr%returnTwoDimLng(key, keepReference)
        else
            allocate(array, source=metaObjPtr%returnTwoDimLng(key, keepReference))
        endif
        deallocate(metaObjPtr)
    endsubroutine getTwoDimLng
    subroutine getOneDimDbl(metaObj, key, array, readonly)
        class(dictType)          , intent(in)           :: metaObj
        character(len=*)         , intent(in)           :: key
        real(kind=8)    , pointer, intent(out)          :: array(:)
        logical                  , intent(in), optional :: readonly
        type(dictType)  , pointer                       :: metaObjPtr
        logical                                         :: keepReference
        keepReference = .true.
        if(present(readonly)) then
            keepReference = .not.readonly
        endif
        allocate(metaObjPtr, source=metaObj)
        if(keepReference) then
            array => metaObjPtr%returnOneDimDbl(key, keepReference)
        else
            allocate(array, source=metaObjPtr%returnOneDimDbl(key, keepReference))
        endif
        deallocate(metaObjPtr)
    endsubroutine getOneDimDbl
    subroutine getTwoDimDbl(metaObj, key, array, readonly)
        class(dictType)          , intent(in)           :: metaObj
        character(len=*)         , intent(in)           :: key
        real(kind=8)    , pointer, intent(out)          :: array(:,:)
        logical                  , intent(in), optional :: readonly
        type(dictType)  , pointer                       :: metaObjPtr
        logical                                         :: keepReference
        keepReference = .true.
        if(present(readonly)) then
            keepReference = .not.readonly
        endif
        allocate(metaObjPtr, source=metaObj)
        if(keepReference) then
            array => metaObjPtr%returnTwoDimDbl(key, keepReference)
        else
            allocate(array, source=metaObjPtr%returnTwoDimDbl(key, keepReference))
        endif
        deallocate(metaObjPtr)
    endsubroutine getTwoDimDbl
    subroutine getCCharArray(metaObj, key, array)
        class(dictType)                , intent(in)  :: metaObj
        character(len=*)               , intent(in)  :: key
        character(kind=c_char)         , intent(out) :: array(:)
        character(len=:)      , pointer              :: cbuff
        integer                                      :: i
        type(dictType)  , pointer     :: metaObjPtr
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
        character       , target               :: blanc = ""
        type charType
            sequence
            character(len=255) :: char
        endtype charType
        type(charType)  , pointer              :: charbuff
        type(dictType)  , pointer     :: metaObjPtr
        allocate(metaObjPtr, source=metaObj)
        charbuff => metaObjPtr%returnScalar(key)
        val => charbuff%char
        selecttype(gnu=>metaObjPtr%returnScalar(key))
            type is(character(*))
                if(trim(gnu).ne."") then
                    val => gnu
                endif
        endselect
        deallocate(metaObjPtr)
    endsubroutine getChar
    subroutine getCFuncPtr(metaObj, key, val)
        class(dictType)          , intent(in)  :: metaObj
        character(len=*)         , intent(in)  :: key
        type(c_funptr)           , intent(out) :: val
        type(dictType)  , pointer     :: metaObjPtr
        allocate(metaObjPtr, source=metaObj)
        val = metaObjPtr%returnCFuncPtr(key)
        deallocate(metaObjPtr)
    endsubroutine getCFuncPtr
    subroutine getLogical(metaObj, key, val)
        class(dictType) , intent(in)  :: metaObj
        character(len=*), intent(in)  :: key
        logical         , intent(out) :: val
        type(dictType)  , pointer     :: metaObjPtr
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
        allocate(metaObjPtr, source=metaObj)
        val => metaObjPtr%returnPyPtr(key)
        if(associated(val)) then
            ncols = int(val%width)
        endif
        deallocate(metaObjPtr)
    endsubroutine getPyPtr
endmodule DL_PY2F
