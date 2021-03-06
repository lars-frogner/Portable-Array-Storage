module pas_io_module
!
! This module contains classes for writing/reading array data to/from 
! binary files, in a format compatible with the corresponding Python
! and C++ versions.
!
! State: Functional
!
! Todo: Data appending
!
! Last modified 14.11.2016 by Lars Frogner
!
implicit none

private

public :: get_type_name, get_shape_string

integer, parameter :: DP = kind(0.0d0)
integer, parameter :: SP = kind(0.0)
integer, parameter :: LO = selected_int_kind(16)
integer, parameter :: SH = selected_int_kind(8)

! Define writer class
type, public :: binary_writer

    private ! Make all attributes private unless explicitly specified as public

    ! Declare class attributes
    integer(LO), public :: block_size

    character(len=:), allocatable :: filename
    integer, allocatable :: block_shape(:)
    character :: dtype
    integer(SH) :: block_dim, dbytes, block_num
    integer(8) :: start_count, stop_count, count_rate
    real(DP) :: write_time

contains

    private

    ! Declare procedures
    procedure, public :: prepare_write ! Constructor
    procedure, public :: write_block
    procedure, public :: print_header_info => print_header_info_writer
    procedure, public :: print_write_syntax
    procedure, public :: end_write

end type binary_writer

! Define reader class
type, public :: binary_reader

    private

    character, allocatable, public :: block(:)
    integer, public :: block_shape_1D(1), &
                       block_shape_2D(2), order_2D(2), &
                       block_shape_3D(3), order_3D(3)

    character(len=:), allocatable :: filename
    integer(SH), allocatable :: body_shape(:), block_shape(:)
    character :: dtype, packtype
    integer(SH) :: block_dim, block_num, dbytes, read_count
    integer(LO) :: start, block_size
    integer(8) :: start_count, stop_count, count_rate
    real(DP) :: read_time

contains

    private
    
    procedure, public :: prepare_read ! Constructor
    procedure, public :: read_block
    procedure, public :: get_block_num
    procedure, public :: print_header_info => print_header_info_reader
    procedure, public :: print_copy_syntax
    procedure, public :: end_read

end type binary_reader

contains

! Subroutines for binary_writer

subroutine prepare_write(this, filename, block_shape, dtype_dbytes)
    implicit none

    ! Opens binary file and writes a header. The header consists of the following entries:

    ! block_dim:                             Number of dimensions of the data block arrays
    ! block_num:                             Total number of data blocks in the body
    ! block_shape[0], ... , block_shape[-1]: Number of elements along each dimension of a data block
    ! dtype:                                 Character indicating the data type of the elements in a data block
    ! dbytes:                                Number of bytes used by an element in a data block
    ! packtype:                              Character indicating the way the arrays have been flattened 
    !                                        ("F": Fortran style, "C": C++/Python style)

    ! All header entries are 32 bit integers except dtype and packtype, which are 8 bit characters.

    class(binary_writer) :: this ! The current instance is stored in the variable "this"

    ! Declare input variables
    character(len=*), intent(in) :: filename
    character(len=*), intent(in) :: dtype_dbytes
    character(len=:), allocatable :: fn_noext
    integer(SH), intent(in) :: block_shape(:)
    integer :: dotpos, read_err

    ! Make sure that the correct file extension is used

    dotpos = index(filename, '.')

    if (dotpos > 0) then

        fn_noext = filename(1:(dotpos-1))

        if (filename(dotpos:) /= '.pas') then

            write(*, '(A)') 'binary_writer warning ("'//filename &
                            //'"): Invalid file extension. Using ".pas" instead.'

        end if

    else 

        fn_noext = filename

    end if

    this%filename = fn_noext//'.pas'

    ! Check that the input string specifying the data type and precision is complete

    if (len_trim(adjustl(dtype_dbytes)) < 2) then

        write(*, '(A)') 'binary_writer error ("'//this%filename &
                        //'"): Invalid input "'//dtype_dbytes//'" for data type/precision. ' &
                        //'Must be a string consisting of a letter (describing the type) followed ' &
                        //'by a number (describing the precision), e.g. "i4" for the 4 byte INTEGER type.'
        stop

    end if

    this%dtype = dtype_dbytes(1:1)
    read(dtype_dbytes(2:), *, iostat=read_err) this%dbytes

    if (read_err /= 0) then

        write(*, '(A)') 'binary_writer error ("'//this%filename &
                        //'"): Invalid input "'//dtype_dbytes//'" for data type/precision. ' &
                        //'The part following "'//dtype_dbytes(1:1)//'" must be a number.'
        stop

    end if

    ! Store necessary attributes for this instance
    this%block_dim = size(block_shape)
    this%block_shape = block_shape
    this%block_size = this%dbytes*product(dble(block_shape))

    this%write_time = 0.0
    this%block_num = 0

    deallocate(fn_noext)

    ! Make sure that the specified data type and precision is valid

    if (this%dtype /= 'S' .and. this%dtype /= 'i' .and. this%dtype /= 'f' .and. this%dtype /= 'c') then

        write(*, '(A)') 'binary_writer error ("'//this%filename &
                        //'"): Invalid data type ("'//this%dtype &
                        //'"). Valid types are "S" (CHARACTER), i" (INTEGER), "f" (REAL) and "c" (COMPLEX).'
        stop

    end if

    if (this%dtype == 'S' .and. this%dbytes /= 1) then

        write(*, '(A, i0, A)') 'binary_writer error ("'//this%filename &
                                //'"): Invalid data precision (', this%dbytes, &
                                ' bytes) for type CHARACTER. The only valid precision is 1 byte.'
        stop

    else if (this%dtype == 'i' .and. this%dbytes /= 1 .and. this%dbytes /= 2 .and. &
        this%dbytes /= 4 .and. this%dbytes /= 8) then

        write(*, '(A, i0, A)') 'binary_writer error ("'//this%filename &
                                //'"): Invalid data precision (', this%dbytes, &
                                ' bytes) for type INTEGER. Valid precisions are 1, 2, 4 and 8 bytes.'
        stop

    else if (this%dtype == 'f' .and. this%dbytes /= 4 .and. this%dbytes /= 8 .and. this%dbytes /= 16) then

        write(*, '(A, i0, A)') 'binary_writer error ("'//this%filename &
                                //'"): Invalid data precision (', this%dbytes, &
                                ' bytes) for type REAL. Valid precisions are 4, 8 and 16 bytes.'
        stop

    else if (this%dtype == 'c' .and. this%dbytes /= 8 .and. this%dbytes /= 16 .and. this%dbytes /= 32) then

        write(*, '(A, i0, A)') 'binary_writer error ("'//this%filename &
                                //'"): Invalid data precision (', this%dbytes, &
                                ' bytes) for type COMPLEX. Valid precisions are 8, 16 and 32 bytes.'
        stop

    end if

    ! Print a warning if automatic packing isn't implemented for the relevant data dimension
    if (this%block_dim > 3) then

        write(*, '(A, i0, A)') 'binary_writer warning ("'//this%filename &
                                //'"): Automatic packing is not implemented for the dimensionality ' &
                                //'of the data blocks that will be written to the file (', &
                                (this%block_dim), 'D). To write the data, you must use the write_block ' &
                                //'subroutine directly with data that is already packed.'

    end if

    ! Record start time
    call system_clock(this%start_count, this%count_rate)

    ! Open binary file for writing
    open(10, file=this%filename, status='replace', form='unformatted', access='stream')

    ! Write header (except the second entry)
    write(10) this%block_dim
    write(10, pos=(4*2 + 1)) block_shape
    write(10) this%dtype
    write(10) this%dbytes
    write(10) 'F'

end subroutine prepare_write

subroutine write_block(this, block)
    implicit none

    ! Takes a data block represented as a 1D byte array and appends it to the end of the binary file.
    ! The correct way to call this subroutine is
    !
    !    call <instance>%write_block(transfer(source=<array>, mold='A', size=<instance>%block_size))
    !
    ! where <instance> is the name of the binary_writer instance and <array> is the name of the array to 
    ! write (which must have the shape and type specified to the prepare_write subroutine).

    class(binary_writer) :: this

    character, intent(in) :: block(this%block_size)
    integer(8) :: count0, count1

    call system_clock(count0)

    write(10) block

    call system_clock(count1)

    this%write_time = this%write_time + (count1 - count0)/real(this%count_rate, DP) ! Update total writing time
    this%block_num = this%block_num + 1                                             ! Update number of data blocks written

end subroutine write_block

subroutine print_header_info_writer(this)
    implicit none

    class(binary_writer) :: this

    write(*, '(A)') new_line('A')//'*** Header info for "'//this%filename//'" ***'
    write(*, '(A, i0, A)') 'Data block dimension: ', (this%block_dim), 'D'
    write(*, '(A, i0)') 'Total number of data blocks: ', (this%block_num)
    write(*, '(A)') 'Data block shape: '//get_shape_string(this%block_shape, .true.)
    write(*, '(A)') 'Data type: '//get_type_name(this%dtype)
    write(*, '(A, i0)') 'Number of bytes per element: ', (this%dbytes)
    write(*, '(A)') 'Pack type: Fortran style'

end subroutine print_header_info_writer

subroutine print_write_syntax(this)
    implicit none

    class(binary_writer) :: this

    write(*, '(A)') '*** Syntax for writing an array with write_block ***'
    write(*, '(A)') 'call <instance>%write_block(transfer(source=<array>, ' &
                    //'mold=''A'', size=<instance>%block_size))'

end subroutine print_write_syntax

subroutine end_write(this, print_report)
    implicit none

    ! Writes the remaining header entry, closes the file and prints an optional writing report

    class(binary_writer) :: this

    logical, intent(in), optional :: print_report
    real(DP) :: tot_size

    write(10, pos=(4 + 1)) this%block_num ! Write remaining header entry (total number of data blocks written)

    close(10) ! Close file

    ! Record stop time
    call system_clock(this%stop_count)

    ! Print writing report

    if (present(print_report) .and. print_report) then

        write(*, '(A)') new_line('A')//'*** Writing report for "'//this%filename//'" ***'

        ! Print block info
        write(*, '(A, i0, A)') 'Data blocks: '//get_shape_string(this%block_shape)//' ' &
                               //get_type_name(this%dtype)//' (', this%dbytes*8, ' bit precision)'

        ! Print number of blocks written
        write(*, '(A, i0)') 'Number of blocks written: ', this%block_num

        ! Print total amount of data written
        tot_size = this%block_num*this%block_size/1024000.0
        write(*, '(A, F0.3, A)') 'Amount of data written: ', tot_size, ' MB'

        ! Print total writing time and speed
        if (this%write_time /= 0.0) then

            write(*, '(A, F0.3, A)') 'Total writing time: ', this%write_time, ' s'

            write(*, '(A, F0.3, A)') 'Average writing speed: ', tot_size/this%write_time, ' MB/s'

        end if

        ! Print total time
        write(*, '(A, F0.3, A)') 'File open for: ', &
                                 (this%stop_count - this%start_count)/real(this%count_rate, DP), ' s'

    end if

    ! Deallocate memory for the allocatable attributes
    deallocate(this%filename)
    deallocate(this%block_shape)

end subroutine end_write


! Subroutines for binary_reader

subroutine prepare_read(this, filename)
    implicit none

    ! Opens a binary file and reads the header.

    class(binary_reader) :: this

    character(len=*), intent(in) :: filename
    character(len=:), allocatable :: fn_noext
    character :: packtype
    integer :: dotpos, i

    ! Make sure that the correct file extension is used

    dotpos = index(filename, '.')

    if (dotpos > 0) then

        fn_noext = filename(1:(dotpos-1))

        if (filename(dotpos:) /= '.pas') then

            write(*, '(A)') 'binary_reader error ("'//filename &
                            //'"): Invalid file extension. Can only read ".pas" files.'
            stop

        end if

    else

        fn_noext = filename

        write(*, '(A)') 'binary_reader warning ("'//filename &
                        //'"): No file extension supplied. Assuming extension is ".pas".'

    end if

    this%filename = fn_noext//'.pas'
    deallocate(fn_noext)

    ! Record start time
    call system_clock(this%start_count, this%count_rate)

    ! Open file for reading
    open(10, file=this%filename, status='old', form='unformatted', access='stream')

    ! Read first header entry
    read(10) this%block_dim

    ! Allocate memory for the array describing the shape of the total data array
    allocate(this%body_shape(this%block_dim+1))

    ! Read the remaining header entries
    read(10) this%body_shape, this%dtype, this%dbytes, this%packtype

    ! Print error and abort if the data type or precision in the file cannot be read

    if (this%dtype /= 'S' .and. this%dtype /= 'i' .and. this%dtype /= 'f' .and. this%dtype /= 'c') then

        write(*, '(A)') 'binary_reader error ("'//this%filename &
                        //'"): Cannot read the data type of this file ("'//this%dtype &
                        //'"). Can only read "S" (CHARACTER), "i" (INTEGER), "f" (REAL) and "c" (COMPLEX).'
        close(10)
        stop

    end if

    if (this%dtype == 'S' .and. this%dbytes /= 1) then

        write(*, '(A, i0, A)') 'binary_reader error ("'//this%filename &
                                //'"): Cannot read the data precision of this file (', &
                                this%dbytes, ' byte). Can only read 1 byte precision for type CHARACTER.'
        close(10)
        stop

    else if (this%dtype == 'i' .and. this%dbytes /= 1 .and. this%dbytes /= 2 .and. &
        this%dbytes /= 4 .and. this%dbytes /= 8) then

        write(*, '(A, i0, A)') 'binary_reader error ("'//this%filename &
                                //'"): Cannot read the data precision of this file (', &
                                this%dbytes, ' byte). Can only read 1, 2, 4 and 8 byte precision for type INTEGER.'
        close(10)
        stop

    else if (this%dtype == 'f' .and. this%dbytes /= 4 .and. this%dbytes /= 8 .and. this%dbytes /= 16) then

        write(*, '(A, i0, A)') 'binary_reader error ("'//this%filename &
                                //'"): Cannot read the data precision of this file (', &
                                this%dbytes, ' byte). Can only read 4, 8 and 16 byte precision for type REAL.'
        close(10)
        stop

    else if (this%dtype == 'c' .and. this%dbytes /= 8 .and. this%dbytes /= 16 .and. this%dbytes /= 32) then

        write(*, '(A, i0, A)') 'binary_reader error ("'//this%filename &
                                //'"): Cannot read the data precision of this file (', &
                                this%dbytes, ' byte). Can only read 8, 16 and 32 byte precision for type COMPLEX.'
        close(10)
        stop

    end if

    ! Set values for class attributes
    this%block_num = this%body_shape(1)
    this%block_shape = this%body_shape(2:)
    this%block_size = this%dbytes*product(dble(this%block_shape))
    this%start = 4*(this%block_dim + 3) + 3 ! Location of the first data block

    ! Allocate enough memory for the byte array to store the bytes of a data block
    allocate(this%block(this%block_size))

    ! Store shape in public non-allocatable array

    if (this%block_dim == 1) then

        this%block_shape_1D = this%block_shape(1)

    else if (this%block_dim == 2) then

        this%block_shape_2D = this%block_shape(1:2)

    else if (this%block_dim == 3) then

        this%block_shape_3D = this%block_shape(1:3)

    else

        write(*, '(A, i0, A)') 'binary_reader error ("'//this%filename &
                                //'"): The dimensionality of the data blocks in this file (', &
                                (this%block_dim), 'D) is currently not supported.'
        stop

    end if

    ! Initialize an array required for reshaping the data correctly 
    ! (since the flattening of an array is done differently in Fortran and C++/Python)

    if (this%packtype == 'C' .or. this%packtype == 'c') then

        this%order_2D = [2, 1]
        this%order_3D = [3, 2, 1]

    else

        if (this%packtype /= 'F' .and. this%packtype /= 'f') then

            write(*, '(A)') 'binary_reader warning ("'//this%filename &
                            //'"): Pack type "'//this%packtype//'" not recognized. ' &
                            //'Assuming Fortran style flattening.'

        end if

        this%order_2D = [1, 2]
        this%order_3D = [1, 2, 3]

    end if

    ! Read the first data block to get some sensible initial values for the byte array
    call this%read_block(1)

    this%read_time = 0.0
    this%read_count = 0

end subroutine prepare_read

subroutine read_block(this, i)
    implicit none

    ! Reads data block nr. i from the file and stores it in the 1D byte array this%block.
    ! To copy the data from this%block into an array of the correct type and shape, use the following assignment:
    !
    ! <array> = reshape(source=transfer(source=<instance>%block, mold=<array>), &
    !                        shape=<instance>%block_shape_<dimension>D, &
    !                        order=<instance>%order_<dimension>D)
    !
    ! where <array> is the name of the array to copy to, <instance> is the name of the binary_reader instance
    ! and <dimension> is the number of dimensions of the array to copy to. Exclude the order argument for 1D arrays.

    class(binary_reader) :: this

    integer, intent(in) :: i
    integer(8) :: count0, count1

    call system_clock(count0)

    read(10, pos=(this%start + (i-1)*this%block_size)) this%block

    call system_clock(count1)

    this%read_time = this%read_time + (count1 - count0)/real(this%count_rate, DP) ! Update total reading time
    this%read_count = this%read_count + 1                                         ! Update number of data blocks read     

end subroutine read_block

function get_block_num(this) result(block_num)

    class(binary_reader), intent(in) :: this
    integer(SH) :: block_num

    block_num = this%block_num

end function get_block_num

subroutine print_header_info_reader(this)
    implicit none

    class(binary_reader) :: this

    character(len=20) :: packtype_str

    if (this%packtype == 'C' .or. this%packtype == 'c') then

        packtype_str = 'C++/Python style'

    else

        packtype_str = 'Fortran style'

    end if

    write(*, '(A)') new_line('A')//'*** Header info for "'//this%filename//'" ***'
    write(*, '(A, i0, A)') 'Data block dimension: ', (this%block_dim), 'D'
    write(*, '(A, i0)') 'Total number of data blocks: ', (this%block_num)
    write(*, '(A)') 'Data block shape: '//get_shape_string(this%block_shape, .true.)
    write(*, '(A)') 'Data type: '//get_type_name(this%dtype)
    write(*, '(A, i0)') 'Number of bytes per element: ', (this%dbytes)
    write(*, '(A)') 'Pack type: '//trim(adjustl(packtype_str))

end subroutine print_header_info_reader

subroutine print_copy_syntax(this)
    implicit none

    class(binary_reader) :: this

    write(*, '(A)') '*** Syntax for copying read data into an array ***'
    write(*, '(A)') '<array> = reshape(source=transfer(source=<instance>%block, mold=<array>), &'//new_line('A') &
                               //'                shape=<instance>%block_shape_<dimension>D, &' &
                               //new_line('A')//'                 order=<instance>%order_<dimension>D)'

end subroutine print_copy_syntax

subroutine end_read(this, print_report)
    implicit none

    ! Closes the file and prints an optional reading report

    class(binary_reader) :: this

    logical, intent(in), optional :: print_report
    real(DP) :: tot_size, read_size

    ! Close the file
    close(10)

    ! Record stop time
    call system_clock(this%stop_count)

    ! Print reading report

    if (present(print_report) .and. print_report) then

        write(*, '(A)') new_line('A')//'*** Reading report for "'//this%filename//'" ***'

        ! Print block info
        write(*, '(A, i0, A)') 'Data blocks: '//get_shape_string(this%block_shape)//' ' &
                               //get_type_name(this%dtype)//' (', this%dbytes*8, ' bit precision)'

        ! Print number of blocks read
        write(*, '(A, i0, A, i0)') 'Number of blocks read: ', this%read_count, '/', this%block_num

        ! Print amount of data read
        read_size = this%read_count*this%block_size/1024000.0
        tot_size = this%block_num*this%block_size/1024000.0
        write(*, '(A, F0.3, A, F0.3, A)') 'Amount of data read: ', read_size, '/', tot_size, ' MB'

        ! Print total reading time and speed

        if (this%read_time /= 0.0) then

            write(*, '(A, F0.3, A)') 'Total reading time: ', this%read_time, ' s'

            write(*, '(A, F0.3, A)') 'Average reading speed: ', read_size/this%read_time, ' MB/s'

        end if

        ! Print total time
        write(*, '(A, F0.3, A)') 'File open for: ', &
                                 (this%stop_count - this%start_count)/real(this%count_rate, DP), ' s'

    end if

    ! Deallocate memory for allocatable attributes that are no longer needed
    deallocate(this%filename, this%body_shape, this%block_shape, this%block)

end subroutine end_read


! Subroutines available to all classes in this module

function get_type_name(dtype, use_fortran_style) result(name)
    implicit none

    character, intent(in) :: dtype
    logical, optional, intent(in) :: use_fortran_style
    character(len=:), allocatable :: name
    character(len=50) :: type_str

    ! Print data type

    if (present(use_fortran_style) .and. use_fortran_style) then

        if (dtype == 'S') then

            type_str = 'CHARACTER'

        else if (dtype == 'i') then

            type_str = 'INTEGER'

        else if (dtype == 'f') then

            type_str = 'REAL'

        else if (dtype == 'c') then

            type_str = 'COMPLEX'

        else

            type_str = '<not defined>'

        end if

    else

        if (dtype == 'S') then

            type_str = 'character'

        else if (dtype == 'i') then

            type_str = 'signed integer'

        else if (dtype == 'f') then

            type_str = 'float'

        else if (dtype == 'c') then

            type_str = 'complex'

        else

            type_str = '<not defined>'

        end if

    end if

    name = trim(adjustl(type_str))

end function get_type_name

function get_shape_string(block_shape, use_paran) result(shape_string)
    implicit none

    integer, intent(in) :: block_shape(:)
    logical, optional, intent(in) :: use_paran
    character(len=:), allocatable :: shape_string
    character(len=100) :: shape_str
    character(len=20) :: num_str
    character(len=3) :: symb
    integer :: i

    if (present(use_paran) .and. use_paran) then

        symb = ','

    else

        symb = ' x'

    end if

    write(num_str, '(i0)') block_shape(1)

    shape_str = adjustl(num_str)

    do i = 2, size(block_shape)

        write(num_str, '(i0)') block_shape(i)
        shape_str = trim(shape_str)//trim(symb)//' '//trim(adjustl(num_str))

    end do

    if (present(use_paran) .and. use_paran) then

        shape_string = '('//trim(adjustl(shape_str))//')'

    else

        shape_string = trim(adjustl(shape_str))

    end if

end function get_shape_string

end module pas_io_module