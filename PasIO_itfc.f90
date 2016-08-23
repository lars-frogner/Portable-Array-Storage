module PasIO
	implicit none

	! This module contains classes for writing/reading array data to/from binary files.

	private :: get_type_name, get_shape_string

	! Define writer class
	type BinWriter

		private ! Make all attributes private unless explicitly specified as public

		! Declare class attributes
		character(len=:), allocatable :: filename
		integer, allocatable :: block_shape(:)
		character :: dtype
		integer*4 :: block_dim, dbytes, block_num
		integer*8 :: block_size
		integer :: start_count, stop_count
		real :: count_rate, write_time

	contains

		! Declare procedures
		procedure, public :: prepare_write ! Constructor
		procedure, public :: write_bytearr
		procedure, public :: print_header_info => print_header_info_writer
		procedure, public :: end_write

		procedure, private :: check_write_input_arr

		procedure, private :: wb_S1_1D, wb_S1_2D, wb_S1_3D, &
							  wb_i1_1D, wb_i2_1D, wb_i4_1D, wb_i8_1D, &
							  wb_f4_1D, wb_f8_1D, wb_f16_1D, &
							  wb_c8_1D, wb_c16_1D, wb_c32_1D, &
							  wb_i1_2D, wb_i2_2D, wb_i4_2D, wb_i8_2D, &
							  wb_f4_2D, wb_f8_2D, wb_f16_2D, &
							  wb_c8_2D, wb_c16_2D, wb_c32_2D, &
							  wb_i1_3D, wb_i2_3D, wb_i4_3D, wb_i8_3D, &
							  wb_f4_3D, wb_f8_3D, wb_f16_3D, &
							  wb_c8_3D, wb_c16_3D, wb_c32_3D

		! Create generic interface for the wb (writing) subroutines 
		! (the correct subroutine will be called automatically depending on the input)

		generic, public :: write_block => wb_S1_1D, wb_S1_2D, wb_S1_3D, &
										  wb_i1_1D, wb_i2_1D, wb_i4_1D, wb_i8_1D, &
										  wb_f4_1D, wb_f8_1D, wb_f16_1D, &
										  wb_c8_1D, wb_c16_1D, wb_c32_1D, &
										  wb_i1_2D, wb_i2_2D, wb_i4_2D, wb_i8_2D, &
										  wb_f4_2D, wb_f8_2D, wb_f16_2D, &
										  wb_c8_2D, wb_c16_2D, wb_c32_2D, &
										  wb_i1_3D, wb_i2_3D, wb_i4_3D, wb_i8_3D, &
										  wb_f4_3D, wb_f8_3D, wb_f16_3D, &
										  wb_c8_3D, wb_c16_3D, wb_c32_3D

	end type BinWriter

	! Define reader class
	type BinReader

		private

		character, allocatable, public :: bytearr(:)
		integer, public :: block_shape_2D(2), block_shape_3D(3), order_arr_2D(2), order_arr_3D(3)

		character(len=:), allocatable :: filename
		integer*4, allocatable :: body_shape(:), block_shape(:)
		character :: dtype, packtype
		integer*4 :: block_dim, block_num, dbytes, read_count
		integer*8 :: start, block_size
		integer :: start_count, stop_count
		real :: count_rate, read_time

	contains
		
		procedure, public :: prepare_read ! Constructor
		procedure, public :: read_to_bytearr
		procedure, public :: print_header_info => print_header_info_reader
		procedure, public :: end_read

		procedure, private :: check_read_input_arr

		procedure, private :: rb_S1_1D, rb_S1_2D, rb_S1_3D, &
							  rb_i1_1D, rb_i2_1D, rb_i4_1D, rb_i8_1D, &
							  rb_f4_1D, rb_f8_1D, rb_f16_1D, &
							  rb_c8_1D, rb_c16_1D, rb_c32_1D, &
							  rb_i1_2D, rb_i2_2D, rb_i4_2D, rb_i8_2D, &
							  rb_f4_2D, rb_f8_2D, rb_f16_2D, &
							  rb_c8_2D, rb_c16_2D, rb_c32_2D, &
							  rb_i1_3D, rb_i2_3D, rb_i4_3D, rb_i8_3D, &
							  rb_f4_3D, rb_f8_3D, rb_f16_3D, &
							  rb_c8_3D, rb_c16_3D, rb_c32_3D

		! Create generic interface for the rb (reading) subroutines 
		! (the correct subroutine will be called automatically depending on the input)

		generic, public :: read_block_to => rb_S1_1D, rb_S1_2D, rb_S1_3D, &
										 	rb_i1_1D, rb_i2_1D, rb_i4_1D, rb_i8_1D, &
											rb_f4_1D, rb_f8_1D, rb_f16_1D, &
											rb_c8_1D, rb_c16_1D, rb_c32_1D, &
										 	rb_i1_2D, rb_i2_2D, rb_i4_2D, rb_i8_2D, &
										 	rb_f4_2D, rb_f8_2D, rb_f16_2D, &
										 	rb_c8_2D, rb_c16_2D, rb_c32_2D, &
										 	rb_i1_3D, rb_i2_3D, rb_i4_3D, rb_i8_3D, &
										 	rb_f4_3D, rb_f8_3D, rb_f16_3D, &
										 	rb_c8_3D, rb_c16_3D, rb_c32_3D

	end type BinReader

contains

	! Subroutines for BinWriter

	subroutine prepare_write(this, filename, block_shape, dtype_dbytes)
		implicit none

		! Opens binary file and writes a header. The header consists of the following entries:
	
		! block_dim: 						     Number of dimensions of the data block arrays
		! block_num:						     Total number of data blocks in the body
		! block_shape[0], ... , block_shape[-1]: Number of elements along each dimension of a data block
		! dtype:								 Character indicating the data type of the elements in a data block
		! dbytes: 						         Number of bytes used by an element in a data block
		! packtype:							     Character indicating the way the arrays have been flattened 
		!										 ("F": Fortran style, "C": C++/Python style)

		! All header entries are 32 bit integers except dtype and packtype, which are 8 bit characters.

		class(BinWriter) :: this ! The current instance is stored in the variable "this"

		! Declare input variables
		character(len=*), intent(in) :: filename
		character(len=*), intent(in) :: dtype_dbytes
		character(len=:), allocatable :: fn_noext
		integer*4, intent(in) :: block_shape(:)
		integer :: dotpos, read_err, count_rate

		! Make sure that the correct file extension is used

		dotpos = index(filename, '.')

		if (dotpos > 0) then

			fn_noext = filename(1:(dotpos-1))

			if (filename(dotpos:) /= '.pas') then

				write(*, '(A)') 'BinWriter warning ("'//filename &
								//'"): Invalid file extension. Using ".pas" instead.'

			end if

		else 

			fn_noext = filename

		end if

		this%filename = fn_noext//'.pas'

		! Check that the input string specifying the data type and precision is complete

		if (len_trim(adjustl(dtype_dbytes)) < 2) then

			write(*, '(A)') 'BinWriter error ("'//this%filename &
							//'"): Invalid input "'//dtype_dbytes//'" for data type/precision. ' &
							//'Must be a string consisting of a letter (describing the type) followed ' &
							//'by a number (describing the precision), e.g. "i4" for the 4 byte INTEGER type.'
			stop

		end if

		this%dtype = dtype_dbytes(1:1)
		read(dtype_dbytes(2:), *, iostat=read_err) this%dbytes

		if (read_err /= 0) then

			write(*, '(A)') 'BinWriter error ("'//this%filename &
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

			write(*, '(A)') 'BinWriter error ("'//this%filename &
							//'"): Invalid data type ("'//this%dtype &
							//'"). Valid types are "S" (CHARACTER), i" (INTEGER), "f" (REAL) and "c" (COMPLEX).'
			stop

		end if

		if (this%dtype == 'S' .and. this%dbytes /= 1) then

			write(*, '(A, i0, A)') 'BinWriter error ("'//this%filename &
									//'"): Invalid data precision (', this%dbytes, &
									' bytes) for type CHARACTER. The only valid precision is 1 byte.'
			stop

		else if (this%dtype == 'i' .and. this%dbytes /= 1 .and. this%dbytes /= 2 .and. &
			this%dbytes /= 4 .and. this%dbytes /= 8) then

			write(*, '(A, i0, A)') 'BinWriter error ("'//this%filename &
									//'"): Invalid data precision (', this%dbytes, &
									' bytes) for type INTEGER. Valid precisions are 1, 2, 4 and 8 bytes.'
			stop

		else if (this%dtype == 'f' .and. this%dbytes /= 4 .and. this%dbytes /= 8 .and. this%dbytes /= 16) then

			write(*, '(A, i0, A)') 'BinWriter error ("'//this%filename &
									//'"): Invalid data precision (', this%dbytes, &
									' bytes) for type REAL. Valid precisions are 4, 8 and 16 bytes.'
			stop

		else if (this%dtype == 'c' .and. this%dbytes /= 8 .and. this%dbytes /= 16 .and. this%dbytes /= 32) then

			write(*, '(A, i0, A)') 'BinWriter error ("'//this%filename &
									//'"): Invalid data precision (', this%dbytes, &
									' bytes) for type COMPLEX. Valid precisions are 8, 16 and 32 bytes.'
			stop

		end if

		! Print a warning if automatic packing isn't implemented for the relevant data dimension
		if (this%block_dim > 3) then

			write(*, '(A, i0, A)') 'BinWriter warning ("'//this%filename &
									//'"): Automatic packing is not implemented for the dimensionality ' &
									//'of the data blocks that will be written to the file (', &
									(this%block_dim), 'D). To write the data, you must use the write_bytearr ' &
									//'subroutine directly with data that is already packed.'

		end if

		! Record start time
		call system_clock(this%start_count, count_rate)
		this%count_rate = real(count_rate)

		! Open binary file for writing
		open(10, file=this%filename, status='replace', form='unformatted', access='stream')

		! Write header (except the second entry)
		write(10) this%block_dim
		write(10, pos=(4*2 + 1)) block_shape
		write(10) this%dtype
		write(10) this%dbytes
		write(10) 'F'

	end subroutine prepare_write

	subroutine write_bytearr(this, block)
		implicit none

		! Takes a data block represented as a 1D byte array and appends it to the end of the binary file.
		! The correct way to call this subroutine is
		!
		! 					call <instance name>%write_bytearr(transfer(<array name>, 'A', <array size>))
		!
		! where <instance name> is the name of the BinWriter instance, <array name> is the name of the array to 
		! write (which must have the dimensions and type specified to the prepare_write subroutine), and 
		! <array size> is the number of bytes used by the array (as found by calling sizeof(<array name>)).

		class(BinWriter) :: this

		character, intent(in) :: block(this%block_size)
		integer :: count0, count1

		call system_clock(count0)

		write(10) block

		call system_clock(count1)

		this%write_time = this%write_time + (count1 - count0)/this%count_rate ! Update total writing time
		this%block_num = this%block_num + 1 	  	 						  ! Update number of data blocks written

	end subroutine write_bytearr

	subroutine print_header_info_writer(this)
		implicit none

		class(BinWriter) :: this

		write(*, '(A)') new_line('A')//'*** Header info for "'//this%filename//'" ***'
		write(*, '(A, i0, A)') 'Data block dimension: ', (this%block_dim), 'D'
		write(*, '(A, i0)') 'Total number of data blocks: ', (this%block_num)
		write(*, '(A)') 'Data block shape: '//get_shape_string(this%block_shape, .true.)
		write(*, '(A)') 'Data type: '//get_type_name(this%dtype)
		write(*, '(A, i0)') 'Number of bytes per element: ', (this%dbytes)
		write(*, '(A)') 'Pack type: Fortran style'

	end subroutine print_header_info_writer

	subroutine end_write(this, printReport)
		implicit none

		! Writes the remaining header entry, closes the file and prints an optional writing report

		class(BinWriter) :: this

		logical, intent(in), optional :: printReport
		real*8 :: tot_size

		write(10, pos=(4 + 1)) this%block_num ! Write remaining header entry (total number of data blocks written)

		close(10) ! Close file

		! Record stop time
		call system_clock(this%stop_count)

		! Print writing report

		if (present(printReport) .and. printReport) then

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
									 (this%stop_count - this%start_count)/this%count_rate, ' s'

		end if

		! Deallocate memory for the allocatable attributes
		deallocate(this%filename)
		deallocate(this%block_shape)

	end subroutine end_write

	subroutine check_write_input_arr(this, in_type, in_precision, in_shape)
		implicit none

		! Gets properties of the array inputted in one of the
		! write_block subroutines and prints an error message
		! if these properties don't correspond to those of
		! specified in the call to prepare_write.

		class(BinWriter) :: this

		character, intent(in) :: in_type
		integer, intent(in) :: in_precision, in_shape(:)

		if (this%dtype /= in_type .or. this%dbytes /= in_precision .or. &
			(this%block_dim /= size(in_shape) .or. any(this%block_shape /= in_shape))) then

			write(*, '(A, i0, A, i0, A)') 'BinWriter error ("'//this%filename &
										   //'"): Invalid argument array to write_block. Must be a ' &
										   //get_type_name(this%dtype, .true.)//'(', &
										   this%dbytes, ') array of shape '//get_shape_string(this%block_shape)//'.'

			close(10)
			stop

		end if

	end subroutine check_write_input_arr


	! Subroutines for BinReader

	subroutine prepare_read(this, filename)
		implicit none

		! Opens a binary file and reads the header.

		class(BinReader) :: this

		character(len=*), intent(in) :: filename
		character(len=:), allocatable :: fn_noext
		character :: packtype
		integer :: dotpos, count_rate, i

		! Make sure that the correct file extension is used

		dotpos = index(filename, '.')

		if (dotpos > 0) then

			fn_noext = filename(1:(dotpos-1))

			if (filename(dotpos:) /= '.pas') then

				write(*, '(A)') 'BinReader error ("'//filename &
								//'"): Invalid file extension. Can only read ".pas" files.'
				stop

			end if

		else

			fn_noext = filename

			write(*, '(A)') 'BinReader warning ("'//filename &
							//'"): No file extension supplied. Assuming extension is ".pas".'

		end if

		this%filename = fn_noext//'.pas'
		deallocate(fn_noext)

		! Record start time
		call system_clock(this%start_count, count_rate)
		this%count_rate = real(count_rate)

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

			write(*, '(A)') 'BinReader error ("'//this%filename &
							//'"): Cannot read the data type of this file ("'//this%dtype &
							//'"). Can only read "S" (CHARACTER), "i" (INTEGER), "f" (REAL) and "c" (COMPLEX).'
			close(10)
			stop

		end if

		if (this%dtype == 'S' .and. this%dbytes /= 1) then

			write(*, '(A, i0, A)') 'BinReader error ("'//this%filename &
									//'"): Cannot read the data precision of this file (', &
									this%dbytes, ' byte). Can only read 1 byte precision for type CHARACTER.'
			close(10)
			stop

		else if (this%dtype == 'i' .and. this%dbytes /= 1 .and. this%dbytes /= 2 .and. &
			this%dbytes /= 4 .and. this%dbytes /= 8) then

			write(*, '(A, i0, A)') 'BinReader error ("'//this%filename &
									//'"): Cannot read the data precision of this file (', &
									this%dbytes, ' byte). Can only read 1, 2, 4 and 8 byte precision for type INTEGER.'
			close(10)
			stop

		else if (this%dtype == 'f' .and. this%dbytes /= 4 .and. this%dbytes /= 8 .and. this%dbytes /= 16) then

			write(*, '(A, i0, A)') 'BinReader error ("'//this%filename &
									//'"): Cannot read the data precision of this file (', &
									this%dbytes, ' byte). Can only read 4, 8 and 16 byte precision for type REAL.'
			close(10)
			stop

		else if (this%dtype == 'c' .and. this%dbytes /= 8 .and. this%dbytes /= 16 .and. this%dbytes /= 32) then

			write(*, '(A, i0, A)') 'BinReader error ("'//this%filename &
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
		allocate(this%bytearr(this%block_size))

		! Print a warning if automatic reshaping isn't implemented for the data dimension in this file
		if (this%block_dim > 3) then

			write(*, '(A, i0, A)') 'BinReader warning ("'//this%filename &
									//'"): Automatic reshaping is not implemented for the ' &
									//'dimensionality of the data blocks in this file (', &
									(this%block_dim), 'D). To read the data, you must call ' &
									//'read_to_bytearr directly, then copy and reshape the byte ' &
									//'array (this%bytearr) manually.'

		end if

		! Initialize an array required for reshaping the data correctly 
		! (since the flattening of an array is done differently in Fortran and C++/Python)

		if (this%packtype == 'C' .or. this%packtype == 'c') then

			this%order_arr_2D = [2, 1]
			this%order_arr_3D = [3, 2, 1]

		else

			if (this%packtype /= 'F' .and. this%packtype /= 'f') then

				write(*, '(A)') 'BinReader warning ("'//this%filename &
								//'"): Pack type "'//this%packtype//'" not recognized. ' &
								//'Assuming Fortran style flattening.'

			end if

			this%order_arr_2D = [1, 2]
			this%order_arr_3D = [1, 2, 3]

		end if

		! Read the first data block to get some sensible initial values for the byte array
		call this%read_to_bytearr(1)

		this%read_time = 0.0
		this%read_count = 0

	end subroutine prepare_read

	subroutine read_to_bytearr(this, i)
		implicit none

		! Reads data block nr. i from the file and stores the bytes in the byte array

		class(BinReader) :: this

		integer, intent(in) :: i
		integer :: count0, count1

		call system_clock(count0)

		read(10, pos=(this%start + (i-1)*this%block_size)) this%bytearr

		call system_clock(count1)

		this%read_time = this%read_time + (count1 - count0)/this%count_rate ! Update total reading time
		this%read_count = this%read_count + 1 								! Update number of data blocks read		

	end subroutine read_to_bytearr

	subroutine print_header_info_reader(this)
		implicit none

		class(BinReader) :: this

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

	subroutine end_read(this, printReport)
		implicit none

		! Closes the file and prints an optional reading report

		class(BinReader) :: this

		logical, intent(in), optional :: printReport
		real*8 :: tot_size, read_size

		! Close the file
		close(10)

		! Record stop time
		call system_clock(this%stop_count)

		! Print reading report

		if (present(printReport) .and. printReport) then

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
									 (this%stop_count - this%start_count)/this%count_rate, ' s'

		end if

		! Deallocate memory for allocatable attributes that are no longer needed
		deallocate(this%filename, this%body_shape, this%block_shape, this%bytearr)

	end subroutine end_read

	subroutine check_read_input_arr(this, in_type, in_precision, in_dim)
		implicit none

		! Gets properties of the array inputted in one of the
		! read_block_to subroutines and prints an error message
		! if these properties don't correspond to those of
		! the data blocks in the file.

		class(BinReader) :: this

		character, intent(in) :: in_type
		integer, intent(in) :: in_precision, in_dim

		if (this%dtype /= in_type .or. this%dbytes /= in_precision .or. this%block_dim /= in_dim) then

			write(*, '(A, i0, A, i0, A)') 'BinReader error ("'//this%filename &
										   //'"): Invalid argument array to read_block_to. Must be an allocatable ', &
										   this%block_dim, 'D '//get_type_name(this%dtype, .true.)//'(', this%dbytes, ') array.'

			close(10)
			stop

		end if

	end subroutine check_read_input_arr


	! Subroutines available to all classes in this module

	function get_type_name(dtype, useFortranStyle) result(name)
		implicit none

		character, intent(in) :: dtype
		logical, optional, intent(in) :: useFortranStyle
		character(len=:), allocatable :: name
		character(len=50) :: type_str

		! Print data type

		if (present(useFortranStyle) .and. useFortranStyle) then

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

	function get_shape_string(block_shape, useParan) result(shape_string)
		implicit none

		integer, intent(in) :: block_shape(:)
		logical, optional, intent(in) :: useParan
		character(len=:), allocatable :: shape_string
		character(len=100) :: shape_str
		character(len=20) :: num_str
		character(len=3) :: symb
		integer :: i

		if (present(useParan) .and. useParan) then

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

		if (present(useParan) .and. useParan) then

			shape_string = '('//trim(adjustl(shape_str))//')'

		else

			shape_string = trim(adjustl(shape_str))

		end if

	end function get_shape_string


	! Subroutines for the writing interface
	! (they all do the same thing but for different input types)

	! 1D writing subroutines for BinWriter

	subroutine wb_S1_1D(this, block)
		implicit none

		! Takes an array that is to be written to the file, checks that is has the
		! correct properties and sends it to the writing subroutine as a byte array.

		class(BinWriter) :: this

		character, intent(in) :: block(:)

		! Make sure that this particular subroutine is the correct one to use for the specified file formatting
		call this%check_write_input_arr(in_type='S', in_precision=1, in_shape=shape(block))

		! Pack the array to a flat byte array and send it to the writing subroutine
		call this%write_bytearr(transfer(block, 'A', this%block_size))

	end subroutine wb_S1_1D


	subroutine wb_i1_1D(this, block)
		implicit none

		class(BinWriter) :: this

		integer*1, intent(in) :: block(:)

		call this%check_write_input_arr(in_type='i', in_precision=1, in_shape=shape(block))
		call this%write_bytearr(transfer(block, 'A', this%block_size))

	end subroutine wb_i1_1D

	subroutine wb_i2_1D(this, block)
		implicit none

		class(BinWriter) :: this

		integer*2, intent(in) :: block(:)

		call this%check_write_input_arr(in_type='i', in_precision=2, in_shape=shape(block))
		call this%write_bytearr(transfer(block, 'A', this%block_size))

	end subroutine wb_i2_1D

	subroutine wb_i4_1D(this, block)
		implicit none

		class(BinWriter) :: this

		integer*4, intent(in) :: block(:)

		call this%check_write_input_arr(in_type='i', in_precision=4, in_shape=shape(block))
		call this%write_bytearr(transfer(block, 'A', this%block_size))

	end subroutine wb_i4_1D

	subroutine wb_i8_1D(this, block)
		implicit none

		class(BinWriter) :: this

		integer*8, intent(in) :: block(:)

		call this%check_write_input_arr(in_type='i', in_precision=8, in_shape=shape(block))
		call this%write_bytearr(transfer(block, 'A', this%block_size))

	end subroutine wb_i8_1D


	subroutine wb_f4_1D(this, block)
		implicit none

		class(BinWriter) :: this

		real*4, intent(in) :: block(:)

		call this%check_write_input_arr(in_type='f', in_precision=4, in_shape=shape(block))
		call this%write_bytearr(transfer(block, 'A', this%block_size))

	end subroutine wb_f4_1D

	subroutine wb_f8_1D(this, block)
		implicit none

		class(BinWriter) :: this

		real*8, intent(in) :: block(:)

		call this%check_write_input_arr(in_type='f', in_precision=8, in_shape=shape(block))
		call this%write_bytearr(transfer(block, 'A', this%block_size))

	end subroutine wb_f8_1D

	subroutine wb_f16_1D(this, block)
		implicit none

		class(BinWriter) :: this

		real*16, intent(in) :: block(:)

		call this%check_write_input_arr(in_type='f', in_precision=16, in_shape=shape(block))
		call this%write_bytearr(transfer(block, 'A', this%block_size))

	end subroutine wb_f16_1D


	subroutine wb_c8_1D(this, block)
		implicit none

		class(BinWriter) :: this

		complex*8, intent(in) :: block(:)

		call this%check_write_input_arr(in_type='c', in_precision=8, in_shape=shape(block))
		call this%write_bytearr(transfer(block, 'A', this%block_size))

	end subroutine wb_c8_1D

	subroutine wb_c16_1D(this, block)
		implicit none

		class(BinWriter) :: this

		complex*16, intent(in) :: block(:)

		call this%check_write_input_arr(in_type='c', in_precision=16, in_shape=shape(block))
		call this%write_bytearr(transfer(block, 'A', this%block_size))

	end subroutine wb_c16_1D

	subroutine wb_c32_1D(this, block)
		implicit none

		class(BinWriter) :: this

		complex*32, intent(in) :: block(:)

		call this%check_write_input_arr(in_type='c', in_precision=32, in_shape=shape(block))
		call this%write_bytearr(transfer(block, 'A', this%block_size))

	end subroutine wb_c32_1D


	! 2D writing subroutines for BinWriter

	subroutine wb_S1_2D(this, block)
		implicit none

		class(BinWriter) :: this

		character, intent(in) :: block(:, :)

		call this%check_write_input_arr(in_type='S', in_precision=1, in_shape=shape(block))
		call this%write_bytearr(transfer(block, 'A', this%block_size))

	end subroutine wb_S1_2D


	subroutine wb_i1_2D(this, block)
		implicit none

		class(BinWriter) :: this

		integer*1, intent(in) :: block(:, :)

		call this%check_write_input_arr(in_type='i', in_precision=1, in_shape=shape(block))
		call this%write_bytearr(transfer(block, 'A', this%block_size))

	end subroutine wb_i1_2D

	subroutine wb_i2_2D(this, block)
		implicit none

		class(BinWriter) :: this

		integer*2, intent(in) :: block(:, :)

		call this%check_write_input_arr(in_type='i', in_precision=2, in_shape=shape(block))
		call this%write_bytearr(transfer(block, 'A', this%block_size))

	end subroutine wb_i2_2D

	subroutine wb_i4_2D(this, block)
		implicit none

		class(BinWriter) :: this

		integer*4, intent(in) :: block(:, :)

		call this%check_write_input_arr(in_type='i', in_precision=4, in_shape=shape(block))
		call this%write_bytearr(transfer(block, 'A', this%block_size))

	end subroutine wb_i4_2D

	subroutine wb_i8_2D(this, block)
		implicit none

		class(BinWriter) :: this

		integer*8, intent(in) :: block(:, :)

		call this%check_write_input_arr(in_type='i', in_precision=8, in_shape=shape(block))
		call this%write_bytearr(transfer(block, 'A', this%block_size))

	end subroutine wb_i8_2D


	subroutine wb_f4_2D(this, block)
		implicit none

		class(BinWriter) :: this

		real*4, intent(in) :: block(:, :)

		call this%check_write_input_arr(in_type='f', in_precision=4, in_shape=shape(block))
		call this%write_bytearr(transfer(block, 'A', this%block_size))

	end subroutine wb_f4_2D

	subroutine wb_f8_2D(this, block)
		implicit none

		class(BinWriter) :: this

		real*8, intent(in) :: block(:, :)

		call this%check_write_input_arr(in_type='f', in_precision=8, in_shape=shape(block))
		call this%write_bytearr(transfer(block, 'A', this%block_size))

	end subroutine wb_f8_2D

	subroutine wb_f16_2D(this, block)
		implicit none

		class(BinWriter) :: this

		real*16, intent(in) :: block(:, :)

		call this%check_write_input_arr(in_type='f', in_precision=16, in_shape=shape(block))
		call this%write_bytearr(transfer(block, 'A', this%block_size))

	end subroutine wb_f16_2D


	subroutine wb_c8_2D(this, block)
		implicit none

		class(BinWriter) :: this

		complex*8, intent(in) :: block(:, :)

		call this%check_write_input_arr(in_type='c', in_precision=8, in_shape=shape(block))
		call this%write_bytearr(transfer(block, 'A', this%block_size))

	end subroutine wb_c8_2D

	subroutine wb_c16_2D(this, block)
		implicit none

		class(BinWriter) :: this

		complex*16, intent(in) :: block(:, :)

		call this%check_write_input_arr(in_type='c', in_precision=16, in_shape=shape(block))
		call this%write_bytearr(transfer(block, 'A', this%block_size))

	end subroutine wb_c16_2D

	subroutine wb_c32_2D(this, block)
		implicit none

		class(BinWriter) :: this

		complex*32, intent(in) :: block(:, :)

		call this%check_write_input_arr(in_type='c', in_precision=32, in_shape=shape(block))
		call this%write_bytearr(transfer(block, 'A', this%block_size))

	end subroutine wb_c32_2D


	! 3D writing subroutines for BinWriter

	subroutine wb_S1_3D(this, block)
		implicit none

		class(BinWriter) :: this

		character, intent(in) :: block(:, :, :)

		call this%check_write_input_arr(in_type='S', in_precision=1, in_shape=shape(block))
		call this%write_bytearr(transfer(block, 'A', this%block_size))

	end subroutine wb_S1_3D


	subroutine wb_i1_3D(this, block)
		implicit none

		class(BinWriter) :: this

		integer*1, intent(in) :: block(:, :, :)

		call this%check_write_input_arr(in_type='i', in_precision=1, in_shape=shape(block))
		call this%write_bytearr(transfer(block, 'A', this%block_size))

	end subroutine wb_i1_3D

	subroutine wb_i2_3D(this, block)
		implicit none

		class(BinWriter) :: this

		integer*2, intent(in) :: block(:, :, :)

		call this%check_write_input_arr(in_type='i', in_precision=2, in_shape=shape(block))
		call this%write_bytearr(transfer(block, 'A', this%block_size))

	end subroutine wb_i2_3D

	subroutine wb_i4_3D(this, block)
		implicit none

		class(BinWriter) :: this

		integer*4, intent(in) :: block(:, :, :)

		call this%check_write_input_arr(in_type='i', in_precision=4, in_shape=shape(block))
		call this%write_bytearr(transfer(block, 'A', this%block_size))

	end subroutine wb_i4_3D

	subroutine wb_i8_3D(this, block)
		implicit none

		class(BinWriter) :: this

		integer*8, intent(in) :: block(:, :, :)

		call this%check_write_input_arr(in_type='i', in_precision=8, in_shape=shape(block))
		call this%write_bytearr(transfer(block, 'A', this%block_size))

	end subroutine wb_i8_3D


	subroutine wb_f4_3D(this, block)
		implicit none

		class(BinWriter) :: this

		real*4, intent(in) :: block(:, :, :)

		call this%check_write_input_arr(in_type='f', in_precision=4, in_shape=shape(block))
		call this%write_bytearr(transfer(block, 'A', this%block_size))

	end subroutine wb_f4_3D

	subroutine wb_f8_3D(this, block)
		implicit none

		class(BinWriter) :: this

		real*8, intent(in) :: block(:, :, :)

		call this%check_write_input_arr(in_type='f', in_precision=8, in_shape=shape(block))
		call this%write_bytearr(transfer(block, 'A', this%block_size))

	end subroutine wb_f8_3D

	subroutine wb_f16_3D(this, block)
		implicit none

		class(BinWriter) :: this

		real*16, intent(in) :: block(:, :, :)

		call this%check_write_input_arr(in_type='f', in_precision=16, in_shape=shape(block))
		call this%write_bytearr(transfer(block, 'A', this%block_size))

	end subroutine wb_f16_3D


	subroutine wb_c8_3D(this, block)
		implicit none

		class(BinWriter) :: this

		complex*8, intent(in) :: block(:, :, :)

		call this%check_write_input_arr(in_type='c', in_precision=8, in_shape=shape(block))
		call this%write_bytearr(transfer(block, 'A', this%block_size))

	end subroutine wb_c8_3D

	subroutine wb_c16_3D(this, block)
		implicit none

		class(BinWriter) :: this

		complex*16, intent(in) :: block(:, :, :)

		call this%check_write_input_arr(in_type='c', in_precision=16, in_shape=shape(block))
		call this%write_bytearr(transfer(block, 'A', this%block_size))

	end subroutine wb_c16_3D

	subroutine wb_c32_3D(this, block)
		implicit none

		class(BinWriter) :: this

		complex*32, intent(in) :: block(:, :, :)

		call this%check_write_input_arr(in_type='c', in_precision=32, in_shape=shape(block))
		call this%write_bytearr(transfer(block, 'A', this%block_size))

	end subroutine wb_c32_3D


	! Subroutines for the reading interface
	! (they all do the same thing but for different input types)

	! 1D reading subroutines for BinReader

	subroutine rb_S1_1D(this, block, i)
		implicit none

		! Takes an allocatable array and fills it with the content 
		! of data block nr. i, casted to the correct type and reshaped.
		! (Uses the previously read data block if i is not supplied.)

		class(BinReader) :: this

		character, allocatable, intent(inout) :: block(:)
		integer, intent(in), optional :: i

		! Make sure that this particular subroutine is the correct one to use for the given file
		call this%check_read_input_arr(in_type='S', in_precision=1, in_dim=1)

		! Allocate new memory for the data block array
		if (allocated(block)) deallocate(block)
		allocate(block(this%block_shape(1)))

		! Call the reading subroutine to update the byte array with the correct data
		if (present(i)) call this%read_to_bytearr(i)

		! Cast the bytes to the correct data type and reshape the block to its original shape
		block = reshape(transfer(this%bytearr, block), shape=[this%block_shape(1)])

	end subroutine rb_S1_1D


	subroutine rb_i1_1D(this, block, i)
		implicit none

		class(BinReader) :: this

		integer*1, allocatable, intent(inout) :: block(:)
		integer, intent(in), optional :: i

		call this%check_read_input_arr(in_type='i', in_precision=1, in_dim=1)

		if (allocated(block)) deallocate(block)
		allocate(block(this%block_shape(1)))

		if (present(i)) call this%read_to_bytearr(i)

		block = reshape(transfer(this%bytearr, block), shape=[this%block_shape(1)])

	end subroutine rb_i1_1D

	subroutine rb_i2_1D(this, block, i)
		implicit none

		class(BinReader) :: this

		integer*2, allocatable, intent(inout) :: block(:)
		integer, intent(in), optional :: i

		call this%check_read_input_arr(in_type='i', in_precision=2, in_dim=1)

		if (allocated(block)) deallocate(block)
		allocate(block(this%block_shape(1)))

		if (present(i)) call this%read_to_bytearr(i)

		block = reshape(transfer(this%bytearr, block), shape=[this%block_shape(1)])

	end subroutine rb_i2_1D

	subroutine rb_i4_1D(this, block, i)
		implicit none

		class(BinReader) :: this

		integer*4, allocatable, intent(inout) :: block(:)
		integer, intent(in), optional :: i

		call this%check_read_input_arr(in_type='i', in_precision=4, in_dim=1)

		if (allocated(block)) deallocate(block)
		allocate(block(this%block_shape(1)))

		if (present(i)) call this%read_to_bytearr(i)

		block = reshape(transfer(this%bytearr, block), shape=[this%block_shape(1)])

	end subroutine rb_i4_1D

	subroutine rb_i8_1D(this, block, i)
		implicit none

		class(BinReader) :: this

		integer*8, allocatable, intent(inout) :: block(:)
		integer, intent(in), optional :: i

		call this%check_read_input_arr(in_type='i', in_precision=8, in_dim=1)

		if (allocated(block)) deallocate(block)
		allocate(block(this%block_shape(1)))

		if (present(i)) call this%read_to_bytearr(i)

		block = reshape(transfer(this%bytearr, block), shape=[this%block_shape(1)])

	end subroutine rb_i8_1D


	subroutine rb_f4_1D(this, block, i)
		implicit none

		class(BinReader) :: this

		real*4, allocatable, intent(inout) :: block(:)
		integer, intent(in), optional :: i

		call this%check_read_input_arr(in_type='f', in_precision=4, in_dim=1)

		if (allocated(block)) deallocate(block)
		allocate(block(this%block_shape(1)))

		if (present(i)) call this%read_to_bytearr(i)

		block = reshape(transfer(this%bytearr, block), shape=[this%block_shape(1)])

	end subroutine rb_f4_1D

	subroutine rb_f8_1D(this, block, i)
		implicit none

		class(BinReader) :: this

		real*8, allocatable, intent(inout) :: block(:)
		integer, intent(in), optional :: i

		call this%check_read_input_arr(in_type='f', in_precision=8, in_dim=1)

		if (allocated(block)) deallocate(block)
		allocate(block(this%block_shape(1)))

		if (present(i)) call this%read_to_bytearr(i)

		block = reshape(transfer(this%bytearr, block), shape=[this%block_shape(1)])

	end subroutine rb_f8_1D

	subroutine rb_f16_1D(this, block, i)
		implicit none

		class(BinReader) :: this

		real*16, allocatable, intent(inout) :: block(:)
		integer, intent(in), optional :: i

		call this%check_read_input_arr(in_type='f', in_precision=16, in_dim=1)

		if (allocated(block)) deallocate(block)
		allocate(block(this%block_shape(1)))

		if (present(i)) call this%read_to_bytearr(i)

		block = reshape(transfer(this%bytearr, block), shape=[this%block_shape(1)])

	end subroutine rb_f16_1D


	subroutine rb_c8_1D(this, block, i)
		implicit none

		class(BinReader) :: this

		complex*8, allocatable, intent(inout) :: block(:)
		integer, intent(in), optional :: i

		call this%check_read_input_arr(in_type='c', in_precision=8, in_dim=1)

		if (allocated(block)) deallocate(block)
		allocate(block(this%block_shape(1)))

		if (present(i)) call this%read_to_bytearr(i)

		block = reshape(transfer(this%bytearr, block), shape=[this%block_shape(1)])

	end subroutine rb_c8_1D

	subroutine rb_c16_1D(this, block, i)
		implicit none

		class(BinReader) :: this

		complex*16, allocatable, intent(inout) :: block(:)
		integer, intent(in), optional :: i

		call this%check_read_input_arr(in_type='c', in_precision=16, in_dim=1)

		if (allocated(block)) deallocate(block)
		allocate(block(this%block_shape(1)))

		if (present(i)) call this%read_to_bytearr(i)

		block = reshape(transfer(this%bytearr, block), shape=[this%block_shape(1)])

	end subroutine rb_c16_1D

	subroutine rb_c32_1D(this, block, i)
		implicit none

		class(BinReader) :: this

		complex*32, allocatable, intent(inout) :: block(:)
		integer, intent(in), optional :: i

		call this%check_read_input_arr(in_type='c', in_precision=32, in_dim=1)

		if (allocated(block)) deallocate(block)
		allocate(block(this%block_shape(1)))

		if (present(i)) call this%read_to_bytearr(i)

		block = reshape(transfer(this%bytearr, block), shape=[this%block_shape(1)])

	end subroutine rb_c32_1D


	! 2D reading subroutines for BinReader

	subroutine rb_S1_2D(this, block, i)
		implicit none

		class(BinReader) :: this

		character, allocatable, intent(inout) :: block(:, :)
		integer, intent(in), optional :: i

		call this%check_read_input_arr(in_type='S', in_precision=1, in_dim=2)

		if (allocated(block)) deallocate(block)
		allocate(block(this%block_shape(1), this%block_shape(2)))

		if (present(i)) call this%read_to_bytearr(i)

		block = reshape(transfer(this%bytearr, block), shape=[this%block_shape(1), this%block_shape(2)], order=this%order_arr_2D)

	end subroutine rb_S1_2D


	subroutine rb_i1_2D(this, block, i)
		implicit none

		class(BinReader) :: this

		integer*1, allocatable, intent(inout) :: block(:, :)
		integer, intent(in), optional :: i

		call this%check_read_input_arr(in_type='i', in_precision=1, in_dim=2)

		if (allocated(block)) deallocate(block)
		allocate(block(this%block_shape(1), this%block_shape(2)))

		if (present(i)) call this%read_to_bytearr(i)

		block = reshape(transfer(this%bytearr, block), shape=[this%block_shape(1), this%block_shape(2)], order=this%order_arr_2D)

	end subroutine rb_i1_2D

	subroutine rb_i2_2D(this, block, i)
		implicit none

		class(BinReader) :: this

		integer*2, allocatable, intent(inout) :: block(:, :)
		integer, intent(in), optional :: i

		call this%check_read_input_arr(in_type='i', in_precision=2, in_dim=2)

		if (allocated(block)) deallocate(block)
		allocate(block(this%block_shape(1), this%block_shape(2)))

		if (present(i)) call this%read_to_bytearr(i)

		block = reshape(transfer(this%bytearr, block), shape=[this%block_shape(1), this%block_shape(2)], order=this%order_arr_2D)

	end subroutine rb_i2_2D

	subroutine rb_i4_2D(this, block, i)
		implicit none

		class(BinReader) :: this

		integer*4, allocatable, intent(inout) :: block(:, :)
		integer, intent(in), optional :: i

		call this%check_read_input_arr(in_type='i', in_precision=4, in_dim=2)

		if (allocated(block)) deallocate(block)
		allocate(block(this%block_shape(1), this%block_shape(2)))

		if (present(i)) call this%read_to_bytearr(i)

		block = reshape(transfer(this%bytearr, block), shape=[this%block_shape(1), this%block_shape(2)], order=this%order_arr_2D)

	end subroutine rb_i4_2D

	subroutine rb_i8_2D(this, block, i)
		implicit none

		class(BinReader) :: this

		integer*8, allocatable, intent(inout) :: block(:, :)
		integer, intent(in), optional :: i

		call this%check_read_input_arr(in_type='i', in_precision=8, in_dim=2)

		if (allocated(block)) deallocate(block)
		allocate(block(this%block_shape(1), this%block_shape(2)))

		if (present(i)) call this%read_to_bytearr(i)

		block = reshape(transfer(this%bytearr, block), shape=[this%block_shape(1), this%block_shape(2)], order=this%order_arr_2D)

	end subroutine rb_i8_2D


	subroutine rb_f4_2D(this, block, i)
		implicit none

		class(BinReader) :: this

		real*4, allocatable, intent(inout) :: block(:, :)
		integer, intent(in), optional :: i

		call this%check_read_input_arr(in_type='f', in_precision=4, in_dim=2)

		if (allocated(block)) deallocate(block)
		allocate(block(this%block_shape(1), this%block_shape(2)))

		if (present(i)) call this%read_to_bytearr(i)

		block = reshape(transfer(this%bytearr, block), shape=[this%block_shape(1), this%block_shape(2)], order=this%order_arr_2D)

	end subroutine rb_f4_2D

	subroutine rb_f8_2D(this, block, i)
		implicit none

		class(BinReader) :: this

		real*8, allocatable, intent(inout) :: block(:, :)
		integer, intent(in), optional :: i

		call this%check_read_input_arr(in_type='f', in_precision=8, in_dim=2)

		if (allocated(block)) deallocate(block)
		allocate(block(this%block_shape(1), this%block_shape(2)))

		if (present(i)) call this%read_to_bytearr(i)

		block = reshape(transfer(this%bytearr, block), shape=[this%block_shape(1), this%block_shape(2)], order=this%order_arr_2D)

	end subroutine rb_f8_2D

	subroutine rb_f16_2D(this, block, i)
		implicit none

		class(BinReader) :: this

		real*16, allocatable, intent(inout) :: block(:, :)
		integer, intent(in), optional :: i

		call this%check_read_input_arr(in_type='f', in_precision=16, in_dim=2)

		if (allocated(block)) deallocate(block)
		allocate(block(this%block_shape(1), this%block_shape(2)))

		if (present(i)) call this%read_to_bytearr(i)

		block = reshape(transfer(this%bytearr, block), shape=[this%block_shape(1), this%block_shape(2)], order=this%order_arr_2D)

	end subroutine rb_f16_2D


	subroutine rb_c8_2D(this, block, i)
		implicit none

		class(BinReader) :: this

		complex*8, allocatable, intent(inout) :: block(:, :)
		integer, intent(in), optional :: i

		call this%check_read_input_arr(in_type='c', in_precision=8, in_dim=2)

		if (allocated(block)) deallocate(block)
		allocate(block(this%block_shape(1), this%block_shape(2)))

		if (present(i)) call this%read_to_bytearr(i)

		block = reshape(transfer(this%bytearr, block), shape=[this%block_shape(1), this%block_shape(2)], order=this%order_arr_2D)

	end subroutine rb_c8_2D

	subroutine rb_c16_2D(this, block, i)
		implicit none

		class(BinReader) :: this

		complex*16, allocatable, intent(inout) :: block(:, :)
		integer, intent(in), optional :: i

		call this%check_read_input_arr(in_type='c', in_precision=16, in_dim=2)

		if (allocated(block)) deallocate(block)
		allocate(block(this%block_shape(1), this%block_shape(2)))

		if (present(i)) call this%read_to_bytearr(i)

		block = reshape(transfer(this%bytearr, block), shape=[this%block_shape(1), this%block_shape(2)], order=this%order_arr_2D)

	end subroutine rb_c16_2D

	subroutine rb_c32_2D(this, block, i)
		implicit none

		class(BinReader) :: this

		complex*32, allocatable, intent(inout) :: block(:, :)
		integer, intent(in), optional :: i

		call this%check_read_input_arr(in_type='c', in_precision=32, in_dim=2)

		if (allocated(block)) deallocate(block)
		allocate(block(this%block_shape(1), this%block_shape(2)))

		if (present(i)) call this%read_to_bytearr(i)

		block = reshape(transfer(this%bytearr, block), shape=[this%block_shape(1), this%block_shape(2)], order=this%order_arr_2D)

	end subroutine rb_c32_2D


	! 3D reading subroutines for BinReader

	subroutine rb_S1_3D(this, block, i)
		implicit none

		class(BinReader) :: this

		character, allocatable, intent(inout) :: block(:, :, :)
		integer, intent(in), optional :: i

		call this%check_read_input_arr(in_type='S', in_precision=1, in_dim=3)

		if (allocated(block)) deallocate(block)
		allocate(block(this%block_shape(1), this%block_shape(2), this%block_shape(3)))

		if (present(i)) call this%read_to_bytearr(i)

		block = reshape(transfer(this%bytearr, block), shape=[this%block_shape(1), this%block_shape(2), this%block_shape(3)], &
					  order=this%order_arr_3D)

	end subroutine rb_S1_3D


	subroutine rb_i1_3D(this, block, i)
		implicit none

		class(BinReader) :: this

		integer*1, allocatable, intent(inout) :: block(:, :, :)
		integer, intent(in), optional :: i

		call this%check_read_input_arr(in_type='i', in_precision=1, in_dim=3)

		if (allocated(block)) deallocate(block)
		allocate(block(this%block_shape(1), this%block_shape(2), this%block_shape(3)))

		if (present(i)) call this%read_to_bytearr(i)

		block = reshape(transfer(this%bytearr, block), shape=[this%block_shape(1), this%block_shape(2), this%block_shape(3)], &
					  order=this%order_arr_3D)

	end subroutine rb_i1_3D

	subroutine rb_i2_3D(this, block, i)
		implicit none

		class(BinReader) :: this

		integer*2, allocatable, intent(inout) :: block(:, :, :)
		integer, intent(in), optional :: i

		call this%check_read_input_arr(in_type='i', in_precision=2, in_dim=3)

		if (allocated(block)) deallocate(block)
		allocate(block(this%block_shape(1), this%block_shape(2), this%block_shape(3)))

		if (present(i)) call this%read_to_bytearr(i)

		block = reshape(transfer(this%bytearr, block), shape=[this%block_shape(1), this%block_shape(2), this%block_shape(3)], &
					  order=this%order_arr_3D)

	end subroutine rb_i2_3D

	subroutine rb_i4_3D(this, block, i)
		implicit none

		class(BinReader) :: this

		integer*4, allocatable, intent(inout) :: block(:, :, :)
		integer, intent(in), optional :: i

		call this%check_read_input_arr(in_type='i', in_precision=4, in_dim=3)

		if (allocated(block)) deallocate(block)
		allocate(block(this%block_shape(1), this%block_shape(2), this%block_shape(3)))

		if (present(i)) call this%read_to_bytearr(i)

		block = reshape(transfer(this%bytearr, block), shape=[this%block_shape(1), this%block_shape(2), this%block_shape(3)], &
					  order=this%order_arr_3D)

	end subroutine rb_i4_3D

	subroutine rb_i8_3D(this, block, i)
		implicit none

		class(BinReader) :: this

		integer*8, allocatable, intent(inout) :: block(:, :, :)
		integer, intent(in), optional :: i

		call this%check_read_input_arr(in_type='i', in_precision=8, in_dim=3)

		if (allocated(block)) deallocate(block)
		allocate(block(this%block_shape(1), this%block_shape(2), this%block_shape(3)))

		if (present(i)) call this%read_to_bytearr(i)

		block = reshape(transfer(this%bytearr, block), shape=[this%block_shape(1), this%block_shape(2), this%block_shape(3)], &
					  order=this%order_arr_3D)

	end subroutine rb_i8_3D


	subroutine rb_f4_3D(this, block, i)
		implicit none

		class(BinReader) :: this

		real*4, allocatable, intent(inout) :: block(:, :, :)
		integer, intent(in), optional :: i

		call this%check_read_input_arr(in_type='f', in_precision=4, in_dim=3)

		if (allocated(block)) deallocate(block)
		allocate(block(this%block_shape(1), this%block_shape(2), this%block_shape(3)))

		if (present(i)) call this%read_to_bytearr(i)

		block = reshape(transfer(this%bytearr, block), shape=[this%block_shape(1), this%block_shape(2), this%block_shape(3)], &
					  order=this%order_arr_3D)

	end subroutine rb_f4_3D

	subroutine rb_f8_3D(this, block, i)
		implicit none

		class(BinReader) :: this

		real*8, allocatable, intent(inout) :: block(:, :, :)
		integer, intent(in), optional :: i

		call this%check_read_input_arr(in_type='f', in_precision=8, in_dim=3)

		if (allocated(block)) deallocate(block)
		allocate(block(this%block_shape(1), this%block_shape(2), this%block_shape(3)))

		if (present(i)) call this%read_to_bytearr(i)

		block = reshape(transfer(this%bytearr, block), shape=[this%block_shape(1), this%block_shape(2), this%block_shape(3)], &
					  order=this%order_arr_3D)

	end subroutine rb_f8_3D

	subroutine rb_f16_3D(this, block, i)
		implicit none

		class(BinReader) :: this

		real*16, allocatable, intent(inout) :: block(:, :, :)
		integer, intent(in), optional :: i

		call this%check_read_input_arr(in_type='f', in_precision=16, in_dim=3)

		if (allocated(block)) deallocate(block)
		allocate(block(this%block_shape(1), this%block_shape(2), this%block_shape(3)))

		if (present(i)) call this%read_to_bytearr(i)

		block = reshape(transfer(this%bytearr, block), shape=[this%block_shape(1), this%block_shape(2), this%block_shape(3)], &
					  order=this%order_arr_3D)

	end subroutine rb_f16_3D


	subroutine rb_c8_3D(this, block, i)
		implicit none

		class(BinReader) :: this

		complex*8, allocatable, intent(inout) :: block(:, :, :)
		integer, intent(in), optional :: i

		call this%check_read_input_arr(in_type='c', in_precision=8, in_dim=3)

		if (allocated(block)) deallocate(block)
		allocate(block(this%block_shape(1), this%block_shape(2), this%block_shape(3)))

		if (present(i)) call this%read_to_bytearr(i)

		block = reshape(transfer(this%bytearr, block), shape=[this%block_shape(1), this%block_shape(2), this%block_shape(3)], &
					  order=this%order_arr_3D)

	end subroutine rb_c8_3D

	subroutine rb_c16_3D(this, block, i)
		implicit none

		class(BinReader) :: this

		complex*16, allocatable, intent(inout) :: block(:, :, :)
		integer, intent(in), optional :: i

		call this%check_read_input_arr(in_type='c', in_precision=16, in_dim=3)

		if (allocated(block)) deallocate(block)
		allocate(block(this%block_shape(1), this%block_shape(2), this%block_shape(3)))

		if (present(i)) call this%read_to_bytearr(i)

		block = reshape(transfer(this%bytearr, block), shape=[this%block_shape(1), this%block_shape(2), this%block_shape(3)], &
					  order=this%order_arr_3D)

	end subroutine rb_c16_3D

	subroutine rb_c32_3D(this, block, i)
		implicit none

		class(BinReader) :: this

		complex*32, allocatable, intent(inout) :: block(:, :, :)
		integer, intent(in), optional :: i

		call this%check_read_input_arr(in_type='c', in_precision=32, in_dim=3)

		if (allocated(block)) deallocate(block)
		allocate(block(this%block_shape(1), this%block_shape(2), this%block_shape(3)))

		if (present(i)) call this%read_to_bytearr(i)

		block = reshape(transfer(this%bytearr, block), shape=[this%block_shape(1), this%block_shape(2), this%block_shape(3)], &
					  order=this%order_arr_3D)

	end subroutine rb_c32_3D

end module PasIO

program main

	use PasIO
	implicit none

	type(BinWriter) :: writer
	type(BinReader) :: reader

	real*8 :: outarr(300, 700)
	real*8, allocatable :: inarr(:, :)
	integer :: i, j, k, n, sh(2)

	n = 2000

	do i = 1, size(outarr, dim=1)

		do j = 1, size(outarr, dim=2)

			outarr(i, j) = i + j !complex(i, j)

		end do

	end do

	!call writer%prepare_write('out_fortran.pas', shape(outarr), 'f8')
	call reader%prepare_read('out_python.pas')


	!call writer%print_header_info()
	!call reader%print_header_info()

	do k = 1, n

		!call writer%write_block(outarr)
		call reader%read_block_to(inarr, k)

	end do

	!call writer%print_header_info()
	!call reader%print_header_info()

	!call writer%end_write(printReport=.true.)
	call reader%end_read(printReport=.true.)

	!print *, inarr(1, 1:4)
	!print *, inarr(2, 1:4)
	!print *, inarr(3, 1:4)
	!print *, inarr(4, 1:4)

end program main