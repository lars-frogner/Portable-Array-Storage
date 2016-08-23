import numpy as np
import sys, os
import time
import struct

class BinWriter:

	'''
	Class for writing array data of arbitrary dimensionality to a binary file.
	'''

	def __init__(self, filename, block_shape, dtype_dbytes):

		'''
		Opens binary file and writes a header. The header consists of the following entries:
	
		block_dim: 						       Number of dimensions of the data block arrays
		block_num:						       Total number of data blocks in the body
		block_shape[0], ... , block_shape[-1]: Number of elements along each dimension of a data block
		dtype:								   Character indicating the data type of the elements in a data block
		dbytes:								   Number of bytes used by an element in a data block
		packtype:							   Character indicating the way the arrays have been flattened
											   ("F": Fortran style, "C": C++ style)

		All header entries are 32 bit integers except dtype and packtype, which are 8 bit characters.
		'''

		self.block_shape = np.asarray(block_shape, dtype='i4') # Set data block shape
		self.dtype_dbytes = dtype_dbytes		     	       # Set element data type and precision

		self.block_num = 0	       					# Set counter to zero
		self.write_time = 0		   					# Set total writing time to zero

		# Make sure that the correct file extension is used

		fn_ext = '' if not '.' in filename else filename.split('.')[1]
		fn_noext = filename.split('.')[0]

		if len(fn_ext) > 0:

			if fn_ext != 'pas':

				print 'BinWriter warning (\"%s\"): Invalid file extension. Using \".pas\" instead.' \
					  % filename

		self.filename = fn_noext + '.pas'

		# Check that the input string specifying the data type and precision is complete

		if len(dtype_dbytes) < 2:

			print 'BinWriter error (\"%s\"): Invalid input \"%s\" for data type/precision. ' % (self.filename, dtype_dbytes) \
				  + 'Must be a string consisting of a letter (describing the type) followed ' \
				  + 'by a number (describing the precision), e.g. "i4" for the 4 byte signed integer type.'
			sys.exit()

		try:

			self.dtype = dtype_dbytes[0]
			self.dbytes = int(dtype_dbytes[1:])

		except ValueError:

			print 'BinWriter error (\"%s\"): Invalid input \"%s\" for data type/precision. ' % (self.filename, dtype_dbytes) \
				  + 'The part following \"%s\" must be a number.' % (dtype_dbytes[0])
			sys.exit()

		# Make sure that the specified data type and precision is valid

		if not self.dtype in ['S', 'i', 'f', 'u', 'c']:

			print 'BinWriter error (\"%s\"): Invalid data type (\"%s\") . ' % (self.filename, self.dtype) \
				  + 'Valid types are \"S\" (character), \"i\" (signed integer), \"f\" (float), \"u\" (unsigned integer) and \"c\" (complex).'
			sys.exit()

		if self.dtype == 'S' and self.dbytes != 1:

			print 'BinWriter error (\"%s\"): Invalid data precision (%d bytes) for character type. ' % (self.filename, self.dbytes) \
				  + 'The only valid precision is 1 byte.'
			sys.exit()

		elif self.dtype == 'i' and not self.dbytes in [1, 2, 4, 8]:

			print 'BinWriter error (\"%s\"): Invalid data precision (%d bytes) for signed integer type. ' % (self.filename, self.dbytes) \
				  + 'Valid precisions are 1, 2, 4 and 8 bytes.'
			sys.exit()

		elif self.dtype == 'f' and not self.dbytes in [2, 4, 8]:

			print 'BinWriter error (\"%s\"): Invalid data precision (%d bytes) for float type. ' % (self.filename, self.dbytes) \
				  + 'Valid precisions are 2, 4 and 8 bytes.'
			sys.exit()

		elif self.dtype == 'u' and not self.dbytes in [1, 2, 4, 8]:

			print 'BinWriter error (\"%s\"): Invalid data precision (%d bytes) for unsigned integer type. ' % (self.filename, self.dbytes) \
				  + 'Valid precisions are 1, 2, 4 and 8 bytes.'
			sys.exit()

		elif self.dtype == 'c' and not self.dbytes in [8, 16]:

			print 'BinWriter error (\"%s\"): Invalid data precision (%d bytes) for complex type. ' % (self.filename, self.dbytes) \
				  + 'Valid precisions are 8 and 16 bytes.'
			sys.exit()

		# Block array dimension
		self.block_dim = len(self.block_shape)

		# Start measuring time
		self.start_time = time.time()

		# Open binary file for writing output
		self.f = open(self.filename, 'wb')

		# Write first header entry
		self.f.write(struct.pack('i4', self.block_dim))

		# Skip second header entry (not yet determined)
		self.f.seek(8, os.SEEK_SET)

		# Write remaining header entries
		self.block_shape.tofile(self.f, sep='')
		self.f.write(struct.pack('c1', self.dtype))
		self.f.write(struct.pack('i4', self.dbytes))
		self.f.write(struct.pack('c1', 'C'))

	def write_block(self, block):

		'Adds a data block to the end of the file.'

		# Get initial time
		t0 = time.time()

		# Write data block
		np.asarray(block, dtype=self.dtype_dbytes).tofile(self.f, sep='')

		# Add elapsed time to total time
		self.write_time += time.time() - t0

		# Increase counter
		self.block_num += 1

	def print_header_info(self):

		print '\n*** Header info for \"%s\" ***' % (self.filename)
		print 'Data block dimension:: %dD' % (self.block_dim)
		print 'Total number of data blocks: %d' % (self.block_num)
		print 'Data block shape: ' + Common.get_shape_string(self.block_shape, useParan=True)
		print 'Data type: ' + Common.dtype_descript[self.dtype]
		print 'Number of bytes per element: %d' % (self.dbytes)
		print 'Pack type: C++/Python style'

		print 'I live in BinWriter!'

	def end_write(self, printReport=False):

		'Adds the second header number, closes the file and prints writing info.'

		# Go to second header number
		self.f.seek(4, os.SEEK_SET)

		# Write number of blocks written
		self.f.write(struct.pack('i4', self.block_num))

		# Close file
		self.f.close()

		# Stop measuring time
		self.stop_time = time.time()

		if printReport:

			# Print title
			print '\n*** Writing report for \"%s\" ***' % (self.filename)

			# Print block info
			print 'Data blocks: %s %s (%d bit precision)' % (Common.get_shape_string(self.block_shape), 
															 Common.dtype_descript[self.dtype], 
															 8*self.dbytes)

			# Print number of blocks written
			print 'Number of blocks written: %d' % self.block_num

			# Print total amount of data written
			tot_size = self.dbytes*self.block_num*np.prod(self.block_shape.astype('i8'))/1024000.0
			print 'Amount of data written: %g MB' % tot_size

			if self.write_time != 0:

				# Print writing time
				print 'Total writing time: %g s' % (self.write_time)

				# Print writing speed
				print 'Average writing speed: %.3g MB/s' % (tot_size/self.write_time)

			# Print amount of time file was open for
			print 'File open for: %.3g s' % (self.stop_time - self.start_time)


class BinReader:

	'''
	Class for reading binary files.
	'''

	def __init__(self, filename):

		'''
		Opens file and reads header.
		'''

		# Make sure that the correct file extension is used

		fn_ext = '' if not '.' in filename else filename.split('.')[1]
		fn_noext = filename.split('.')[0]

		if len(fn_ext) > 0:

			if fn_ext != 'pas':

				print 'BinReader error (\"%s\"): Invalid file extension. Can only read \".pas\" files.' \
					  % filename
				sys.exit()

		else:

			print 'BinReader warning (\"%s\"): No file extension supplied. Assuming extension is \".pas\".' % filename

		self.filename = fn_noext + '.pas'

		# Start measuring time
		self.start_time = time.time()

		# Open binary file
		self.f = open(self.filename, 'rb')

		# Read array dimensions from header
		self.block_dim = int(np.fromfile(self.f, count=1, dtype='i4'))

		# Move read indicator to next header entry
		self.f.seek(4, os.SEEK_SET)

		# Read array shape numbers
		self.body_shape = np.fromfile(self.f, count=(self.block_dim + 1), dtype='i4')

		# Move read indicator to next header entry
		self.f.seek(4*(self.block_dim + 2), os.SEEK_SET)

		# Read element data type
		self.dtype = str(np.fromfile(self.f, count=1, dtype='a1')[0])

		# Move read indicator to next header entry
		self.f.seek(4*(self.block_dim + 2) + 1, os.SEEK_SET)

		# Read element precision (number of bytes for each element)
		self.dbytes = int(np.fromfile(self.f, count=1, dtype='i4'))

		# Move read indicator to final header entry
		self.f.seek(4*(self.block_dim + 3) + 1, os.SEEK_SET)

		# Read pack type
		self.packtype = str(np.fromfile(self.f, count=1, dtype='a1')[0])

		# Print error and abort if the data type or precision in the file cannot be read

		if not self.dtype in ['S', 'i', 'f', 'u', 'c']:

			print 'BinReader error (\"%s\"): Cannot read the data type of this file (\"%s\") . ' % (self.filename, self.dtype) \
				  + 'Can only read \"S\" (character), \"i\" (signed integer), \"f\" (float), \"u\" (unsigned integer) and \"c\" (complex).'
			self.f.close()
			sys.exit()

		if self.dtype == 'S' and self.dbytes != 1:

			print 'BinReader error (\"%s\"): Cannot read the data precision of this file (%d byte). ' % (self.filename, self.dbytes) \
				  + 'Can only read 1 byte precision for character type.'
			self.f.close()
			sys.exit()

		elif self.dtype == 'i' and not self.dbytes in [1, 2, 4, 8]:

			print 'BinReader error (\"%s\"): Cannot read the data precision of this file (%d byte). ' % (self.filename, self.dbytes) \
				  + 'Can only read 1, 2, 4 and 8 byte precision for signed integer type.'
			self.f.close()
			sys.exit()

		elif self.dtype == 'f' and not self.dbytes in [2, 4, 8]:

			print 'BinReader error (\"%s\"): Cannot read the data precision of this file (%d byte). ' % (self.filename, self.dbytes) \
				  + 'Can only read 2, 4 and 8 byte precision for float type.'
			self.f.close()
			sys.exit()

		elif self.dtype == 'u' and not self.dbytes in [1, 2, 4, 8]:

			print 'BinReader error (\"%s\"): Cannot read the data precision of this file (%d byte). ' % (self.filename, self.dbytes) \
				  + 'Can only read 1, 2, 4 and 8 byte precision for unsigned integer type.'
			self.f.close()
			sys.exit()

		elif self.dtype == 'c' and not self.dbytes in [8, 16]:

			print 'BinReader error (\"%s\"): Cannot read the data precision of this file (%d byte). ' % (self.filename, self.dbytes) \
				  + 'Can only read 8 and 16 byte precision for complex type.'
			self.f.close()
			sys.exit()

		# Print warning if the stated pack type is unknown

		if not self.packtype.upper() in ['C', 'F']:

			print 'BinReader warning (\"%s\"): Pack type \"%s\" not recognized. Assuming C++/Python style flattening.' \
				  % self.packtype

		self.dtype_dbytes = self.dtype + ('%d' % self.dbytes)   # Data type and precision in body array
		self.start = 4*(self.block_dim + 3) + 2 		        # Position of first value in body
		self.block_shape = self.body_shape[1:] 			        # Shape of the data blocks
		self.block_num = self.body_shape[0]				        # Total number of data blocks
		self.block_len = np.prod(self.block_shape.astype('i8')) # Number of elements in each data block
		self.block_size = self.dbytes*self.block_len		    # Number of bytes in each data block

		self.read_count = 0
		self.read_time = 0

	def read_all(self):

		'''
		Reads the entire body array and stores the array in an attribute.
		'''

		# Move read indicator to beginning of body
		self.f.seek(self.start, os.SEEK_SET)

		try:

			# Get initial time
			t0 = time.time()

			# Create array containing all the body data
			data = np.fromfile(self.f, dtype=self.dtype_dbytes)

			# Reshape array
			if self.block_num == 1:

				data = data.reshape(self.block_shape, order=self.packtype)

			else:

				data = data.reshape(self.body_shape, order=self.packtype)

		except IOError:

			print 'BinReader error (\"%s\"): File is too large to read all at once. Read in blocks instead.' \
				   % (self.filename)
			self.f.close()
			sys.exit()

		# Add elapsed time to total time
		self.read_time += time.time() - t0

		# Set number of blocks read
		self.read_count = self.block_num

		return data

	def read_block(self, i):

		'''
		Reads block nr. i of the body array and stores the array in an attribute.
		'''

		# Move read indicator to beginning of data block i
		self.f.seek(self.start + i*self.block_size, os.SEEK_SET)

		# Get initial time
		t0 = time.time()

		# Create array containing data in block i
		data = np.fromfile(self.f, count=self.block_len, dtype=self.dtype_dbytes).reshape(self.block_shape, order=self.packtype)

		# Add elapsed time to total time
		self.read_time += time.time() - t0

		# Increase counter
		self.read_count += 1

		return data

	def print_header_info(self):

		print '\n*** Header info for \"%s\" ***' % (self.filename)
		print 'Data block dimension: %dD' % (self.block_dim)
		print 'Total number of data blocks: %d' % (self.block_num)
		print 'Data block shape: ' + Common.get_shape_string(self.block_shape, useParan=True)
		print 'Data type: ' + Common.dtype_descript[self.dtype]
		print 'Number of bytes per element: %d' % (self.dbytes)
		print 'Pack type: ' + ('Fortran style' if self.packtype.upper() == 'F' else 'C++/Python style')

		print 'I live in BinReader!'

	def end_read(self, printReport=False):

		'''
		Closes the file and prints reading info.
		'''

		# Close file
		self.f.close()

		# Stop measuring time
		self.stop_time = time.time()

		if printReport:

			# Print title
			print '\n*** Reading report for \"%s\" ***' % (self.filename)

			# Print block info
			print 'Data blocks: %s %s (%d bit precision)' % (Common.get_shape_string(self.block_shape), 
															 Common.dtype_descript[self.dtype], 
															 8*self.dbytes)

			# Print number of blocks read
			print 'Number of blocks read: %d/%d' % (self.read_count, self.block_num)

			# Print amount of data read
			read_size = self.read_count*self.block_size/1024000.0
			tot_size = self.block_num*self.block_size/1024000.0
			print 'Amount of data read: %g/%g MB' % (read_size, tot_size)

			if self.read_time != 0:

				# Print reading time
				print 'Total reading time: %g s' % (self.read_time)

				# Print reading speed
				print 'Average reading speed: %g MB/s' % (read_size/self.read_time)

			# Print amount of time file was open for
			print 'File open for: %.3g s' % (self.stop_time - self.start_time)


class Restrictor:

	'''
	Class for overwriting some methods from BinWriter and BinReader 
	to prevent them from being called from a BinAppender instance.
	'''

	def end_write(self, printReport=False):

		raise AttributeError

	def read_all(self):

		raise AttributeError

	def read_block(self, i):

		raise AttributeError

	def end_read(self, printReport=False):

		raise AttributeError


class BinAppender(Restrictor, BinWriter, BinReader):

	'''
	Class for appending array data of arbitrary dimensionality to 
	a binary file. Data blocks can be appended by calling the 
	method "write_block", inherited from the BinWriter class.
	'''

	def __init__(self, filename, block_shape, dtype_dbytes):

		'Opens a binary file, reads the header and prepares to append data to the file.'

		# Call constructor of the writing class to get header data
		BinReader.__init__(self, filename)

		# Close the file (which is opened in reading mode)
		self.f.close()

		# Check that the data to append has the same format as the existing data

		if (np.asarray(block_shape, dtype='i4') != self.block_shape).any():

			print 'BinAppender error (\"%s\"): Input block shape [%s] must match that of the blocks already written to the file [%s].' \
				  % (self.filename, Common.get_shape_string(np.asarray(block_shape, dtype='i4')), self.block_shape)
			sys.exit()

		if dtype_dbytes != self.dtype_dbytes:

			print 'BinAppender error (\"%s\"): Input data type and precision (\"%s\") must match that of the blocks already written to the file (\"%s\").' \
				  % (self.filename, dtype_dbytes, self.dtype_dbytes)
			sys.exit()

		if self.packtype.upper() != 'C':

			print 'BinAppender error (\"%s\"): Pack type of file to append to must be C++/Python style.' % (self.filename)
			sys.exit()

		# Start measuring time
		self.start_time = time.time()

		# Open the file in append mode
		self.f = open(self.filename, 'ab')

		self.write_time = 0
		self.block_num_orig = self.block_num
		self.block_num = 0

	def end_append(self, printReport=False):

		'Updates the second header number, closes the file and prints writing info.'

		# Close the file (which is in append mode)
		self.f.close()

		# Stop measuring time
		self.stop_time = time.time()

		# Reopen the file in a mode that permits overwriting
		self.f = open(self.filename, 'r+b')

		# Go to second header number
		self.f.seek(4, os.SEEK_SET)

		# Update total number of blocks written
		self.f.write(struct.pack('i4', self.block_num_orig + self.block_num))

		# Close file
		self.f.close()

		if printReport:

			# Print title
			print '\n*** Writing report for \"%s\" (append mode) ***' % (self.filename)

			# Print block info
			print 'Data blocks: %s %s (%d bit precision)' % (Common.get_shape_string(self.block_shape), 
															 Common.dtype_descript[self.dtype], 
															 8*self.dbytes)

			# Print number of blocks appended
			print 'Number of blocks appended: %d (total is now %d)' % (self.block_num, 
																	   self.block_num + self.block_num_orig)

			# Print total amount of data appended
			block_size_MB = self.dbytes*np.prod(self.block_shape.astype('i8'))/1024000.0
			tot_size = self.block_num*block_size_MB
			print 'Amount of data appended: %g MB (total is now %g)' % (tot_size, 
																		tot_size + self.block_num_orig*block_size_MB)

			if self.write_time != 0:

				# Print writing time
				print 'Total writing time: %g s' % (self.write_time)

				# Print writing speed
				print 'Average writing speed: %.3g MB/s' % (tot_size/self.write_time)

			# Print amount of time file was open for
			print 'File open for: %.3g s' % (self.stop_time - self.start_time)


class Common:

	'Namespace available to all the classes in this module.'

	# Dictionary with type descriptions
	dtype_descript = {'S':'character', 'i':'signed integer', 'f':'float', 'u':'unsigned integer', 'c':'complex'}

	@staticmethod
	def get_shape_string(arr, useParan=False):

		'Returns a pretty string representation of an input 1D array.'

		symb = ', ' if useParan else ' x '

		arr_str = '%d' % (arr[0])

		for val in arr[1:]:
			arr_str += '%s%d' % (symb, val)

		if useParan: arr_str = '(' + arr_str + ')'

		return arr_str


if __name__ == '__main__':

	arr = np.ones((300, 700), dtype='f8')

	n = 2000
	'''
	for i in xrange(arr.shape[0]):
		for j in xrange(arr.shape[1]):

			arr[i, j] = i + j + 2

	writer = BinWriter('out_python.pas', arr.shape, 'f8')

	writer.print_header_info()

	for k in xrange(n):
		
		writer.write_block(arr)

	writer.print_header_info()

	writer.end_write(printReport=1)
	'''
	reader = BinReader('out_python.pas')

	for k in xrange(n):

		data = reader.read_block(k)

	reader.end_read(printReport=1)

	#print data.shape
	#print data[0, :4]
	#print data[1, :4]
	#print data[2, :4]
	#print data[3, :4]
	#'''
	'''
	apppender = BinAppender('out_python.pas', arr.shape, 'f8')

	for k in xrange(n):
		
		apppender.write_block(arr)

	apppender.end_append(printReport=1)
	#'''