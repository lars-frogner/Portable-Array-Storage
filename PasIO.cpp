#include "PasIO.h"

BinWriter::BinWriter(char const* filename, int block_dim, int* block_shape, int block_size, char dtype, int dbytes): filename(filename), block_shape(block_shape), block_size(block_size), dtype(dtype), dbytes(dbytes) {

	/*
	Opens binary file and writes a header. The header consists of the following entries:
	
	body_dim: 						       Number of dimensions of the body array (everything following the header)
	block_num:						       Total number of data blocks in the body
	block_shape[0], ... , block_shape[-1]: Number of elements along each dimension of a data block
	dtype:								   Char indicating the data type of the elements in a data block
	dbytes: 						       Number of bytes used by an element in a data block
	packtype:							   Character indicating the way the arrays have been flattened ("F": Fortran style, "C": C++ style)

	The shape of the body array is thus ( block_num, block_shape[0], ... , block_shape[-1] ). All header entries are
	32 bit integers except dtype and packtype, which are 8 bit characters.
	*/

	block_num = 0;	// Set counter to zero
	tot_time = 0.0;	// Set total writing time to zero

	char packtype = 'C';

	// Body array dimension is one larger than data block dimension
	body_dim = block_dim + 1;

	// Open binary file for writing output
	outfile.open(filename, std::ios::out | std::ios::trunc | std::ios::binary);

	// Write first header entry
	outfile.write((char*)&body_dim, 4);

	// Skip second header entry (not yet determined)
	outfile.seekp(8, std::ios::beg);

	// Write remaining header entries
	outfile.write((char*)block_shape, block_dim*4);
	outfile.write(&dtype, 1);
	outfile.write((char*)&dbytes, 4);
	outfile.write(&packtype, 1);
}

void BinWriter::write_block(char* block) {

	/*
	Adds a data block to the end of the file.
	*/

	// Get initial clock
	clock_t clock_start = clock();

	// Write data block
	outfile.write(block, block_size);

	// Add elapsed time to total reading time
	tot_time += (double)(clock() - clock_start)/CLOCKS_PER_SEC;

	// Increase counter
	block_num++;
}

void BinWriter::end_write(bool printReport) {

	/*
	Adds the second header number, closes the file and prints writing info.
	*/

	// Go to second header number
	outfile.seekp(4, std::ios::beg);

	// Write number of blocks written
	outfile.write((char*)&block_num, 4);

	// Close file
	outfile.close();

	if (printReport) {

		// Print title
		printf("\n*** Writing report for \"%s\" ***\n", filename);

		// Print amount of data written

		float tot_size = block_num*(block_size/1024000.0);

		printf("Amount of data written: %g MB\n", tot_size);

		// Print array shape

		int block_dim = body_dim - 1;
		char buffer[100];
		std::string shape_str = "(";

		sprintf(buffer, "%d", block_num);
		shape_str += buffer;

		for (int i = 0; i < block_dim; i++) {

			sprintf(buffer, ", %d", block_shape[i]);
			shape_str += buffer;
		}
		shape_str += ")";

		std::cout << "Data array shape: " << shape_str << std::endl;

		// Print data type

		std::string dtype_str;
		std::string type_out_str = "Data type: ";

		if (dtype == 'u') {

			dtype_str = "unsigned integer";
		}
		else if (dtype == 'i') {

			dtype_str = "signed integer";
		}
		else if (dtype == 'f') {

			dtype_str = "float";
		}

		sprintf(buffer, "%d bit ", 8*dbytes);
		type_out_str += buffer;
		type_out_str += dtype_str;

		std::cout << type_out_str << std::endl;

		if (tot_time != 0) {

			// Print writing time
			printf("Total writing time: %g s\n", tot_time);

			// Print writing speed
			printf("Average writing speed: %.3g MB/s\n", tot_size/tot_time);
		}
	}
}

int main() {

	int dims[2] = {500, 500};
	double arr[dims[0]][dims[1]];

	for (int i = 0; i < dims[0]; i++) {
		for (int j = 0; j < dims[1]; j++) {

			arr[i][j] = 0.0;
		}
	}

	clock_t clock_start = clock();

	BinWriter writer("out_cpp.bin", sizeof(dims)/sizeof(dims[0]), dims, sizeof(arr), 'f', 8);

	for (int n = 0; n < 10000; n++) {

		writer.write_block((char*)arr);
	}

	writer.end_write(true);

	printf("\nTotal time: %g s", ((double)(clock()- clock_start)/CLOCKS_PER_SEC));

	return 0;
}