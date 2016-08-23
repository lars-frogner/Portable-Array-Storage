#include <iostream>
#include <fstream>
#include <ctime>

class BinWriter {

	/*
	Class for writing array data of arbitrary dimensionality to a binary file.
	*/

	// Define private class members
	private:
		
		int block_size; // Total number of bytes used by each block of data
		int block_num;  // Counter to keep track of the number of data blocks written to the file
		int dbytes;	    // Number of bytes used by each element in a data block
		int body_dim;	// Number of dimensions of body array

		int* block_shape; // Pointer to array containing the number of elements along each dimension of a data block

		std::ofstream outfile; // Stream for output file

		char dtype; // Data type ('u': unsigned int, 'i': signed int, 'f': float)

		char const* filename; // File name

		double tot_time; // Total writing time


	// Defince public class members
	public:

		BinWriter(char const*, int, int*, int, char, int); // Constructor
		void write_block(char*);
		void end_write(bool);
};