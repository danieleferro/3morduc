/*
 * HTTPFunctor.cpp
 *
 * This file is part of REAR.
 * Copyright (C) 2010 Daniele Ferro (daniele.ferro86@gmail.com) 
 *                    Loris Fichera (loris.fichera@gmail.com)
 *
 * REAR is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * REAR is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with REAR.  If not, see <http://www.gnu.org/licenses/>.
 */

#include "HTTPFunctor.h"

HTTPFunctor::HTTPFunctor() {


  this->_m_pBuffer = NULL;
  this->_m_pBuffer = (char*) malloc(MAX_FILE_LENGTH * sizeof(char));
  this->_m_Size = 0;

  this->_raw_image = NULL;


}


HTTPFunctor::~HTTPFunctor() {

  if (this->_m_pBuffer)
    free(this->_m_pBuffer);

  if (this->_raw_image)
    free(this->_raw_image);
}


void* HTTPFunctor::Realloc(void* ptr, size_t size) {
    
  if(ptr)
    return realloc(ptr, size);
  else
    return malloc(size);
}


size_t HTTPFunctor::WriteHTTPBodyCallback(char* ptr, size_t size, size_t nmemb) {

  // Calculate the real size of the incoming buffer
  size_t realsize = size * nmemb;
  
  // (Re)Allocate memory for the buffer
  _m_pBuffer = (char*) Realloc(_m_pBuffer, _m_Size + realsize);
  
  // Test if Buffer is initialized correctly & copy memory
  if (_m_pBuffer == NULL) {
    realsize = 0;
  }
  
  memcpy(&(_m_pBuffer[_m_Size]), ptr, realsize);
  _m_Size += realsize;
   
  // return the real size of the buffer...
  return realsize;

}


size_t HTTPFunctor::WriteHTTPHeaderCallback(char* ptr, size_t size, size_t nmemb) {

  // Calculate the real size of the incoming buffer
  size_t realsize = size * nmemb;
  
  _header = _header.append(ptr);
 
  // return the real size of the buffer...
  return realsize;

};


int HTTPFunctor::ReadHalfJPEGBuffer() {

  if (_m_pBuffer == NULL) {

    return -1;
  }


  // these are standard libjpeg structures for reading(decompression)
  struct jpeg_decompress_struct cinfo;
  struct jpeg_error_mgr jerr;

  // libjpeg data structure for storing one row, that is, scanline of an image
  JSAMPROW row_pointer[1];
    
  unsigned long location = 0;
  int i = 0;
    
  // here we set up the standard libjpeg error handler
  cinfo.err = jpeg_std_error( &jerr );
  
  // setup decompression process and source, then read JPEG header
  jpeg_create_decompress( &cinfo );
    
  // this makes the library read from char buffer
  jpeg_memory_src(&cinfo, (const JOCTET*) _m_pBuffer, _m_Size);
  
  // reading the image header which contains image information
  jpeg_read_header( &cinfo, TRUE );

  // Uncomment the following to output image information, if needed.

  /*
  printf( "JPEG File Information: \n" );
  printf( "Image width and height: %d pixels and %d pixels.\n", cinfo.image_width, cinfo.image_height );
  printf( "Color components per pixel: %d.\n", cinfo.num_components );
  printf( "Color space: %d.\n", cinfo.jpeg_color_space );
  */
	
  // start decompression jpeg here
  jpeg_start_decompress( &cinfo );

  // allocate memory to hold the uncompressed image
  // cinfo.image_width/2 because i wanto to split the image
  _raw_image = (unsigned char*)malloc( cinfo.output_width/2*cinfo.output_height*cinfo.num_components );

  // now actually read the jpeg into the raw buffer
  row_pointer[0] = (unsigned char *)malloc( cinfo.output_width*cinfo.num_components );


  // read one scan line at a time
  while( cinfo.output_scanline < cinfo.image_height ) {

    jpeg_read_scanlines( &cinfo, row_pointer, 1 );
    
    // cinfo.image_width/2 because i wanto to split the image
    for( i=0; i < (cinfo.image_width/2)*cinfo.num_components; i++) {

      _raw_image[location++] = row_pointer[0][i];
    }

  }
	
  // wrap up decompression, destroy objects, free pointers and close open files
  jpeg_finish_decompress( &cinfo );
  jpeg_destroy_decompress( &cinfo );
  free( row_pointer[0] );

  /* yup, we succeeded! */
  return 1;
}


int HTTPFunctor::WriteJPEGFile(const char *filename ) {

  struct jpeg_compress_struct cinfo;
  struct jpeg_error_mgr jerr;
  
  // this is a pointer to one row of image data
  JSAMPROW row_pointer[1];
  FILE *outfile = fopen( filename, "wb" );
	
  if ( !outfile ) {
    printf("Error opening output jpeg file %s\n!", filename );
    exit(1);
  }

  cinfo.err = jpeg_std_error( &jerr );
  jpeg_create_compress(&cinfo);
  jpeg_stdio_dest(&cinfo, outfile);
  
  // Setting the parameters of the output file here
  cinfo.image_width = 640;	
  cinfo.image_height = 480;
  cinfo.input_components = 3;
  cinfo.in_color_space = JCS_RGB;
  
  // default compression parameters, we shouldn't be worried about these
  jpeg_set_defaults( &cinfo );


  // now do the compression
  jpeg_start_compress( &cinfo, TRUE );

  
  // like reading a file, this time write one row at a time
  while (cinfo.next_scanline < cinfo.image_height) {

    row_pointer[0] = &_raw_image[ cinfo.next_scanline * cinfo.image_width *  cinfo.input_components];
    jpeg_write_scanlines( &cinfo, row_pointer, 1 );
    
  }

  // similar to read file, clean up after we're done compressing
  jpeg_finish_compress( &cinfo );
  jpeg_destroy_compress( &cinfo );
  fclose( outfile );


  /* success code is 1! */
  return 1;
}



std::string HTTPFunctor::GetOdometryString() {


  std::string search = "Server";
  size_t init_index;
  size_t last_index;

  // get init_index for "Server"
  init_index = _header.rfind(search);

  if (init_index != std::string::npos) {

    // get last_index for "Server" (find CLRF)
    last_index = _header.find_first_of('\n', init_index);

    
    // return substring found
    return _header.substr(init_index, (last_index-init_index+1));

  }
  else {

    printf("Error, impossible to parse HTTP header.\n");
    exit(1);    
  }


}


void HTTPFunctor::ResetFunctor() {

  if (this->_m_pBuffer) {
    free(this->_m_pBuffer);
    this->_m_pBuffer = NULL;
  }

  if (this->_raw_image) {
    free(this->_raw_image);
    this->_raw_image = NULL;
  }  

  this->_header = "";

  this->_m_Size = 0;

}

void HTTPFunctor::CreateImage(const char * filename) {

  ReadHalfJPEGBuffer();
  WriteJPEGFile(filename);
}
