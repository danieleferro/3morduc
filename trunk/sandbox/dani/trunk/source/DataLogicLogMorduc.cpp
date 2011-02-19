/*
 * DataLogicLogSimulator.cpp
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

#include "DataLogicLogMorduc.h"

DataLogicLogMorduc::DataLogicLogMorduc(int session) {

  _simulation_session = session;
  _index = 0;
  _raw_image = NULL;
}

DataLogicLogMorduc::~DataLogicLogMorduc() {

  delete &(_images_collection);
}

void DataLogicLogMorduc::RetrieveData(robot_data * data) {

  image_data grabbed_frame_data;
  std::string image_path;

  /* STEP 1: retrieve data from file (server) */
  GetOdometricData(data);
  
  std::cout << "X value: " << data->x << std::endl;
  std::cout << "Y value: " << data->y << std::endl;
  std::cout << "Theta value: " << data->theta << std::endl;
  std::cout << "Time value: " << data->time << std::endl;

  /* STEP 2: get single image path */
  image_path = GetSingleImage();
   
  // fill grabbed frame metadata
  grabbed_frame_data.x = data->x;
  grabbed_frame_data.y = data->y;
  grabbed_frame_data.theta = data->theta;
  grabbed_frame_data.time = data->time;

  strcpy(grabbed_frame_data.path, image_path.c_str());

  // store the collected metadata if it's not already stored
  for (std::vector<image_data>::iterator it =
	 _images_collection.begin();
       it != _images_collection.end();
       it++)
    {
      if ( (*it).time == grabbed_frame_data.time )
	{
	  return;
	}
    }

  _images_collection.push_back(grabbed_frame_data);
  
  

  return;

}

void DataLogicLogMorduc::Command(int command) {


  /* sends the command to the robot
     (in our case read next line from file)
  */
     
  // increase index to point the next line of the file
  _index++;

}


// select the image to set as background using the euclidean metric
void DataLogicLogMorduc::SelectImage(robot_data * robot_status, image_data * bg_image_data,
				     IImageSelector * calculator) {

  // call the "calculator" instance to choose the right image

  // since our data are already stored with vector,
  // we simply pass its reference
  calculator->ChooseImage(robot_status, bg_image_data, &_images_collection);


}


void DataLogicLogMorduc::GetOdometricData(robot_data* data) {

  /* this function will create two files,
     one with odometry data string and
     one with coupled image */

  std::ostringstream o;

  // file with all odometry for this log session
  FILE * odom_data_in;

  // char array and string where to store a text line
  char        line[150];
  std::string line_read;

  // number line to read
  int line_number = _index;

  // temp counter
  int counter;

  // path to read odometric info
  o << "../log_morduc/log_" << _simulation_session << "/odometric.txt";

  // open text file in read mode (rt)
  odom_data_in = fopen(o.str().c_str(), "rt");

  // read file line
  while(fgets(line, 150, odom_data_in) &&
	line_number > 1) {
    
    line_number--;
  }

  line_read = line;

  // get tokens from string
  counter = line_read.find_first_of('/', 0) + 1;
  line_read = line_read.substr(counter);

  // std::cout << line_read << std::endl;

  // get time
  counter = line_read.find_first_of('\\', 0) + 1;
  data->time = atof(line_read.substr(0, counter).c_str());
  line_read = line_read.substr(counter);

  // get x
  counter = line_read.find_first_of('\\', 0) + 1;
  data->x = atof(line_read.substr(0, counter).c_str()) * MAGNITUDE;
  line_read = line_read.substr(counter);

  // get y
  counter = line_read.find_first_of('\\', 0) + 1;
  data->y = atof(line_read.substr(0, counter).c_str()) * MAGNITUDE;
  line_read = line_read.substr(counter);

  // get theta
  counter = line_read.find_first_of('\\', 0) + 1;
  data->theta = atof(line_read.substr(0, counter).c_str()) * MAGNITUDE;
  line_read = line_read.substr(counter);


  // std::cout << line_read << std::endl;

  return;
  
}


int DataLogicLogMorduc::ReadHalfJPEGFile(const char * filename) {

  /* these are standard libjpeg structures for reading(decompression) */
  struct jpeg_decompress_struct cinfo;
  struct jpeg_error_mgr jerr;
  
  /* libjpeg data structure for storing one row, that is, scanline of an image */
  JSAMPROW row_pointer[1];
  
  FILE *infile = fopen( filename, "rb" );
  unsigned long location = 0;
  int i = 0;
  
  if ( !infile ) {
    printf("Error opening jpeg file %s\n!", filename );
    return -1;
  }

  /* here we set up the standard libjpeg error handler */
  cinfo.err = jpeg_std_error( &jerr );
  
  /* setup decompression process and source, then read JPEG header */
  jpeg_create_decompress( &cinfo );
  
  /* this makes the library read from infile */
  jpeg_stdio_src( &cinfo, infile );
  
  /* reading the image header which contains image information */
  jpeg_read_header( &cinfo, TRUE );
  
  /* Uncomment the following to output image information, if needed. */
  
  /*
  printf( "JPEG File Information: \n" );
  printf( "Image width and height: %d pixels and %d pixels.\n", cinfo.image_width, cinfo.image_height );
  printf( "Color components per pixel: %d.\n", cinfo.num_components );
  printf( "Color space: %d.\n", cinfo.jpeg_color_space );
  */
	
  /* Start decompression jpeg here */
  jpeg_start_decompress( &cinfo );

  /* allocate memory to hold the uncompressed image */
  // cinfo.image_width/2 because i wanto to split the image
  _raw_image = (unsigned char*)malloc( cinfo.output_width/2*cinfo.output_height*cinfo.num_components );
  
  /* now actually read the jpeg into the raw buffer */
  row_pointer[0] = (unsigned char *)malloc( cinfo.output_width*cinfo.num_components );
  
  /* read one scan line at a time */
  while( cinfo.output_scanline < cinfo.image_height ) {
    jpeg_read_scanlines( &cinfo, row_pointer, 1 );
    
    // cinfo.image_width/2 because i wanto to split the image
    for( i=0; i < (cinfo.image_width/2)*cinfo.num_components; i++) 
      _raw_image[location++] = row_pointer[0][i];
  }
  
  /* wrap up decompression, destroy objects, free pointers and close open files */
  jpeg_finish_decompress( &cinfo );
  jpeg_destroy_decompress( &cinfo );
  free( row_pointer[0] );
  fclose( infile );

  /* yup, we succeeded! */
  return 1;
}


int DataLogicLogMorduc::WriteJPEGFile(const char* filename, int width, int height) {

  struct jpeg_compress_struct cinfo;
  struct jpeg_error_mgr jerr;
  
  /* this is a pointer to one row of image data */
  JSAMPROW row_pointer[1];
  FILE *outfile = fopen( filename, "wb" );
  
  if ( !outfile ) {
    printf("Error opening output jpeg file %s\n!", filename );
    return -1;
  }

  cinfo.err = jpeg_std_error( &jerr );
  jpeg_create_compress(&cinfo);
  jpeg_stdio_dest(&cinfo, outfile);
  
  /* Setting the parameters of the output file here */
  cinfo.image_width = width;	
  cinfo.image_height = height;
  cinfo.input_components = 3;  /* or 1 for GRACYSCALE images */
  cinfo.in_color_space = JCS_RGB; /* or JCS_GRAYSCALE for grayscale images */
  
  /* default compression parameters, we shouldn't be worried about these */
  jpeg_set_defaults( &cinfo );
  
  /* Now do the compression .. */
  jpeg_start_compress( &cinfo, TRUE );
  
  /* like reading a file, this time write one row at a time */
  while( cinfo.next_scanline < cinfo.image_height ) {
    row_pointer[0] = &_raw_image[ cinfo.next_scanline * cinfo.image_width *  cinfo.input_components];
    jpeg_write_scanlines( &cinfo, row_pointer, 1 );
  }
  
  /* similar to read file, clean up after we're done compressing */
  jpeg_finish_compress( &cinfo );
  jpeg_destroy_compress( &cinfo );
  fclose( outfile );
  
  /* success code is 1! */
  return 1;
  
}



std::string DataLogicLogMorduc::GetSingleImage() {

  int ret;

  std::ostringstream o;

  // path image to read
  o << "../log_morduc/log_" << _simulation_session << "/img" << _index << ".jpg";
  ret = ReadHalfJPEGFile(o.str().c_str());
  
  if (ret != 1) {
    std::cout << "Image not read !" << std::endl;
    // return previous path
    _index--;

    o.str("");
    o.clear();
    o << "../log_morduc/log_" << _simulation_session << "/img" << _index << ".jpg";
    return o.str();
  }

  o.str("");
  o.clear();

  // path image to write (half the previous one)
  o << "../log_morduc/log_" << _simulation_session << "/half_img" << _index << ".jpg";
  ret = WriteJPEGFile(o.str().c_str(), 640, 480);

  if (ret != 1)
    std::cout << "Error writing JPEG image !" << std::endl;

  
  
  // return half image path
  return o.str();


}
