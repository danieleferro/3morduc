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

#include "DataLogicMorduc.h"

DataLogicMorduc::DataLogicMorduc(const char* url, const char* path_data) {

  _index = 0;
  
  odom_file = NULL;
  img_file = NULL;

  _path_server = url;
  _path_data = path_data;

  // add last slash if missing
  if (_path_server[_path_server.length()-1] != '/')
    _path_server = _path_server.append("/");

  // add last slash if missing
  if (_path_data[_path_data.length()-1] != '/')
    _path_data = _path_data.append("/");



  _raw_image = NULL;
  
  // open new file per odometry data
  std::string odom_file_path = _path_data  + "odometry.txt";
  odom_file = fopen(odom_file_path.c_str(), "wt");  

  if (odom_file == NULL ) {
    std::cout << "Error on opening \n" << odom_file_path.c_str()
	      << std::endl << "Program will terminate." << std::endl;
    exit(1);
  }



}

DataLogicMorduc::~DataLogicMorduc() {

  delete &(_images_collection);
  fclose(odom_file);
}


size_t DataLogicMorduc::WriteToFile(void *ptr, size_t size, size_t nmemb, void *stream) {


  //std::cout << "HEADER" << std::endl;

  int written = fwrite(static_cast<const char*>(ptr), size, nmemb, (FILE *)stream);
  return written;

}

static size_t WriteToFile2(void *ptr, size_t size, size_t nmemb, void *stream) {


  std::cout << "CALLBACK" << std::endl;

  //int written = fwrite(ptr, size, nmemb, (FILE *)stream);
  return size*nmemb;

}


void DataLogicMorduc::RetrieveData(robot_data * data) {


  WriteFilesFromServer();
  GetOdometricData(data);

  /*

  std::cout << "X value: " << data->x << std::endl;
  std::cout << "Y value: " << data->y << std::endl;
  std::cout << "Theta value: " << data->theta << std::endl;
  std::cout << "Time value: " << data->time << std::endl;

  /* STEP 2: get single image path
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
  
  */  

  return;

}

void DataLogicMorduc::Command(int command) {


  /* sends the command to the robot
     (in our case read next line from file)
  */
     
  // increase index to point the next line of the file
  _index++;

}


// select the image to set as background using the euclidean metric
void DataLogicMorduc::SelectImage(robot_data * robot_status, image_data * bg_image_data,
				     IImageSelector * calculator) {

  // call the "calculator" instance to choose the right image

  // since our data are already stored with vector,
  // we simply pass its reference
  calculator->ChooseImage(robot_status, bg_image_data, &_images_collection);


}


void DataLogicMorduc::GetOdometricData(robot_data* data) {

  data->x     = 100;
  data->y     = 100;
  data->theta = 10;
  data->time  = 10;



  /*
  // char array and string where to store a text line
  char        line[150];
  std::string line_read;
  int         counter = 0;

  // read file line
  if (_index <= _index_max)
    fgets(line, 150, odom_file);

  else {

    data->x     = _last_robot_data.x;
    data->y     = _last_robot_data.y;
    data->theta = _last_robot_data.theta;
    data->time  = _last_robot_data.time;

    return;

  }

  // from char * to string
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


  // std::cout << _index << " -->" << line_read << std::endl;

  // save returned data
  _last_robot_data.x     = data->x;
  _last_robot_data.y     = data->y;
  _last_robot_data.theta = data->theta;
  _last_robot_data.time  = data->time;


  */
  return;
  
}


int DataLogicMorduc::ReadHalfJPEGFile() {

  /* libjpeg data structure for storing one row, that is, scanline of an image */
  JSAMPROW row_pointer[1];
  
  unsigned long location = 0;
  int i = 0;

  /* Start decompression jpeg here */
  jpeg_start_decompress(&_decomp_cinfo);

  /* allocate memory to hold the uncompressed image */
  // cinfo.image_width/2 because i wanto to split the image
  _raw_image = (unsigned char*) malloc(_decomp_cinfo.output_width/2*
				      _decomp_cinfo.output_height*
				      _decomp_cinfo.num_components );
  
  /* now actually read the jpeg into the raw buffer */
  row_pointer[0] = (unsigned char *) malloc(_decomp_cinfo.output_width*
					    _decomp_cinfo.num_components);
  
  /* read one scan line at a time */
  while (_decomp_cinfo.output_scanline < _decomp_cinfo.image_height) {

    jpeg_read_scanlines( &_decomp_cinfo, row_pointer, 1 );
    
    // cinfo.image_width/2 because i wanto to split the image
    for( i=0; i < (_decomp_cinfo.image_width/2)*_decomp_cinfo.num_components; i++)

      _raw_image[location++] = row_pointer[0][i];

  }
  
  /* wrap up decompression, destroy objects, free pointers and close open files */
  jpeg_finish_decompress( &_decomp_cinfo );
  jpeg_destroy_decompress( &_decomp_cinfo );
  free( row_pointer[0] );

  /* yup, we succeeded! */
  return 1;
}


int DataLogicMorduc::WriteJPEGFile() {

  /* this is a pointer to one row of image data */
  JSAMPROW row_pointer[1];

  _comp_cinfo.err = jpeg_std_error(&_jerr );
  jpeg_create_compress(&_comp_cinfo);
  jpeg_stdio_dest(&_comp_cinfo, img_file);
  
  /* Setting the parameters of the output file here */
  _comp_cinfo.image_width = 640;	
  _comp_cinfo.image_height = 480;
  _comp_cinfo.input_components = 3;  /* or 1 for GRACYSCALE images */
  _comp_cinfo.in_color_space = JCS_RGB; /* or JCS_GRAYSCALE for grayscale images */
  
  /* default compression parameters, we shouldn't be worried about these */
  jpeg_set_defaults(&_comp_cinfo);
  
  /* Now do the compression .. */
  jpeg_start_compress(&_comp_cinfo, TRUE);
  
  /* like reading a file, this time write one row at a time */
  while (_comp_cinfo.next_scanline < _comp_cinfo.image_height) {

    row_pointer[0] = &_raw_image[_comp_cinfo.next_scanline*
				 _comp_cinfo.image_width*
				 _comp_cinfo.input_components];

    jpeg_write_scanlines(&_comp_cinfo, row_pointer, 1);

  }
  
  /* similar to read file, clean up after we're done compressing */
  jpeg_finish_compress(&_comp_cinfo);
  jpeg_destroy_compress(&_comp_cinfo);
  
  /* success code is 1! */
  return 1;
  
}


void DataLogicMorduc::WriteFilesFromServer() {

  CURL *curl_handle;
  CURLcode curl_res;
  // CURLINFO info;
  std::string command_URL =_path_server  + "stereo.jpg";
  std::string headerfilename = _path_data + "head_out";
  std::string bodyfilename = _path_data + "body_out";


  FILE *headerfile;

  FILE *bodyfile;

  // init curl session
  curl_global_init(CURL_GLOBAL_ALL);
  curl_handle = curl_easy_init();

  // set url to download\n");
  curl_easy_setopt(curl_handle, CURLOPT_URL, command_URL.c_str());

  // send all data to this function
  curl_easy_setopt(curl_handle, CURLOPT_WRITEFUNCTION, &DataLogicMorduc::WriteToFile);

  /* open the files */ 
  headerfile = fopen(headerfilename.c_str(),"w");
  if (headerfile == NULL) {

    printf("Error to open file \"head_out\"\n");
    curl_easy_cleanup(curl_handle);
    exit(2);

  }

  bodyfile = fopen(bodyfilename.c_str(),"w");
  if (bodyfile == NULL) {

    printf("Error to open file \"body_out.txt\"\n");
    curl_easy_cleanup(curl_handle);
    exit(2);
  }


  // header to this file handle 
  curl_easy_setopt(curl_handle, CURLOPT_WRITEHEADER, headerfile);

  // we want the data (body) to this file handle 
  curl_easy_setopt(curl_handle, CURLOPT_WRITEDATA, bodyfile);

  // download the file
  curl_res = curl_easy_perform(curl_handle);

  if (curl_res != 0) {

    printf("Error in dowloading files from server.\n");
    fclose(bodyfile);
    fclose(headerfile);
    curl_easy_cleanup(curl_handle);
  }


  
  /*
  long http_code;
  double c_length;  

  printf("Get http return code\n");
  curl_easy_getinfo(curl_handle, CURLINFO_RESPONSE_CODE, &http_code);
  printf("http code: %lu\n", http_code);

  printf("Get size of download page\n");
  curl_easy_getinfo(curl_handle, CURLINFO_SIZE_DOWNLOAD, &c_length);
  printf("length: %g\n", c_length);
  */

  

  // close all files and sessions
  fclose(bodyfile);
  fclose(headerfile);

  curl_easy_cleanup(curl_handle);

  printf("END WRITE FILES FROM SERVER");

}



std::string DataLogicMorduc::GetSingleImage() {

  /*
  int ret;
  std::ostringstream o;

  if (_index > _index_max)
    // no more image, return last
    return _last_image_path;


  // path image to read
  o << "../log_morduc/log_" << _simulation_session << "/img" << _index << ".jpg";

  // open a binary file for reading
  img_file = fopen(o.str().c_str(), "rb" );  

  if ( img_file == NULL ) {
    std::cout << "Error on opening \n" << o.str()
	      << std::endl << "Program will terminate." << std::endl;
    exit(1);
  }

  // set up the standard libjpeg error handler
  _decomp_cinfo.err = jpeg_std_error(&_jerr);
  
  // setup decompression process and source, then read JPEG header
  jpeg_create_decompress(&_decomp_cinfo);
  
  // this makes the library read from img_file
  jpeg_stdio_src(&_decomp_cinfo, img_file);
  
  // reading the image header which contains image information
  jpeg_read_header(&_decomp_cinfo, TRUE);
  
  // Uncomment the following to output image information, if needed.
  
  
//   printf( "JPEG File Information: \n" );
//   printf( "Image width and height: %d pixels and %d pixels.\n", cinfo.image_width, cinfo.image_height );
//   printf( "Color components per pixel: %d.\n", cinfo.num_components );
//   printf( "Color space: %d.\n", _decomp_cinfo.jpeg_color_space );
    
  
  if (_decomp_cinfo.image_width == 1280 && _decomp_cinfo.image_height == 480) {

    // call functions to override image with 640x480 one

    ReadHalfJPEGFile();
    fclose(img_file);

    // reopen binary file for writing.
    img_file = fopen(o.str().c_str(), "wb" );  
    WriteJPEGFile();

  }
  else {
    
    if (_decomp_cinfo.image_width == 640 && _decomp_cinfo.image_height == 480) {

      // dimension image are right
      jpeg_destroy_decompress(&_decomp_cinfo);
    }
    else {

      // invalid cases
      std::cout << "Image \n" << o.str()
		<< " has not the proper dimension (1280x480 or 640x480)."
		<< std::endl << "Program will terminate." << std::endl;
      fclose(img_file);
      exit(1);

    }

  }

  
  fclose(img_file);
  free(_raw_image);
  _last_image_path = o.str();
  
  return o.str();
  */

  return "ciao";

}
