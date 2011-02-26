/*
 * DataLogicMorduc.cpp
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
#include <curlpp/cURLpp.hpp>
#include <curlpp/Easy.hpp>
#include <curlpp/Options.hpp>
#include <curlpp/Exception.hpp>



DataLogicMorduc::DataLogicMorduc(const char* url, const char* path_data) {

  _index = 1;
  
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

  // open new file per odometry data
  std::string odom_file_path = _path_data  + "odometric.txt";
  odom_file = fopen(odom_file_path.c_str(), "wt");  

  if (odom_file == NULL ) {
    std::cout << "Error on opening \n" << odom_file_path.c_str()
	      << std::endl << "Program will terminate." << std::endl;
    exit(1);
  }
  fclose(odom_file);


}

DataLogicMorduc::~DataLogicMorduc() {

  delete &(_images_collection);
  fclose(odom_file);
}

void DataLogicMorduc::RetrieveData(robot_data * data) {


  std::ostringstream o;
  // img path
  o << _path_data << "img" << _index << ".jpg";

  std::string command_URL = _path_server + "stereo.jpg";
  std::string image_path = o.str();
  std::string line_read;

  image_data grabbed_frame_data;
  
  // STEP 1: REQUEST DATA FROM SERVER

  try {

    curlpp::Cleanup cleaner;
    curlpp::Easy request;

    _http_functor.ResetFunctor();

    
    // Set the writer callback to enable cURL 
    // to write result in a memory area
    curlpp::types::WriteFunctionFunctor functor_body(&_http_functor, 
						     &HTTPFunctor::WriteHTTPBodyCallback);
      
    curlpp::types::WriteFunctionFunctor functor_header(&_http_functor, 
						       &HTTPFunctor::WriteHTTPHeaderCallback);
    
      

    // header options' list
    std::list<std::string> headers;
    headers.push_back("User-Agent: REAR client"); 

      
    // Setting request options
    request.setOpt(new curlpp::options::Url(command_URL));
    request.setOpt(new curlpp::options::Verbose(false));
    request.setOpt(new curlpp::options::WriteFunction(functor_body));
    request.setOpt(new curlpp::options::HeaderFunction(functor_header));
    request.setOpt(new curlpp::options::HttpHeader(headers));
    
    
    // send request
    request.perform();
      
    line_read = _http_functor.GetOdometryString();

    if (__DATA_LOGIC_MORDUC__DBG)
      std::cout << "Response from move command (robot data):" << std::endl
		<< line_read << std::endl;

    
    _http_functor.CreateImage(image_path.c_str());
    
  }
  
  catch (curlpp::LogicError & e) {
    std::cout << "Error retriving data from server: "
	      << e.what() << std::endl;
    exit(1);
  }
  
  catch (curlpp::RuntimeError & e) {

    std::cout << "Error retriving data from server: "
	      << e.what() << std::endl;
    exit(1);
  }

  
  // STEP 2: WRITE ODOMETRIC DATA TO FILE AND FILL ROBOT DATA
  
  // open new file per odometry data
  std::string odom_file_path = _path_data  + "odometric.txt";
  odom_file = fopen(odom_file_path.c_str(), "at");  

  if (odom_file == NULL ) {
    std::cout << "Error on opening \n" << odom_file_path.c_str()
	      << std::endl << "Program will terminate." << std::endl;
    exit(1);
  }

  fputs(line_read.c_str(), odom_file);
  fclose(odom_file);

  FillOdometricData(line_read, data);

  // STEP 3: insert image in collection
   
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

void DataLogicMorduc::Command(int command) {


  // send the command to the robot

  std::string command_URL;
  std::ostringstream os;

  switch (command) {

  case FORWARD:
    command_URL = _path_server + "stereo.fow1.jpg";
    break;

  case BACKWARD:
    command_URL = _path_server + "stereo.bak1.jpg";
    break;

  case RIGHT:
    command_URL = _path_server + "stereo.rgt.jpg";
    break;

  case LEFT:
    command_URL = _path_server + "stereo.lft.jpg";
    break;

  default:
    std::cout << "Unknown command: " << command
	      << std::endl << "Program will terminate."
	      << std::endl;

    exit(1);
      
  }

 
  try {

    curlpp::Cleanup cleaner;
    curlpp::Easy request;

    // header options' list
    std::list<std::string> headers;
    headers.push_back("User-Agent: REAR client"); 

    // Setting request options
    request.setOpt(new curlpp::options::Url(command_URL));
    request.setOpt(new curlpp::options::Verbose(false));
    request.setOpt(new curlpp::options::HttpHeader(headers));
    // redirect response to os
    request.setOpt(new curlpp::options::WriteStream(&os));    

    
    // send request
    request.perform();
      
    // if (__DATA_LOGIC_MORDUC__DBG)
    //   std::cout << "Response from command request:" << std::endl
    // 		<< os.str() << std::endl;


  }
  
  catch (curlpp::LogicError & e) {
    std::cout << "Error sending command data to server: "
	      << e.what() << std::endl;
    exit(1);
  }
  
  catch (curlpp::RuntimeError & e) {

    std::cout << "Error sending command data to server: "
	      << e.what() << std::endl;
    exit(1);
  }

  
     
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

void DataLogicMorduc::FillOdometricData(std::string line_read, robot_data* data) {

  int counter;

  // get tokens from string
  counter = line_read.find_first_of('/', 0) + 1;
  line_read = line_read.substr(counter);


  for (int j=0; j < line_read.length(); j++) {

    if (line_read[j] == ',')
      line_read[j] = '.';
  }

  std::cout << line_read << std::endl;

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
  data->y = atof(line_read.substr(0, counter).c_str()) * MAGNITUDE * -1;
  line_read = line_read.substr(counter);

  // get theta (server returns angle in radiant)
  counter = line_read.find_first_of('\\', 0) + 1;
  data->theta = TO_DEGREES( atof(line_read.substr(0, counter).c_str())) ;
  line_read = line_read.substr(counter);


  if (__DATA_LOGIC_MORDUC__DBG) {

    std::cout << "X value: " << data->x << std::endl;
    std::cout << "Y value: " << data->y << std::endl;
    std::cout << "Theta value: " << data->theta << std::endl;
    std::cout << "Time value: " << data->time << std::endl;
  }

  return;

}
