/*
 * DataManager.cpp
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

#include "DataManager.h"

DataManager::DataManager(Robot * robot, IDataLogic * logic, Camera * camera, 
			 IImageSelector * calculator)
{
  _rob = robot;
  _logic = logic;
  _camera = camera;
  _calculator = calculator;

  _robot_status = (robot_data *) malloc(sizeof(robot_data));
  _bg_image_data = (image_data *) malloc(sizeof(image_data));

  if ( _robot_status == NULL ||
       _bg_image_data == NULL )
    {
      std::cout << "DataManager: failed to allocate memory \
                    for internal data." 
		<< std::endl << "exiting." << std::endl;
      exit(-1);
    }

  FirstStep();
}

DataManager::~DataManager()
{
  free(_robot_status);
  free(_bg_image_data);
}

void DataManager::FirstStep() {

  /*
  image_data old_image;

  old_image.x = _bg_image_data -> x;
  old_image.y = _bg_image_data -> y;
  old_image.theta = _bg_image_data -> theta;
  */

  // retrieve first data from robot
  _logic->RetrieveData(_robot_status);

  // move robot with _robot_status data
  // theta in degrees
  _rob->Place(_robot_status->x,
	      _robot_status->y,
	      _robot_status->theta ); 
    
  _logic->SelectImage(_robot_status, _bg_image_data,
		      _calculator);

  // move camera
  _camera -> SetPosition( _bg_image_data -> x,
			  9.f,
			  _bg_image_data -> y);
  
  _camera -> SetYAngle( _bg_image_data -> theta - 90);
  

  /* use image_path to load the image */
  LoadGLTextures(_texture, _bg_image_data->path);

  std::cout << "--** Data collected successfully (first step)."
	    << std::endl << std::endl;

}




void DataManager::NextStep(int command) {

  image_data old_image;

  old_image.x = _bg_image_data -> x;
  old_image.y = _bg_image_data -> y;
  old_image.theta = _bg_image_data -> theta;


  // send command to the robot and then retrieve new data
  _logic->Command(command);
  _logic->RetrieveData(_robot_status);

  // move robot with _robot_status data
  // theta in degrees
  _rob->Place(_robot_status->x,
	      _robot_status->y,
	      _robot_status->theta ); 
    
  _logic->SelectImage(_robot_status, _bg_image_data,
		      _calculator);

  /* use data in out_element to change
     camera position
  */   

  if ( old_image.x != _bg_image_data -> x ||
       old_image.y != _bg_image_data -> y ||
       old_image.theta != _bg_image_data -> theta )
    {
      
      //std::cout << "*** Moving the camera... ***" << std::endl;
      //std::cout <<  _bg_image_data->theta - old_image.theta << std::endl;

      _camera -> SetPosition( _bg_image_data -> x,
			      9.f,
			      _bg_image_data -> y);
      
      _camera -> SetYAngle( _bg_image_data -> theta - 90);
    }

  /* use image_path to load the image */
  LoadGLTextures(_texture, _bg_image_data->path);
  
  std::cout << "--** Command sended and data collected successfully."
	    << std::endl << std::endl;
}

    
void DataManager::LoadGLTextures(GLuint * texture, const char* filename) {	

  std::string temp_filename = filename;
  short point_index = temp_filename.find_last_of('.');
  

  temp_filename = temp_filename.substr(point_index, temp_filename.length()); 

  if (!temp_filename.compare(".png")) {
  
    //std::cout << "A png image" << std::endl;
    /* load image from png file */
    * texture = loadPNGImage(filename);

  }
  else {
    if (!temp_filename.compare(".jpg") || !temp_filename.compare(".jpeg")) {
      
      //std::cout << "A jpeg image" << std::endl;
      /* load image from jpeg file */
      * texture = loadJPEGImage(filename);

    }
    
    else {
      // unknown type
      std::cout << "Not a valide texture image format." << std::endl;
      exit(1);
    }

  }

  if (!*texture) {
    std::cout << "Texture value: " << *texture << std::endl;
    exit(1);
  }        
   
  // Bind 2d texture (x and y size)
  glBindTexture(GL_TEXTURE_2D, *texture);   

}
