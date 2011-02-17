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

DataLogicLogMorduc::DataLogicLogMorduc(int a)
{
  _simulation_session = a;
  _index = 1;
}

DataLogicLogMorduc::~DataLogicLogMorduc()
{
  delete &(_images_collection);
}

void DataLogicLogMorduc::RetrieveData(robot_data * data)
{
  std::ostringstream o;
  image_data grabbed_frame_data;
  /*
  // pointer to text file containing data
  FILE * position_data;
  char line[50];
  
  std::string * line_read;
  std::string line_values[4];
  std::string position_data_name;

  int time;
  int line_number = _index;

  // grabbed image metadata
  
  o << "../log_morduc/log_" << _simulation_session << "/data_" << _simulation_session << ".txt";
  position_data_name = o.str();

  position_data = fopen(position_data_name.c_str(), "rt");

  while(fgets(line, 50, position_data) &&
	line_number > 1)
    {
      line_number--;
    }

  line_read = new std::string(line);

  std::string buf;
  std::stringstream ss(*line_read);
  // Create vector to hold our words
  std::vector<std::string> tokens;
  
  // put token in vector element
  while (ss >> buf)
    tokens.push_back(buf);
  
  
  data->x = atof ( tokens[0].c_str() );
  data->y = atof( tokens[1].c_str() );
  data->theta = TO_DEGREES(- atof ( tokens[2].c_str() ));
  data->time = atof ( tokens[3].c_str() );

  time = (int) data->time;

  */

  data->x = 0;
  data->y = 0;
  data->theta = 0;
  data->time = 0;


  /* clear the stream and 
     add a new value to it */
  o.str("");
  o.clear();
  o << "../log_morduc/log_" << _simulation_session << "/screenshot_" << 1 << ".jpg";

  // fill grabbed frame metadata
  grabbed_frame_data.x = data->x;
  grabbed_frame_data.y = data->y;
  grabbed_frame_data.theta = data->theta;
  grabbed_frame_data.time = data->time;

  strcpy(grabbed_frame_data.path, o.str().c_str());

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
			    IImageSelector * calculator)
{

  // call the "calculator" instance to choose the right image

  // since our data are already stored with vector,
  // we simply pass its reference
  calculator->ChooseImage(robot_status, bg_image_data, &_images_collection);


}
