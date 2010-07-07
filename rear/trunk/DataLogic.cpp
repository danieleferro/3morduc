#include "DataLogic.h"

DataLogic::DataLogic(int session)
{
  _simulation_session = session;
  _index = -1;
}

DataLogic::~DataLogic()
{
  delete &(_images_collection);
}

void DataLogic::RetrieveData(robot_data * data)
{
  // pointer to text file containing data
  FILE * position_data;
  char line[50];
  
  std::string * line_read;
  std::string line_values[4];
  std::string position_data_name;
  std::ostringstream o;
  
  int time;
  int line_number = _index;

  // grabbed image metadata
  image_data grabbed_frame_data;
  
  o << "../log/log_" << _simulation_session << "/data_" << _simulation_session << ".txt";
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

//   data->x = atof ( tokens[1].c_str() );
//   data->y = - atof ( tokens[0].c_str() );
  data->x = atof ( tokens[0].c_str() );
  data->y = atof( tokens[1].c_str() );
  data->theta = TO_DEGREES(- atof ( tokens[2].c_str() ));
  data->time = atof ( tokens[3].c_str() );

  time = (int) data->time;

  /* clear the stream and 
     add a new value to it */
  o.str("");
  o.clear();
  o << "../log/log_" << _simulation_session << "/screenshot_" << _simulation_session << "_" << time << ".png";

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

void DataLogic::Command(int command) {


  /* sends the command to the robot
     (in our case read next line from file)
  */
     
  // increase index to point the next line of the file
  _index++;


}


// select the image to set as background using the euclidean metric
void DataLogic::SelectImage(robot_data * robot_status, image_data * bg_image_data,
			    DistanceCalcInterface * calculator)
{

  // call the "calculator" instance to choose the right image

  // since our data are already stored with vector,
  // we simply pass its reference
  calculator->ChooseImage(robot_status, bg_image_data, &_images_collection);


}
