#include "DataLogic.h"

DataLogic::DataLogic(int session)
{
  _simulation_session = session;
  _index = 0;
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
  
  o << "../log/log_" << _simulation_session 
    << "/data_" << _simulation_session << ".txt";
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

  data->x = atof ( tokens[1].c_str() );
  data->y = - atof ( tokens[0].c_str() );
  data->theta = atof ( tokens[2].c_str() );
  data->time = atof ( tokens[3].c_str() );

  time = (int) data->time;
  
  /* clear the stream and 
     add a new value to it */
  o.str("");
  o.clear();
  o << "../log/log_" << _simulation_session << "/screenshot_" 
    << _simulation_session << "_" << time << ".png";

  // fill grabbed frame metadata
  grabbed_frame_data.x = data->x;
  grabbed_frame_data.y = data->y;
  grabbed_frame_data.theta = data->theta;
  grabbed_frame_data.time = data->time;

  strcpy(grabbed_frame_data.path, o.str().c_str());

  //search for possible duplicates
  bool duplicate = false;
  for (std::vector<image_data>::iterator it =
	 _images_collection.begin();
       it != _images_collection.end();
       it++)
    {
      if ( strcmp(it->path, grabbed_frame_data.path) == 0)
	{
	  duplicate = true;
	  break;
	}
    }

  if ( ! duplicate )
    {
      // store the collected metadata
      _images_collection.push_back(grabbed_frame_data);
      
      // increase index to point the next line of the file
    }

  _index++;

  return;
}

// select the image to set as background using the euclidean metric
void DataLogic::SelectImage(robot_data * robot_status, 
			    image_data * bg_image_data)
{
  float distances[_images_collection.size()];
  float min;

  std::cout << "Number of stored images: " 
	    << _images_collection.size() << std::endl;

  // calculate the euclidean metric for each stored image
  int i = 0;
  for (std::vector<image_data>::iterator it =
	 _images_collection.begin();
       it != _images_collection.end();
       it++)
    {
      distances[i] =
      	sqrt( pow((robot_status -> x) - (it -> x), 2) +
      	      pow((robot_status -> y) - (it -> y), 2) +
	      // angles need a 100 factor
      	      100 * pow((robot_status -> theta) - (it -> theta), 2));


      std::cout << "distances[" << i << "].x = " 
		<< robot_status->x - it-> x << std::endl;

      std::cout << "distances[" << i << "].y = " 
		<< robot_status->y - it-> y << std::endl;

      std::cout << "distances[" << i << "].theta = " 
		<< robot_status->theta - it-> theta << std::endl;

      std::cout << "distances[" << i << "].overall = " 
		<< distances[i] << std::endl;
      
      std::cout << std::endl;
      
      //       distances[i] =
      // 	sqrt( pow((robot_status -> x) - (it -> x), 2) +
      // 	      pow((robot_status -> y) - (it -> y), 2));
      
      //       std::cout << "distance is " <<
      // 	distances[i] << std::endl;

      i++;
    }

  // find the minimum distance
  i = 0;
  min = 10000;
  for (int j = 0; j < _images_collection.size(); j++)
    {
      if (distances[j] < min && 
	  distances[j] != 0.f &&
	  distances[j] > 8.f)
	{
	  i = j;
	  min = distances[j];
	}
    }

  std::cout << "The selected background images has distance " 
	    << min << std::endl;

  // return the selected image data
  bg_image_data->x = _images_collection[i].x;
  bg_image_data->y = _images_collection[i].y;
  bg_image_data->theta = _images_collection[i].theta;
  bg_image_data->time = _images_collection[i].time;

  strcpy(bg_image_data->path, _images_collection[i].path);

}
