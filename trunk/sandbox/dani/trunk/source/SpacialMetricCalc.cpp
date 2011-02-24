#include "SpacialMetricCalc.h"

SpacialMetricCalc::SpacialMetricCalc(float opt_dist) {

  _optimal_distance = opt_dist;

}

void SpacialMetricCalc::ChooseImage( robot_data * robot_status, image_data * bg_image_data,
				     std::vector<image_data> * _images_collection) {

  float distances[_images_collection->size()];
  float min;

  // calculate the distance for each stored image
  int i = 0;
  for (std::vector<image_data>::iterator it =
	 _images_collection->begin();
       it != _images_collection->end();
       it++)
    {
      distances[i] = Calculate(robot_status, &*it);
      //      std::cout << "Return value from Calculate Function: " << distances[i] << std::endl;
      i++;
    }

  for (int i = 0; i < _images_collection->size(); i ++)
    std::cout << "distance[" << i << "]: "
	      << distances[i]
	      << std::endl;

  // find the minimum distance
  i = 0;
  min = distances[0];
  for (int j = 1; j < _images_collection->size(); j++)
    {
      if (distances[j] < min)
	{
	  i = j;
	  min = distances[j];
	}
    }

  // return the selected image data
  bg_image_data->x = (*_images_collection)[i].x;
  bg_image_data->y = (*_images_collection)[i].y;
  bg_image_data->theta = (*_images_collection)[i].theta;
  bg_image_data->time = (*_images_collection)[i].time;

  strcpy(bg_image_data->path, (*_images_collection)[i].path);

  // std::cout << "----> " << bg_image_data->path << "<-----" << std::endl;

}


float SpacialMetricCalc::Calculate(robot_data * robot_status, image_data * bg_image_data) {

  float distance;
  float score;

  distance = 
    sqrt( pow((robot_status -> x ) - (bg_image_data -> x), 2) +
	  pow((robot_status -> y ) - (bg_image_data -> y), 2) +
	  pow((robot_status -> theta ) - (bg_image_data -> theta), 2));


  if ( distance <= _optimal_distance )
    score = distance / _optimal_distance;
 else
    score = - ( distance - 2 * _optimal_distance) / _optimal_distance;

  return (- score);


}
