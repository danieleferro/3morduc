#ifndef __DISTANCE_CALC_INTERFACE
#define __DISTANCE_CALC_INTERFACE

#include "data_types.h"
#include <vector>

class DistanceCalcInterface
{
 public:
  virtual void ChooseImage(robot_data *, image_data *, std::vector<image_data> *) = 0;
};

#endif
