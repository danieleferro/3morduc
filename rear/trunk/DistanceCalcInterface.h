#ifndef __DISTANCE_CALC_INTERFACE
#define __DISTANCE_CALC_INTERFACE

#include "data_types.h"

class DistanceCalcInterface
{
 public:
  virtual float Calculate(robot_data *, image_data *) = 0;
};

#endif
