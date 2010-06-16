#ifndef __DISTANCE_CALC_INTERFACE
#define __DISTANCE_CALC_INTERFACE

#include "DataLogicInterface.h"

#define IMAGE_NOT_VALID -1

class DistanceCalcInterface
{
 public:
  virtual float Calculate(robot_data *, image_data *) = 0;
};

#endif
