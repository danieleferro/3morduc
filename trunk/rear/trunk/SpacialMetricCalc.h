#ifndef __SPACIAL_METRIC
#define __SPACIAL_METRIC

#include "DistanceCalcInterface.h"
#include <iostream>
#include <math.h>
#include <stdlib.h>
#include <cstring>

#define TO_RADIANS(X) X * M_PI / 180
#define TO_DEGREES(X) X * 180 / M_PI

/*
  Basic spacial metric - implements selection method 2 of
  [sugimoto]
*/
class SpacialMetricCalc : public DistanceCalcInterface
{
 private:
  float Calculate(robot_data *, image_data *);
    
 public:
  void ChooseImage(robot_data *, image_data *, std::vector<image_data> *);

};


#endif
