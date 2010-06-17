#ifndef __DISTANCE_CALC
#define __DISTANCE_CALC

#include "DistanceCalcInterface.h"
#include <iostream>
#include <math.h>

#define TO_RADIANS(X) X * M_PI / 180
#define TO_DEGREES(X) X * 180 / M_PI

#define DEBUG 1

/*
  Basic spacial metric - implements selection method 2 of
  [sugimoto]
*/
class SpacialMetricCalc : public DistanceCalcInterface
{
 public:
  float Calculate(robot_data *, image_data *);
};


/*
  Sweep metric - implements method described in report
*/
class SweepMetricCalc : public DistanceCalcInterface
{
 public:
  float Calculate(robot_data *, image_data *);
  SweepMetricCalc(float, float, float, float, float, float);
  
 private:
  static const int X0 = 0;
  static const int Y0 = 1;
  static const int M0 = 2;
  
  float _sweep_angle;
  float _angle_offset;
  
  /* gaussian parameters for assigning distance score */
  float _mu_distance;
  float _sigma_distance;

  /* gaussian parameters for assigning angle score */
  float _mu_angle;
  float _sigma_angle;


  /*
    Primitives to be called by Calculate
   */
  float PointAlgorithm( robot_data * robot_status, 
			image_data * bg_image_data);

  bool WithinBoundaries( robot_data * robot_status, 
			 image_data * bg_image_data);
  
  bool RightlyOriented( robot_data * robot_status, 
			image_data * bg_image_data);

  float Normalize180(float angle);
};


#endif
