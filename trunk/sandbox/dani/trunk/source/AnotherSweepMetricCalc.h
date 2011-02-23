#ifndef __ANOTHER_SWEEP_METRIC
#define __ANOTHER_SWEEP_METRIC

#include "IImageSelector.h"
#include <iostream>
#include <fstream>
#include <math.h>
#include <stdlib.h>
#include <cstring>
#include "data_types.h"

#define TO_RADIANS(X) X * M_PI / 180
#define TO_DEGREES(X) X * 180 / M_PI

#define _ASM_DEBUG 0

#define EGO_IMAGE       1
#define IMAGE_NOT_VALID 2


/*
  Sweep metric - implements method described in report
*/
class AnotherSweepMetricCalc : public IImageSelector
{
 public:
  void ChooseImage(robot_data *, image_data *, std::vector<image_data> *);
  AnotherSweepMetricCalc(float, float, float, float, float, float);
  
 private:

  float _sweep_angle;
  float _angle_offset;
  
  /* gaussian parameters for assigning distance score */
  float _mu_distance;
  float _sigma_distance;

  /* gaussian parameters for assigning angle score */
  float _mu_angle;
  float _sigma_angle;

  /* radius for AOB triangle (sweep area approximation) */
  float _radius;
  
  /*  A, O, B triangle points */
  fPoint * _A;
  fPoint * _O;
  fPoint * _B;

  float Calculate(robot_data *, image_data *);

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

  float Sign(fPoint * p1, fPoint * p2, fPoint * p3);
  
  /* check if a point pt is included in v1v2v3 triangle */
  bool IsPointInTri(fPoint * pt, fPoint * v1, fPoint * v2, fPoint * v3);

  /* return A and B point for AOB triangle (sweep area approximation) */
  void FindTriangleVerteces(float sweep_angle, float radius);

};


#endif
