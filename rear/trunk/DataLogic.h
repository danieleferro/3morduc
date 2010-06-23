#ifndef __DATA_LOGIC
#define __DATA_LOGIC

#include <string>
#include <vector>
#include <stdlib.h>
#include <sstream>
#include <math.h>
#include <iostream>
#include <cstdio>
#include <cstring>
#include "DataLogicInterface.h"
#include "DistanceCalcInterface.h"

#define TO_DEGREES(X) X * 180 / M_PI

class DataLogic : public DataLogicInterface
{
 private:
  std::vector<image_data> _images_collection;
  int _index;
  int _simulation_session;
  
 public:
  DataLogic(int);
  ~DataLogic();
  void SelectImage(robot_data *, image_data *,
		   DistanceCalcInterface *);

  void RetrieveData(robot_data *);
};

#endif
