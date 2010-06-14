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
		   float (*)(robot_data *, image_data *));
  void RetrieveData(robot_data *);
};

#endif
