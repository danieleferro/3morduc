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
#include "IDataLogic.h"
#include "IImageSelector.h"

#define TO_DEGREES(X) X * 180 / M_PI

class DataLogic : public IDataLogic
{
 private:
  std::vector<image_data> _images_collection;
  int _index;
  int _simulation_session;
  
 public:
  DataLogic(int);
  ~DataLogic();
  void SelectImage(robot_data *, image_data *,
		   IImageSelector *);

  void RetrieveData(robot_data *);

  void Command(int);


};

#endif
