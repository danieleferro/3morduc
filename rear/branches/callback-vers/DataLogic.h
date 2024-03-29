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

struct image_data {
  float x;
  float y;
  float theta;
  float time;
  char path[100];
};

struct robot_data {
  float x;
  float y;
  float theta;
  float time;
};

class DataLogic
{
 private:
  std::vector<image_data> _images_collection;
  int _index;
  int _simulation_session;
  
 public:
  DataLogic(int);
  ~DataLogic();
  void SelectImage(robot_data *, image_data *, 
		   float (* calculator)(robot_data *, image_data *));
  void RetrieveData(robot_data *);
};

#endif
