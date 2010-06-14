#ifndef __DATA_LOGIC_INTERFACE
#define __DATA_LOGIC_INTERFACE

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

class DataLogicInterface {

 public:

  virtual void SelectImage(robot_data *, image_data *) = 0;
  virtual void RetrieveData(robot_data *) = 0;
  
};

#endif
